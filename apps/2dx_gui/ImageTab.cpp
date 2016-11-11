#include <QMessageBox>

#include "ApplicationData.h"
#include "ProjectData.h"
#include "ImageTab.h"
#include "ExecutionWindow.h"

ImageTab::ImageTab(QWidget* parent) 
: QWidget(parent){
    
    windowTabWidget = new QTabWidget(this);
    windowTabWidget->setDocumentMode(true);
    windowTabWidget->setTabsClosable(true);
    windowTabWidget->setIconSize(QSize(20,20));
    windowTabWidget->setTabShape(QTabWidget::Triangular);
    connect(windowTabWidget, SIGNAL(tabCloseRequested(int)), this, SLOT(closeImageWindow(int)));
    
    automatorWindow_ = new ProcessingManager(this);
    
    windowTabWidget->addTab(automatorWindow_, ApplicationData::icon("parallel_processing"), "Parallel Processing");
    tabIdToWorkingDir_.insert(0, 0);

    //No closable buttons on the library and merge tabs
    windowTabWidget->tabBar()->setTabButton(0, QTabBar::RightSide, 0);
    windowTabWidget->tabBar()->setTabButton(0, QTabBar::LeftSide, 0);
    
    //Tab Colors
    windowTabWidget->tabBar()->setTabTextColor(0, Qt::darkMagenta);
    
    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(windowTabWidget, 0, 0);
    
    setLayout(mainLayout);
}

void ImageTab::closeImageWindow(int index) {
    if(index == 0) return;
    
    QWidget* currWidget = windowTabWidget->widget(index);;
    
    ExecutionWindow* win = imagesInitializedToTabs_[tabIdToWorkingDir_[index]];
    
    if(win->isRunningScript()) {
        QMessageBox::information(this, "Error in closing image", "Image cannot be closed as it is being processed.");
        return;
    }
    
    if(win->getConf()->isModified()) {
        win->getConf()->save();
    }
    
    windowTabWidget->removeTab(index);
    delete currWidget;
    imagesInitializedToTabs_.remove(tabIdToWorkingDir_[index]);
    
    tabIdToWorkingDir_.removeAt(index);
    QList<ProjectImage*> imagesShown = tabIdToWorkingDir_;
    imagesShown.removeFirst();
    projectData.setImagesOpen(imagesShown);
}

void ImageTab::showImageWindow(ProjectImage* image) {

    if (!imagesInitializedToTabs_.keys().contains(image)) {
        qDebug() << "Initializing: " << image->toString();

        ExecutionWindow* imageWin = new ExecutionWindow(QDir(ApplicationData::scriptsDir().canonicalPath() + "/image/"), image, this);
        connect(imageWin, &ExecutionWindow::executing, [=] (bool run){
            if(run) setTabProcessing(image);
            else setTabNormal(image);
        });
   
        imagesInitializedToTabs_.insert(image, imageWin);
    }

    //Check if the tab is already visible
    if (!tabIdToWorkingDir_.contains(image)) {
        int currTabIndex = windowTabWidget->count();
        windowTabWidget->addTab(imagesInitializedToTabs_[image], image->toString());
        tabIdToWorkingDir_.insert(currTabIndex, image);
        QList<ProjectImage*> imagesShown = tabIdToWorkingDir_;
        imagesShown.removeFirst();
        projectData.setImagesOpen(imagesShown);
    }

    windowTabWidget->setCurrentWidget(imagesInitializedToTabs_[image]);
}

void ImageTab::saveConfigs() {
    for(int i=0; i<imagesInitializedToTabs_.keys().size(); ++i) {
        imagesInitializedToTabs_[imagesInitializedToTabs_.keys()[i]]->save();
    }
}

bool ImageTab::configModified() {
    for(int i=0; i<imagesInitializedToTabs_.keys().size(); ++i) {
        if(imagesInitializedToTabs_[imagesInitializedToTabs_.keys()[i]]->modified()) return true;
    }
    return false;
}


void ImageTab::setTabNormal(ProjectImage* image) {
    if(tabIdToWorkingDir_.contains(image)) {
        windowTabWidget->setTabIcon(tabIdToWorkingDir_.indexOf(image), ApplicationData::icon("file"));
        windowTabWidget->tabBar()->setTabTextColor(tabIdToWorkingDir_.indexOf(image), Qt::black);
    }
}

void ImageTab::setTabProcessing(ProjectImage* image) {
    if(tabIdToWorkingDir_.contains(image)) {
        windowTabWidget->setTabIcon(tabIdToWorkingDir_.indexOf(image), ApplicationData::icon("processing"));
        windowTabWidget->tabBar()->setTabTextColor(tabIdToWorkingDir_.indexOf(image), Qt::red);
    }
}

void ImageTab::focusOnProcessingWindow() {
    windowTabWidget->setCurrentIndex(0);
}
