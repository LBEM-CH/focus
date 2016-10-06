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
    
    automatorWindow_ = new ParallelProcessingWindow(this);
    
    windowTabWidget->addTab(automatorWindow_, ApplicationData::icon("parallel_processing"), "Parallel Processing");
    tabIdToWorkingDir_.insert(0, QString());

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
    QStringList imagesShown = tabIdToWorkingDir_;
    imagesShown.removeFirst();
    projectData.setImagesOpen(imagesShown);
}

void ImageTab::showImageWindow(const QString& workingDir) {

    if (!imagesInitializedToTabs_.keys().contains(workingDir)) {
        std::cout << "Initializing: " << workingDir.toStdString() << "\n";

        QDir procdirectory(workingDir + "/proc");
        if (!procdirectory.exists()) procdirectory.mkdir(workingDir + "/proc");
        QDir logdirectory(workingDir + "/LOGS");
        if (!logdirectory.exists()) logdirectory.mkdir(workingDir + "/LOGS");
        
        ExecutionWindow* imageWin = new ExecutionWindow(QDir(workingDir), QDir(ApplicationData::scriptsDir().canonicalPath() + "/image/"), this);
        connect(imageWin, &ExecutionWindow::executing,
                [=] (bool run){
                    if(run) setTabProcessing(workingDir);
                    else setTabNormal(workingDir);
                });
   
        imagesInitializedToTabs_.insert(workingDir, imageWin);
    }

    //Check if the tab is already visible
    if (!tabIdToWorkingDir_.contains(workingDir)) {
        int currTabIndex = windowTabWidget->count();
        windowTabWidget->addTab(imagesInitializedToTabs_[workingDir], projectData.projectDir().relativeFilePath(workingDir));
        tabIdToWorkingDir_.insert(currTabIndex, workingDir);
        QStringList imagesShown = tabIdToWorkingDir_;
        imagesShown.removeFirst();
        projectData.setImagesOpen(imagesShown);
    }

    windowTabWidget->setCurrentWidget(imagesInitializedToTabs_[workingDir]);
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


void ImageTab::setTabNormal(const QString& workingDir) {
    if(tabIdToWorkingDir_.contains(workingDir)) {
        windowTabWidget->setTabIcon(tabIdToWorkingDir_.indexOf(workingDir), ApplicationData::icon("file"));
        windowTabWidget->tabBar()->setTabTextColor(tabIdToWorkingDir_.indexOf(workingDir), Qt::black);
    }
}

void ImageTab::setTabProcessing(const QString& workingDir) {
    if(tabIdToWorkingDir_.contains(workingDir)) {
        windowTabWidget->setTabIcon(tabIdToWorkingDir_.indexOf(workingDir), ApplicationData::icon("processing"));
        windowTabWidget->tabBar()->setTabTextColor(tabIdToWorkingDir_.indexOf(workingDir), Qt::red);
    }
}