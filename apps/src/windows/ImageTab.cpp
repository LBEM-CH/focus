#include <QMessageBox>

#include "ApplicationData.h"
#include "ProjectData.h"
#include "ImageTab.h"
#include "ExecutionWindow.h"

ImageTab::ImageTab(QWidget* parent) 
: QWidget(parent){
    noImageLabel = new QLabel("No Images Activated\nPlease double click image from Library to activate and process!");
    noImageLabel->setWordWrap(true);
    noImageLabel->setAlignment(Qt::AlignCenter);
    QFont font = noImageLabel->font();
    font.setBold(true);
    font.setPointSize(24);
    noImageLabel->setFont(font);
    
    QPalette pal = noImageLabel->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    noImageLabel->setPalette(pal);
    
    windowTabWidget = new QTabWidget(this);
    windowTabWidget->setDocumentMode(true);
    windowTabWidget->setTabsClosable(true);
    windowTabWidget->setIconSize(QSize(20,20));
    windowTabWidget->setTabShape(QTabWidget::Triangular);
    windowTabWidget->hide();
    connect(windowTabWidget, SIGNAL(tabCloseRequested(int)), this, SLOT(closeImageWindow(int)));
    
    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(noImageLabel, 0, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    mainLayout->addWidget(windowTabWidget, 1, 0);
    
    setLayout(mainLayout);
}


void ImageTab::closeImageWindow(int index) { 
    QWidget* currWidget = windowTabWidget->widget(index);;
    
    
    ExecutionWindow* win = imagesInitializedToTabs_[imagesShown_[index]];
    
    if(win->isRunningScript()) {
        QMessageBox::information(this, "Error in closing image", "Image cannot be closed as it is being processed.");
        return;
    }
    
    if(win->getConf()->isModified()) {
        win->getConf()->save();
    }
    
    windowTabWidget->removeTab(index);
    delete currWidget;
    imagesInitializedToTabs_.remove(imagesShown_[index]);
    
    imagesShown_.removeAt(index);
    emit imagesOpenChanged(imagesShown_);
    if(imagesShown_.isEmpty()) {
        windowTabWidget->hide();
        noImageLabel->show();
    } 
}

void ImageTab::showImageWindow(const QString& workingDir) {

    if (!imagesInitializedToTabs_.keys().contains(workingDir)) {
        std::cout << "Initializing: " << workingDir.toStdString() << "\n";
        ParametersConfiguration* imageData = projectData.parameterData(QDir(workingDir));

        QDir procdirectory(workingDir + "/proc");
        if (!procdirectory.exists()) procdirectory.mkdir(workingDir + "/proc");
        QDir logdirectory(workingDir + "/LOGS");
        if (!logdirectory.exists()) logdirectory.mkdir(workingDir + "/LOGS");
        
        ResultsData* results = new ResultsData(ProjectData::logsDir(QDir(workingDir)).canonicalPath() + "/2dx_initialization.results", workingDir, this);
        QStringList scriptsDirs;
        scriptsDirs << ApplicationData::scriptsDir().canonicalPath() + "/image/standard" << ApplicationData::scriptsDir().canonicalPath() + "/image/custom";
        ExecutionWindow* imageWin = new ExecutionWindow(QDir(workingDir), results, scriptsDirs, ExecutionWindow::Type::IMAGE, this);
        connect(imageWin, &ExecutionWindow::executing,
                [=] (bool run){
                    if(run) setTabProcessing(workingDir);
                    else setTabNormal(workingDir);
                });
   
        imagesInitializedToTabs_.insert(workingDir, imageWin);
    }

    //Check if the tab is already visible
    if (!imagesShown_.contains(workingDir)) {
        int currTabIndex = windowTabWidget->count();
        windowTabWidget->addTab(imagesInitializedToTabs_[workingDir], projectData.projectDir().relativeFilePath(workingDir));
        imagesShown_.insert(currTabIndex, workingDir);
        emit imagesOpenChanged(imagesShown_);
    }

    noImageLabel->hide();
    windowTabWidget->show();
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
    if(imagesShown_.contains(workingDir)) {
        windowTabWidget->setTabIcon(imagesShown_.indexOf(workingDir), ApplicationData::icon("file"));
        windowTabWidget->tabBar()->setTabTextColor(imagesShown_.indexOf(workingDir), Qt::black);
    }
}

void ImageTab::setTabProcessing(const QString& workingDir) {
    if(imagesShown_.contains(workingDir)) {
        windowTabWidget->setTabIcon(imagesShown_.indexOf(workingDir), ApplicationData::icon("processing"));
        windowTabWidget->tabBar()->setTabTextColor(imagesShown_.indexOf(workingDir), Qt::red);
    }
}

QStringList ImageTab::getImagesOpen() {
    return imagesShown_;
}