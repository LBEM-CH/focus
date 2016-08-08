#include <iostream>

#include "image_tab.h"

ImageTab::ImageTab(confData* data, QWidget* parent) 
: QWidget(parent){
    mainData = data;
    
    noImageLabel = new QLabel("No Images Activated\nPlease activate image from Library to process!");
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
    windowTabWidget->removeTab(index);
    imagesShown_.removeAt(index);
    emit imagesOpenChanged(imagesShown_);
    if(imagesShown_.isEmpty()) {
        windowTabWidget->hide();
        noImageLabel->show();
    }
}

void ImageTab::showImageWindow(const QString& workingDir) {

    if (!imagesInitializedToTabs_.keys().contains(workingDir)) {
        confData* imageData = new confData(workingDir + "/" + "2dx_image.cfg", mainData);
        QString userConfigPath = QDir::homePath() + "/.2dx/2dx_master.cfg";
        if (QFileInfo(userConfigPath).exists()) imageData->updateConf(userConfigPath);

        imageData->setDir("application", mainData->getDir("application"));
        imageData->setDir("plugins", mainData->getDir("pluginsDir"));
        imageData->setDir("tools", mainData->getDir("pluginsDir") + "/tools/");
        imageData->setDir("standardScripts", QDir(mainData->getDir("application") + "../kernel/2dx_image" + "/" + "scripts-standard/"));
        imageData->setDir("customScripts", QDir(mainData->getDir("application") + "../kernel/2dx_image" + "/" + "scripts-custom/"));
        imageData->setDir("config", mainData->getDir("config"));
        imageData->setDir("project", QDir(workingDir + "/../"));
        imageData->setDir("icons", imageData->getDir("config") + "/resource");
        imageData->setDir("working", workingDir);

        QDir procdirectory(workingDir + "/proc");
        if (!procdirectory.exists()) procdirectory.mkdir(workingDir + "/proc");
        imageData->setDir("remoteProc", workingDir + "/proc/");
        QDir logdirectory(workingDir + "/LOGS");
        if (!logdirectory.exists()) logdirectory.mkdir(workingDir + "/LOGS");
        imageData->setDir("logs", workingDir + "/LOGS");

        imageData->setDir("binDir", imageData->getDir("application") + "/../kernel/mrc/bin");
        imageData->setDir("procDir", imageData->getDir("application") + "/../kernel/proc/");

        confData *cfg = new confData(QDir::homePath() + "/.2dx/" + "2dx.cfg", imageData->getDir("config") + "/" + "2dx.cfg");
        if (cfg->isEmpty()) {
            std::cerr << "2dx.cfg not found." << std::endl;
            exit(0);
        }
        cfg->save();

        imageData->setAppConf(cfg);
        imageData->addApp("this", imageData->getDir("application") + "/../" + "bin/" + "2dx_image");
        imageData->addApp("2dx_image", imageData->getDir("application") + "/../" + "bin/" + "2dx_image");
        imageData->addApp("2dx_merge", imageData->getDir("application") + "/../" + "bin/" + "2dx_merge");
        imageData->addApp("logBrowser", imageData->getDir("application") + "/../" + "bin/" + "2dx_logbrowser");

        imageData->setURL("help", "http://2dx.org/documentation/2dx-software");
        imageData->setURL("bugReport", "https://github.com/C-CINA/2dx/issues");

        imageData->addImage("appImage", new QImage(imageData->getDir("application") + "/resource/" + "icon.png"));

        imageData->syncWithUpper();
        ImageWindow* imageWin = new ImageWindow(imageData);
        connect(imageWin, &ImageWindow::executing,
                [=] (bool run){
                    if(run) setTabProcessing(workingDir);
                    else setTabNormal(workingDir);
                });
                
        imagesInitializedToTabs_.insert(workingDir, imageWin);
    }

    //Check if the tab is already visible
    if (!imagesShown_.contains(workingDir)) {
        int currTabIndex = windowTabWidget->count();
        QString tabName = workingDir;
        tabName = tabName.remove(mainData->getDir("project"));
        windowTabWidget->addTab(imagesInitializedToTabs_[workingDir], tabName);
        imagesShown_.insert(currTabIndex, workingDir);
        emit imagesOpenChanged(imagesShown_);
    }

    noImageLabel->hide();
    windowTabWidget->show();
    windowTabWidget->setCurrentWidget(imagesInitializedToTabs_[workingDir]);
}

void ImageTab::setTabNormal(const QString& workingDir) {
    if(imagesShown_.contains(workingDir)) {
        windowTabWidget->setTabIcon(imagesShown_.indexOf(workingDir), *(mainData->getIcon("file")));
        windowTabWidget->tabBar()->setTabTextColor(imagesShown_.indexOf(workingDir), Qt::black);
    }
}

void ImageTab::setTabProcessing(const QString& workingDir) {
    if(imagesShown_.contains(workingDir)) {
        windowTabWidget->setTabIcon(imagesShown_.indexOf(workingDir), *(mainData->getIcon("processing")));
        windowTabWidget->tabBar()->setTabTextColor(imagesShown_.indexOf(workingDir), Qt::red);
    }
}

QStringList ImageTab::getImagesOpen() {
    return imagesShown_;
}