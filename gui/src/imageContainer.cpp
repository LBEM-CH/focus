#include <iostream>

#include "imageContainer.h"

imageContainer::imageContainer(confData* data, QWidget* parent) 
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
    windowTabWidget->setTabsClosable(true);
    windowTabWidget->hide();
    connect(windowTabWidget, SIGNAL(tabCloseRequested(int)), this, SLOT(closeImageWindow(int)));
    
    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->addWidget(noImageLabel, 0, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    mainLayout->addWidget(windowTabWidget, 1, 0);
    
    setLayout(mainLayout);
}


void imageContainer::closeImageWindow(int index) {
    windowTabWidget->removeTab(index);
    imagesShown_.removeAt(index);
    if(imagesShown_.isEmpty()) {
        windowTabWidget->hide();
        noImageLabel->show();
    }
}

void imageContainer::showImageWindow(const QString& workingDir) {

    if (!imagesInitializedToTabs_.contains(workingDir)) {
        confData* imageData = new confData(workingDir + "/" + "2dx_image.cfg", mainData);
        QString userConfigPath = QDir::homePath() + "/.2dx/2dx_master.cfg";
        if (QFileInfo(userConfigPath).exists()) imageData->updateConf(userConfigPath);

        imageData->setUserConf(mainData->userConf());
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
        imageWindow* imageWin = new imageWindow(imageData);
        imagesInitializedToTabs_.insert(workingDir, imageWin);
    }

    //Check if the tab is already visible
    if (!imagesShown_.contains(workingDir)) {
        int currTabIndex = windowTabWidget->count();
        QString tabName = workingDir;
        tabName = tabName.remove(mainData->getDir("project"));
        windowTabWidget->addTab(imagesInitializedToTabs_[workingDir], *(mainData->getIcon("image")), tabName);
        imagesShown_.insert(currTabIndex, workingDir);
    }

    noImageLabel->hide();
    windowTabWidget->show();
    windowTabWidget->setCurrentWidget(imagesInitializedToTabs_[workingDir]);
}