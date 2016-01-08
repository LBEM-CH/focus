/**************************************************************************
 *   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
 *   HStahlberg@ucdavis.edu                                                *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <QDebug>
#include <QDesktopServices>
#include <QModelIndexList>
#include <QItemSelectionModel>
#include <QTabWidget>
#include <QToolBar>
#include <iostream>

#include "mainWindow.h"
#include "blockContainer.h"

using namespace std;

mainWindow::mainWindow(const QString &directory, QWidget *parent)
: QMainWindow(parent) {

    m_do_autosave = true;
    mainData = setupMainConfiguration(directory);
    userData = new confData(QDir::homePath() + "/.2dx/" + "2dx_merge-user.cfg", mainData->getDir("config") + "/" + "2dx_merge-user.cfg");
    userData->save();
    mainData->setUserConf(userData);
    
    installedVersion = mainData->version();
    setWindowTitle("2dx (" + installedVersion + ")");
    setUnifiedTitleAndToolBarOnMac(true);

    connect(&importProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(importFinished()));
    
    updates = new updateWindow(mainData, this);
    updates->hide();

    results = new resultsData(mainData, mainData->getDir("working") + "/LOGS/" + "2dx_initialization.results", mainData->getDir("working"), this);
    albumCont = new albumContainer(mainData, results, this);
    executionCont = new executionContainer(mainData, results, this);

    connect(executionCont, SIGNAL(scriptCompletedSignal()), albumCont, SLOT(maskResults()));
    connect(executionCont, SIGNAL(scriptCompletedSignal()), this, SLOT(stopPlay()));

    QSplitter* container = new QSplitter(Qt::Vertical, this);
    
    container->addWidget(albumCont);
    container->addWidget(executionCont);
    container->setStretchFactor(0, 1);
    container->setStretchFactor(1, 2);

    QWidget *centralWidget = new QWidget(this);
    QGridLayout* layout = new QGridLayout(centralWidget);
    layout->setMargin(0);
    layout->setSpacing(0);
    centralWidget->setLayout(layout);
    layout->addWidget(container);
    
    QTabWidget* centralTabs = new QTabWidget(this);
    centralTabs->addTab(centralWidget, "MERGE TOOL");
    
    setCentralWidget(centralTabs);

    about = new aboutWindow(mainData, this, true);
    about->hide();

    setupActions();
    setupToolBar();
    setupMenuBar();
    
    int fontSize = mainData->userConf()->get("fontSize", "value").toInt();
    if (fontSize != 0) {
        QFont mainFont(QApplication::font());
        mainFont.setPointSize(fontSize);
        QApplication::setFont(mainFont);
    } else {
        QFont font = QApplication::font();
        mainData->userConf()->set("fontSize", QString::number(font.pointSize()));
        mainData->userConf()->save();
        QApplication::setFont(font);
    }
    updateFontInfo();
    
    album = NULL;
    euler = NULL;
    reproject = NULL;

    importCount = 0;
    
    resize(1040, 720);
    
    
    //Just to have the correct sizes on startup
    container->setSizes(QList<int>() << 1 << 2);

}

confData* mainWindow::setupMainConfiguration(const QString &directory) 
{
    confData* mainData;
    
    QDir applicationDir, configDir;

#ifdef Q_OS_MAC
    applicationDir = QDir(QApplication::applicationDirPath() + "/../../../");
#else
    applicationDir = QDir(QApplication::applicationDirPath());
#endif

    configDir = QDir(applicationDir.canonicalPath() + "/../" + "config/");

    QString mergeConfigLocation = directory + "/merge/" + "2dx_merge.cfg";
    QString appConfigLocation = configDir.canonicalPath() + "/" + "2dx_master.cfg";
    if (QFileInfo(mergeConfigLocation).exists()) {
        mainData = new confData(mergeConfigLocation, appConfigLocation);
        if (QFileInfo(appConfigLocation).exists()) {
            mainData->updateConf(appConfigLocation);
        }
    } else {
        mainData = new confData(mergeConfigLocation, appConfigLocation);
    }
    mainData->setDir("project", QDir(directory));
    mainData->setDir("working", QDir(directory + "/merge"));

    if (!QFileInfo(mainData->getDir("working") + "/" + "2dx_merge.cfg").exists()) mainData->save();
    
    mainData->setDir("application", applicationDir);

    mainData->setDir("binDir", mainData->getDir("application") + "/bin/");
    mainData->setDir("procDir", mainData->getDir("application") + "../proc/");
    createDir(mainData->getDir("working") + "/config");

    mainData->setDir("config", configDir);

    createDir(QDir::homePath() + "/.2dx/");
    QString userPath = QDir::homePath() + "/.2dx";
    createDir(userPath + "/2dx_merge");

    confData *cfg = new confData(userPath + "/2dx.cfg", mainData->getDir("config") + "/" + "2dx.cfg");
    if (cfg->isEmpty()) {
        cerr << "2dx.cfg not found." << endl;
        exit(0);
    }
    cfg->save();

    mainData->setAppConf(cfg);

    mainData->setDir("home_2dx", userPath);
    mainData->setDir("pluginsDir", mainData->getDir("application") + "/.." + "/plugins");
    mainData->setDir("translatorsDir", mainData->getDir("pluginsDir") + "/translators");
    mainData->setDir("resource", QDir(mainData->getDir("config") + "/resource/"));
    mainData->setDir("2dx_bin", mainData->getDir("application") + "/.." + "/bin");
    mainData->addApp("this", mainData->getDir("application") + "/../" + "bin/" + "2dx_merge");
    mainData->addApp("2dx_image", mainData->getDir("2dx_bin") + "/" + "2dx_image");
    mainData->addApp("2dx_merge", mainData->getDir("2dx_bin") + "/" + "2dx_merge");

    createDir(mainData->getDir("working") + "/proc");
    mainData->setDir("remoteProc", mainData->getDir("working") + "/proc/");
    createDir(mainData->getDir("working") + "/LOGS");
    mainData->setDir("logs", mainData->getDir("working") + "/LOGS");
    mainData->setDir("merge2DScripts", QDir(mainData->getDir("application") + "../kernel/2dx_merge" + "/" + "scripts-merge2D/"));
    mainData->setDir("merge3DScripts", QDir(mainData->getDir("application") + "../kernel/2dx_merge" + "/" + "scripts-merge3D/"));
    mainData->setDir("customScripts", QDir(mainData->getDir("application") + "../kernel/2dx_merge" + "/" + "scripts-custom/"));
    mainData->setDir("singleParticleScripts", QDir(mainData->getDir("application") + "../kernel/2dx_merge" + "/" + "scripts-singleparticle/"));
    mainData->addImage("appImage", new QImage("resource/icon.png"));

    mainData->addApp("logBrowser", mainData->getDir("application") + "/../" + "bin/" + "2dx_logbrowser");

    mainData->setURL("help", "http://2dx.org/documentation/2dx-software");
    mainData->setURL("bugReport", "https://github.com/C-CINA/2dx/issues");

    if (!setupIcons(mainData, mainData->getDir("resource"))) cerr << "Error loading images." << mainData->getDir("resource").toStdString() << endl;
    
    connect(mainData, SIGNAL(dataModified(bool)), this, SLOT(setSaveState(bool)));
    
    return mainData;
}

void mainWindow::setupActions() {
    openAction = new QAction(*(mainData->getIcon("open")), tr("&New"), this);
    openAction->setShortcut(tr("Ctrl+O"));
    connect(openAction, SIGNAL(triggered()), this, SLOT(open()));

    saveAction = new QAction(*(mainData->getIcon("save")), tr("&Save"), this);
    saveAction->setShortcut(tr("Ctrl+S"));
    connect(saveAction, SIGNAL(triggered()), mainData, SLOT(save()));

    importAction = new QAction(*(mainData->getIcon("import")), tr("&Import Images"), this);
    connect(importAction, SIGNAL(triggered()), this, SLOT(import()));

    showImageLibraryAction = new QAction(*(mainData->getIcon("library")), tr("&Show Image Library"), this);
    showImageLibraryAction->setShortcut(tr("Ctrl+D"));
    showImageLibraryAction->setCheckable(true);
    showImageLibraryAction->setChecked(true);
    connect(showImageLibraryAction, SIGNAL(toggled(bool)), albumCont, SLOT(showContents(bool)));

    viewAlbum = new QAction(*(mainData->getIcon("album")), tr("&Reconstruction album"), this);
    viewAlbum->setShortcut(tr("Ctrl+Shift+A"));
    connect(viewAlbum, SIGNAL(triggered()), this, SLOT(showAlbum()));

    playAction = new QAction(*(mainData->getIcon("play")), tr("&Run selected script(s)"), this);
    playAction->setCheckable(true);
    connect(playAction, SIGNAL(toggled(bool)), executionCont, SLOT(execute(bool)));

    dryRun = new QAction(*(mainData->getIcon("dryRun")), tr("&Dry Run (No changes in config files)"), this);
    dryRun->setCheckable(true);
    dryRun->setChecked(false);
    connect(dryRun, SIGNAL(toggled(bool)), results, SLOT(setDryRunMode(bool)));

    timer_refresh = 10000;
    timer = new QTimer(this);
    connect(timer, SIGNAL(timeout()), mainData, SLOT(save()));
    timer->start(timer_refresh);

    refreshAction = new QAction(*(mainData->getIcon("refresh")), tr("&Refresh Results"), this);
    refreshAction->setShortcut(tr("Ctrl+Shift+r"));
    connect(refreshAction, SIGNAL(triggered()), this, SLOT(reload()));

    manualAction = new QAction(*(mainData->getIcon("help")), tr("Show Manual"), this);
    manualAction->setCheckable(true);
    connect(manualAction, SIGNAL(triggered(bool)), executionCont, SLOT(showManual(bool)));

}

void mainWindow::setupToolBar() {
    QToolBar* fileToolBar = addToolBar(tr("File"));
    fileToolBar->addAction(openAction);
    fileToolBar->addAction(importAction);
    fileToolBar->addAction(saveAction);

    QToolBar* viewToolBar = addToolBar(tr("View"));
    viewToolBar->addAction(showImageLibraryAction);
    viewToolBar->addAction(viewAlbum);

    QToolBar* actionToolBar = addToolBar(tr("Action"));
    actionToolBar->addAction(playAction);
    actionToolBar->addAction(refreshAction);
    actionToolBar->addAction(dryRun);

    QToolBar* helpToolBar = addToolBar(tr("Help"));
    helpToolBar->addAction(manualAction);
}

void mainWindow::setupMenuBar() {
    /**
     * Setup File menu
     */
    QMenu *fileMenu = new QMenu("File");
    fileMenu->addAction(openAction);
    fileMenu->addAction(saveAction);
    fileMenu->addAction(importAction);

    QAction *closeAction = new QAction("Quit", this);
    closeAction->setShortcut(tr("Ctrl+Q"));
    connect(closeAction, SIGNAL(triggered()), qApp, SLOT(closeAllWindows()));
    fileMenu->addAction(closeAction);

    /**
     * Setup Edit menu
     */
    QMenu *editMenu = new QMenu("Edit");

    QAction *increaseFontAction = new QAction("Increase Font Size", this);
    increaseFontAction->setShortcut(tr("]"));
    connect(increaseFontAction, SIGNAL(triggered()), this, SLOT(increaseFontSize()));
    editMenu->addAction(increaseFontAction);

    QAction *decreaseFontAction = new QAction("Decrease Font Size", this);
    decreaseFontAction->setShortcut(tr("["));
    connect(decreaseFontAction, SIGNAL(triggered()), this, SLOT(decreaseFontSize()));
    editMenu->addAction(decreaseFontAction);

    /**
     * setup View Menu
     */
    QMenu* viewMenu = new QMenu("View");
    viewMenu->addAction(showImageLibraryAction);
    
    QAction* showSelectedAction = new QAction(*(mainData->getIcon("selected")), tr("&Show checked images only"), this);
    showSelectedAction->setShortcut(tr("Ctrl+X"));
    showSelectedAction->setCheckable(true);
    connect(showSelectedAction, SIGNAL(toggled(bool)), albumCont, SLOT(showSelected(bool)));
    viewMenu->addAction(showSelectedAction);
    
    viewMenu->addAction(viewAlbum);

    QAction *viewReproject = new QAction("Show Reproject GUI", this);
    viewReproject->setShortcut(tr("Ctrl+Shift+P"));
    connect(viewReproject, SIGNAL(triggered()), this, SLOT(showReproject()));
    viewMenu->addAction(viewReproject);


    /**
     * Setup Options menu
     */
    QMenu *optionMenu = new QMenu("Options");

    QAction *openPreferencesAction = new QAction("Preferences", this);
    connect(openPreferencesAction, SIGNAL(triggered()), this, SLOT(editHelperConf()));
    optionMenu->addAction(openPreferencesAction);

    QAction *showAutoSaveAction = new QAction("Autosave On/Off", this);
    connect(showAutoSaveAction, SIGNAL(triggered()), this, SLOT(toggleAutoSave()));
    optionMenu->addAction(showAutoSaveAction);

    /**
     * Setup Actions menu 
     */
    QMenu *actionMenu = new QMenu("Action");
    actionMenu->addAction(playAction);
    actionMenu->addAction(refreshAction);
    actionMenu->addAction(dryRun);

    /**
     * Setup select menu
     */
    QMenu *selectMenu = new QMenu("Select");

    QAction *selectAllAction = new QAction("Check all images", this);
    selectAllAction->setShortcut(tr("Ctrl+A"));
    selectMenu->addAction(selectAllAction);
    connect(selectAllAction, SIGNAL(triggered()), albumCont->getDirModel(), SLOT(selectAll()));

    QAction *invertSelectedAction = new QAction("Invert check", this);
    invertSelectedAction->setShortcut(tr("Ctrl+I"));
    selectMenu->addAction(invertSelectedAction);
    connect(invertSelectedAction, SIGNAL(triggered()), albumCont->getDirModel(), SLOT(invertSelection()));

    QAction *saveDirectorySelectionAction = new QAction("Save checked list", this);
    connect(saveDirectorySelectionAction, SIGNAL(triggered()), this, SLOT(saveDirectorySelection()));
    selectMenu->addAction(saveDirectorySelectionAction);

    QAction *loadDirectorySelectionAction = new QAction("Load checked list", this);
    connect(loadDirectorySelectionAction, SIGNAL(triggered()), this, SLOT(loadDirectorySelection()));
    selectMenu->addAction(loadDirectorySelectionAction);

    /**
     * Setup Help menu
     */
    QMenu *helpMenu = new QMenu("Help");

    QSignalMapper *mapper = new QSignalMapper(this);

    QAction *viewOnlineHelp = new QAction(*(mainData->getIcon("manual")), tr("&View Online Help"), this);
    viewOnlineHelp->setCheckable(false);
    connect(viewOnlineHelp, SIGNAL(triggered()), mapper, SLOT(map()));
    mapper->setMapping(viewOnlineHelp, mainData->getURL("help"));
    helpMenu->addAction(viewOnlineHelp);

    QAction* bugReport = new QAction(*(mainData->getIcon("Bug")), tr("&Report Issue/Bug"), this);
    bugReport->setCheckable(false);
    connect(bugReport, SIGNAL(triggered()), mapper, SLOT(map()));
    mapper->setMapping(bugReport, mainData->getURL("bugReport"));
    helpMenu->addAction(bugReport);

    helpMenu->addAction(manualAction);

    connect(mapper, SIGNAL(mapped(const QString &)), this, SLOT(openURL(const QString &)));

    QAction *showUpdatesAction = new QAction("Update...", this);
    connect(showUpdatesAction, SIGNAL(triggered()), updates, SLOT(show()));
    helpMenu->addAction(showUpdatesAction);

    QAction *showAboutAction = new QAction("About", this);
    connect(showAboutAction, SIGNAL(triggered()), about, SLOT(show()));
    helpMenu->addAction(showAboutAction);

    menuBar()->addMenu(fileMenu);
    menuBar()->addMenu(editMenu);
    menuBar()->addMenu(viewMenu);
    menuBar()->addMenu(actionMenu);
    menuBar()->addMenu(optionMenu);
    menuBar()->addMenu(selectMenu);
    menuBar()->addMenu(helpMenu);
}

bool mainWindow::setupIcons(confData *data, const QDir &directory) {
    if (!directory.exists()) return false;
    QString entry, label, type;
    QHash<QString, QIcon *> icons;

    foreach(entry, directory.entryList(QStringList() << "*", QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted)) {
        if (entry.contains(QRegExp(".*\\-..\\.png$"))) {
            label = entry.section(".png", 0, 0).section("-", 0, 0).trimmed().toLower();
            type = entry.section(".png", 0, 0).section("-", 1, 1).trimmed().toLower();
            if (icons[label] == NULL) icons.insert(label, new QIcon);
            if (type == "ad") icons[label]->addPixmap(directory.canonicalPath() + "/" + entry, QIcon::Active, QIcon::On);
            if (type == "id") icons[label]->addPixmap(directory.canonicalPath() + "/" + entry, QIcon::Normal, QIcon::On);
            if (type == "au") icons[label]->addPixmap(directory.canonicalPath() + "/" + entry, QIcon::Active, QIcon::Off);
            if (type == "iu") icons[label]->addPixmap(directory.canonicalPath() + "/" + entry, QIcon::Normal, QIcon::Off);
        } else if (entry.contains(".png", Qt::CaseInsensitive)) {
            label = entry.section(".png", 0, 0).trimmed().toLower();
            icons.insert(label, new QIcon);
            icons[label]->addPixmap(directory.canonicalPath() + "/" + entry);
        }
    }

    QHashIterator<QString, QIcon*> it(icons);
    while (it.hasNext()) {
        it.next();
        data->addIcon(it.key(), it.value());
    }
    return true;
}


void mainWindow::setSaveState(bool state) {
    if (state == false) {
        saveAction->setChecked(false);
        saveAction->setCheckable(false);
    } else {
        saveAction->setCheckable(true);
        saveAction->setChecked(true);
    }
}

void mainWindow::stopPlay() 
{;
    playAction->setChecked(false);
}

void mainWindow::reload() {
    results->load();
    albumCont->reload();
    if (album != NULL)
    {
        album->reload();
    }
}

bool mainWindow::createDir(const QString &dir) {
    QDir directory(dir);
    if (!directory.exists())
        return directory.mkdir(dir);
    return false;
}

void mainWindow::launchAlbum(const QString &path) {

    if (album == NULL && albumCont != NULL) {
        album = new imageAlbum(albumCont->getDirModel());
        connect(albumCont->getDirView()->selectionModel(), SIGNAL(currentRowChanged(const QModelIndex&, const QModelIndex&)), album, SLOT(currentSelectionChanged(const QModelIndex&, const QModelIndex&)));
    }
}

void mainWindow::launchEuler() {

    if (euler == NULL) {
        euler = new eulerWindow(mainData);
        //    album->setModel(sortModel);
        //    album->setSelectionModel(dirView->selectionModel());
    }
}

void mainWindow::launchReproject() {
    if (reproject == NULL) {
        reproject = new reprojectWindow(mainData);
    }
}

void mainWindow::closeEvent(QCloseEvent *event) {
    if (!mainData->isModified())
        event->accept();
    else {
        int choice = QMessageBox::question(this, tr("Confirm Exit"), tr("Data not saved, exit?"), tr("Save && Quit"), tr("Quit Anyway"), QString("Cancel"), 0, 1);
        if (choice == 0) {
            mainData->save();
            event->accept();
        } else if (choice == 1)
            event->accept();
        else if (choice == 2)
            event->ignore();
    }
}

void mainWindow::importFiles(const QHash<QString, QHash<QString, QString> > &imageList) {
    QHashIterator<QString, QHash<QString, QString> > it(imageList);
    importCount = imageList.size();
    while (it.hasNext()) {
        it.next();
        qDebug() << "Importing File: " << it.key();
        importFile(it.key(), it.value());
    }
}

void mainWindow::importFile(const QString &file, const QHash<QString, QString> &imageCodes) {
    QHashIterator<QString, QString> it(imageCodes);
    QString fileName = file;
    QString pC = imageCodes["protein_code"];
    QString tiltAngle = imageCodes["tilt_angle"];
    QString frame = imageCodes["frame_number"];
    QString subID = imageCodes["sub_image_number"];
    QString ext = QFileInfo(file).suffix();

    qDebug() << "pC=" << pC << "  tiltAngle=" << tiltAngle << "  frame=" << frame << "  subID=" << subID << "  ext=" << ext;

    QDir tiltDir(mainData->getDir("project") + "/" + pC + tiltAngle);
    QString newFile = pC + tiltAngle + frame + subID;
    QString tiltDirectory = pC + tiltAngle;
    QString tiltConfigLocation = mainData->getDir("project") + "/" + tiltDirectory + "/2dx_master.cfg";
    if (!tiltDir.exists()) {
        qDebug() << pC + tiltAngle << " does not exist...creating.";
        tiltDir.setPath(mainData->getDir("project"));
        tiltDir.mkdir(tiltDirectory);
        //confData tiltData(tiltConfigLocation);
        confData tiltData(mainData->getDir("project") + "/" + tiltDirectory + "/2dx_master.cfg", mainData->getDir("project") + "/2dx_master.cfg");
        tiltData.save();
        tiltData.setSymLink("../2dx_master.cfg", mainData->getDir("project") + "/" + tiltDirectory + "/2dx_master.cfg");
    }

    tiltDir.setPath(mainData->getDir("project") + "/" + tiltDirectory);
    tiltDir.mkdir(newFile);

    QFile::copy(fileName, tiltDir.path() + "/" + newFile + "/" + newFile + '.' + ext);
    QString newFilePath = tiltDir.path() + "/" + newFile;
    QString newFileConfigPath = newFilePath + "/2dx_image.cfg";
    if (!QFileInfo(newFileConfigPath).exists()) {
        //HENN>
        qDebug() << "Copying " << tiltConfigLocation << " to " << newFileConfigPath;
        if (!QFile::copy(tiltConfigLocation, newFileConfigPath))
            qDebug() << "Failed to copy " << tiltConfigLocation << " to " << newFileConfigPath;
        else {
            QFileInfo oldFile(fileName);
            QString name = oldFile.fileName();
            QString oldFileDir = oldFile.absolutePath();
            QString oldStackName = oldFileDir + "/../aligned_stacks/" + name;
            QFileInfo oldStack(oldStackName);
            if (!oldStack.exists()) {
                // qDebug() << "Stack " << oldStackName << " not found. Trying ";
                oldStackName = oldFileDir + "/../DC_stacks/" + name;
                // qDebug() << oldStackName;
            }

            QString newStackName = newFilePath + "/" + newFile + "_stack." + ext;
            // qDebug() << "oldStackName = " << oldStackName << ", newStackName = " << newStackName;

            QFile f(newFileConfigPath);
            if (f.open(QIODevice::Append | QIODevice::Text)) {
                // qDebug() << "Apending imagename_original = " << name << " to 2dx_image.cfg file.";
                QTextStream stream(&f);
                // qDebug() << "set imagename = " << newFile;
                stream << "set imagename = " << newFile << endl;
                // qDebug() << "set imagenumber = " << frame+ subID;
                stream << "set imagenumber = " << frame + subID << endl;
                // qDebug() << "set nonmaskimagename = " << newFile;
                stream << "set nonmaskimagename = " << newFile << endl;
                // qDebug() << "set imagename_original = " << name;
                stream << "set imagename_original = " << name << endl;

                if (oldStack.exists()) {
                    qDebug() << "Copying " << oldStackName << " to " << newStackName;
                    stream << "set movie_stackname = " << newFile + "_stack." + ext << endl;
                    if (!QFile::copy(oldStackName, newStackName)) {
                        qDebug() << "ERROR when trying to copy stack." << endl;
                    }
                }
                f.close();
            }
        }
        //HENN<
    }
    importProcess.start(mainData->getApp("2dx_image") + " " + newFilePath + " " + "\"2dx_initialize\"");
    importProcess.waitForFinished(8 * 60 * 60 * 1000);
}

void mainWindow::importFinished() {
    importCount--;
    if (importCount <= 0) albumCont->updateModel();
}

void mainWindow::import() {
    QMessageBox::StandardButton reply;
    reply = QMessageBox::question(this, 
                                    "Import Images", 
                                    QString("A new and improved tool to import images is available in Custom Tab called as <Import Images and Movies>\n")
                                    +"Are you sure you want to continue using the old import tool?",
                                    QMessageBox::Yes|QMessageBox::No);
    if (reply == QMessageBox::Yes) 
    {
        QStringList fileList = QFileDialog::getOpenFileNames(NULL, "Choose image files to add", mainData->getDir("project"), "Images (*.tif *.mrc)");
        if (fileList.isEmpty()) 
        {
            return;
        }
        importTool *import = new importTool(mainData, fileList);
        connect(import, SIGNAL(acceptedImages(const QHash< QString, QHash < QString, QString > >&)), this, SLOT(importFiles(const QHash<QString, QHash<QString, QString> > &)));
    }
}

void mainWindow::autoImport() {
    if (!autoImportMonitor) {
        QString dir = QFileDialog::getExistingDirectory(NULL, "Choose import directory", mainData->getDir("project"));
        autoImportMonitor = new autoImportTool(mainData, dir);
    } else
        autoImportMonitor->show();
}

void mainWindow::open() {
    QProcess::startDetached(mainData->getApp("this"));
}

void mainWindow::openURL(const QString &url) {
    QProcess::startDetached(mainData->getApp("webBrowser") + " " + url);
}

void mainWindow::increaseFontSize() {
    QFont font = QApplication::font();
    font.setPointSize(font.pointSize() + 1);
    mainData->userConf()->set("fontSize", QString::number(font.pointSize()));
    mainData->userConf()->save();
    QApplication::setFont(font);
    updateFontInfo();
}

void mainWindow::decreaseFontSize() {
    QFont font = QApplication::font();
    font.setPointSize(font.pointSize() - 1);
    mainData->userConf()->set("fontSize", QString::number(font.pointSize()));
    mainData->userConf()->save();
    QApplication::setFont(font);
    updateFontInfo();
}

void mainWindow::toggleAutoSave() {
    m_do_autosave = !m_do_autosave;

    if (m_do_autosave) {
        QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched on"));
        timer->start(timer_refresh);
    } else {
        QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched off"));
        timer->stop();
    }
}

void mainWindow::updateFontInfo() {
    executionCont->updateFontInfo();
}

void mainWindow::saveDirectorySelection() {
    QString saveName = QFileDialog::getSaveFileName(this, "Save Selection As...", mainData->getDir("working") + "/2dx_merge_dirfile.dat");
    if (QFileInfo(saveName).exists()) QFile::remove(saveName);
    QFile::copy(mainData->getDir("working") + "/2dx_merge_dirfile.dat", saveName);
}

void mainWindow::loadDirectorySelection() {
    QString loadName = QFileDialog::getOpenFileName(this, "Save Selection As...", mainData->getDir("working") + "/2dx_merge_dirfile.dat");
    albumCont->loadSelection(loadName);
}

void mainWindow::showAlbum(bool show) {
    if (album == NULL)
        launchAlbum(mainData->getDir("project"));

    album->setHidden(!show);
}

void mainWindow::showEuler(bool show) {
    if (euler == NULL)
        launchEuler();

    euler->setHidden(!show);
}

void mainWindow::showReproject(bool show) {
    if (reproject == NULL)
        launchReproject();

    reproject->setHidden(!show);
}

void mainWindow::editHelperConf() {
    new confEditor(mainData->getSubConf("appConf"));
}
