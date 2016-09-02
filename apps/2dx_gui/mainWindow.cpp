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
#include <QActionGroup>
#include <iostream>

#include "mainWindow.h"
#include "blockContainer.h"
#include "user_preferences.h"
#include "project_preferences.h"
#include "user_projects.h"

using namespace std;

mainWindow::mainWindow(const QString &directory, QWidget *parent)
: QMainWindow(parent) {
    
    if (!QFileInfo(directory + "/merge/" + "2dx_merge.cfg").exists()) {
        QMessageBox::critical(this, "Configuration Files not found!", "This folder does not have 2DX configuration files. Will quit now.");
        exit(0);
    }

    m_do_autosave = true;
    mainData = setupMainConfiguration(directory);
    UserProjects(mainData).addProjectPath(mainData->getDir("project"));
    UserPreferences(mainData).loadAllFontSettings();
    UserPreferences(mainData).loadWindowPreferences(this);
    
    timer_refresh = 10000;
    timer = new QTimer(this);
    
    updateWindowTitle();
    setUnifiedTitleAndToolBarOnMac(true);

    updates = new updateWindow(mainData, this);
    updates->hide();

    about = new aboutWindow(mainData, this);
    about->hide();

    setupToolBar();
    setupWindows();
    setupActions();
    setupMenuBar();
    
    connect(mainData, SIGNAL(dataModified(bool)), this, SLOT(setSaveState(bool)));

    euler = NULL;
    reproject = NULL;

    openLibraryWindowAct->setChecked(true);
    centralWin_->setCurrentWidget(libraryWin_);
    setCentralWidget(centralWin_);
    
    connect(timer, SIGNAL(timeout()), this, SLOT(save()));
    timer->start(timer_refresh);
    
    resize(1300, 900);
}

confData* mainWindow::setupMainConfiguration(const QString &directory) {
    confData* mainData;

    QDir applicationDir, configDir;

#ifdef Q_OS_MAC
    applicationDir = QDir(QApplication::applicationDirPath() + "/../../../");
#else
    applicationDir = QDir(QApplication::applicationDirPath());
#endif

    configDir = QDir(applicationDir.canonicalPath() + "/../" + "resources/config/");

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

    mainData->setDir("binDir", mainData->getDir("application") + "/../kernel/mrc/bin/");
    mainData->setDir("procDir", mainData->getDir("application") + "/../scripts/proc/");
    createDir(mainData->getDir("working") + "/config");

    mainData->setDir("config", configDir);

    createDir(QDir::homePath() + "/.2dx/");
    QString userPath = QDir::homePath() + "/.2dx";
    createDir(userPath + "/2dx_merge");

    confData *cfg = new confData(userPath + "/2dx.cfg", mainData->getDir("config") + "/" + "2dx.cfg", true);
    if (cfg->isEmpty()) {
        cerr << "2dx.cfg not found." << endl;
        exit(0);
    }
    cfg->save();

    mainData->setAppConf(cfg);

    mainData->setDir("home_2dx", userPath);
    mainData->setDir("pluginsDir", mainData->getDir("application") + "/.." + "/plugins");
    mainData->setDir("translatorsDir", mainData->getDir("pluginsDir") + "/translators");
    mainData->setDir("resource", mainData->getDir("application") + "/../resources/images");
    mainData->setDir("2dx_bin", mainData->getDir("application") + "/.." + "/bin");
    mainData->addApp("this", mainData->getDir("application") + "/../" + "bin/" + "2dx_gui");
    mainData->addApp("2dx_image", mainData->getDir("2dx_bin") + "/" + "2dx_image");
    mainData->addApp("2dx_merge", mainData->getDir("2dx_bin") + "/" + "2dx_merge");

    createDir(mainData->getDir("working") + "/proc");
    mainData->setDir("remoteProc", mainData->getDir("working") + "/proc/");
    createDir(mainData->getDir("working") + "/LOGS");
    mainData->setDir("logs", mainData->getDir("working") + "/LOGS");
    mainData->setDir("merge2DScripts", QDir(mainData->getDir("application") + "../scripts/merge/merge2D/"));
    mainData->setDir("merge3DScripts", QDir(mainData->getDir("application") + "../scripts/merge/merge3D/"));
    mainData->setDir("customScripts", QDir(mainData->getDir("application") + "../scripts/merge/custom/"));
    mainData->setDir("frealignScripts", QDir(mainData->getDir("application") + "../scripts/singleparticle/frealign/"));
    mainData->setDir("relionScripts", QDir(mainData->getDir("application") + "../scripts/singleparticle/relion/"));
    mainData->setDir("projectToolScripts", QDir(mainData->getDir("application") + "../scripts/project/"));
    mainData->addImage("appImage", new QImage(mainData->getDir("application") + "icons/icon.png"));

    mainData->addApp("logBrowser", mainData->getDir("application") + "/../" + "bin/" + "2dx_logbrowser");

    mainData->setURL("help", "http://2dx.org/documentation/2dx-software");
    mainData->setURL("bugReport", "https://github.com/C-CINA/2dx/issues");

    if (!setupIcons(mainData, mainData->getDir("resource"))) cerr << "Error loading images." << mainData->getDir("resource").toStdString() << endl;

    return mainData;
}

void mainWindow::setupActions() {
    openAction = new QAction(*(mainData->getIcon("open")), tr("&Open Project"), this);
    openAction->setShortcut(tr("Ctrl+O"));
    connect(openAction, SIGNAL(triggered()), this, SLOT(open()));

    saveAction = new QAction(*(mainData->getIcon("save")), tr("&Save"), this);
    saveAction->setShortcut(tr("Ctrl+S"));
    connect(saveAction, SIGNAL(triggered()), mainData, SLOT(save()));
}

void mainWindow::setupMenuBar() {
    /**
     * Setup File menu
     */
    QMenu *fileMenu = new QMenu("File");
    fileMenu->addAction(openAction);
    
    QStringList recents = UserProjects(mainData).projectPaths();
    if (recents.count() > 0) {
        QMenu* openRecentsMenu = new QMenu("Recent Projects");
        openRecentsMenu->setIcon(*(mainData->getIcon("recent")));
        for(int i=0; i< recents.size(); ++i) {
            if (recents[i] != "" && recents[i] != QDir(mainData->getDir("project")).absolutePath()) {
                QAction* act = new QAction(recents[i], this);
                connect(act, &QAction::triggered,
                        [ = ] (bool){open(recents[i]);});
                openRecentsMenu->addAction(act);
            }
        }
        fileMenu->addMenu(openRecentsMenu);
    }
    
    fileMenu->addAction(saveAction);

    QAction *closeAction = new QAction(*(mainData->getIcon("quit")), "Quit", this);
    closeAction->setShortcut(tr("Ctrl+Q"));
    connect(closeAction, SIGNAL(triggered()), qApp, SLOT(closeAllWindows()));
    fileMenu->addAction(closeAction);


    /**
     * Setup Options menu
     */
    QMenu *optionMenu = new QMenu("Options");

    QAction *openPreferencesAction = new QAction(*(mainData->getIcon("preferences")), "Preferences", this);
    connect(openPreferencesAction, SIGNAL(triggered()), this, SLOT(editHelperConf()));
    optionMenu->addAction(openPreferencesAction);
    
    QAction* setProjectNameAction = new QAction(*(mainData->getIcon("change_name")), "Change Project Name", this);
    connect(setProjectNameAction, SIGNAL(triggered()), this, SLOT(changeProjectName()));
    optionMenu->addAction(setProjectNameAction);

    QAction *showAutoSaveAction = new QAction(*(mainData->getIcon("autosave")), "Autosave On/Off", this);
    connect(showAutoSaveAction, SIGNAL(triggered()), this, SLOT(toggleAutoSave()));
    optionMenu->addAction(showAutoSaveAction);

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

    connect(mapper, SIGNAL(mapped(const QString &)), this, SLOT(openURL(const QString &)));

    QAction *showUpdatesAction = new QAction(*(mainData->getIcon("update")), "Update...", this);
    connect(showUpdatesAction, SIGNAL(triggered()), updates, SLOT(show()));
    helpMenu->addAction(showUpdatesAction);

    QAction *showAboutAction = new QAction(*(mainData->getIcon("about")), "About", this);
    connect(showAboutAction, SIGNAL(triggered()), about, SLOT(show()));
    helpMenu->addAction(showAboutAction);

    menuBar()->addMenu(fileMenu);
    menuBar()->addMenu(optionMenu);
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

void mainWindow::setupToolBar() {
    QToolBar* mainToolBar = addToolBar("");
    mainToolBar->setIconSize(QSize(32, 32));
    mainToolBar->setFloatable(false);
    mainToolBar->setMovable(false);
    
    openLibraryWindowAct = new QAction(*(mainData->getIcon("library")), "Project Library", mainToolBar);
    openLibraryWindowAct->setCheckable(true);
    connect(openLibraryWindowAct, &QAction::triggered,
            [=] () {
                centralWin_->setCurrentWidget(libraryWin_);
            });
            
    openImageWindowAct = new QAction(*(mainData->getIcon("image")), "Edit Images", mainToolBar);
    openImageWindowAct->setCheckable(true);
    connect(openImageWindowAct, &QAction::triggered,
            [=] () {
                centralWin_->setCurrentWidget(imageWin_);
            });
    
    openMergeWindowAct = new QAction(*(mainData->getIcon("merge_tool")), "Merge Tool", mainToolBar);
    openMergeWindowAct->setCheckable(true);
    connect(openMergeWindowAct, &QAction::triggered,
            [=] () {
                centralWin_->setCurrentWidget(mergeWin_);
            });
            
    openSPWindowAct = new QAction(*(mainData->getIcon("singleparticle")), "Export project to Single Particle packages", mainToolBar);
    openSPWindowAct->setCheckable(true);
    connect(openSPWindowAct, &QAction::triggered,
            [=] () {
                centralWin_->setCurrentWidget(spWin_);
            });
            
    openProjectToolsAct = new QAction(*(mainData->getIcon("project_tools")), "Project Tools", mainToolBar);
    openProjectToolsAct->setCheckable(true);
    connect(openProjectToolsAct, &QAction::triggered,
            [=] () {
                centralWin_->setCurrentWidget(projectToolsWin_);
            });
    
    QActionGroup* group = new QActionGroup(mainToolBar);
    group->addAction(openLibraryWindowAct);
    group->addAction(openImageWindowAct);
    group->addAction(openMergeWindowAct);
    group->addAction(openSPWindowAct);
    group->addAction(openProjectToolsAct);
    group->setExclusive(true);
    
    QAction *openPreferencesAction = new QAction(*(mainData->getIcon("preferences")), "Preferences", this);
    openPreferencesAction->setCheckable(false);
    connect(openPreferencesAction, SIGNAL(triggered()), this, SLOT(editHelperConf()));
    
    QWidget* spacer1 = new QWidget();
    spacer1->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QWidget* spacer2 = new QWidget();
    spacer2->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    
    mainToolBar->addAction(openProjectToolsAct);
    mainToolBar->addWidget(spacer1);
    mainToolBar->addAction(openLibraryWindowAct);
    mainToolBar->addAction(openImageWindowAct);
    mainToolBar->addAction(openMergeWindowAct);
    mainToolBar->addAction(openSPWindowAct);
    mainToolBar->addWidget(spacer2);
    mainToolBar->addAction(openPreferencesAction);
    
}

void mainWindow::setupWindows() {
    centralWin_ = new QStackedWidget(this);

    results = new resultsData(mainData, mainData->getDir("working") + "/LOGS/" + "2dx_initialization.results", mainData->getDir("working"), this);
    libraryWin_ = new LibraryTab(mainData, results, this);
    
    QStringList scriptsDir;
    scriptsDir << mainData->getDir("merge2DScripts") << mainData->getDir("merge3DScripts") << mainData->getDir("customScripts");
    mergeWin_ = new ExecutionWindow(mainData, results, scriptsDir, this);
    mergeWin_->runInitialization();
    
    spWin_ = new ExecutionWindow(mainData, results, QStringList() << mainData->getDir("frealignScripts") << mainData->getDir("relionScripts"), this);
    spWin_->runInitialization();
    
    projectToolsWin_ = new ExecutionWindow(mainData, results, QStringList() << mainData->getDir("projectToolScripts"), this);
    
    imageWin_ = new ImageTab(mainData, this);
    connect(imageWin_, &ImageTab::saveAsProjectDefaultRequested, 
            [ = ] (confData* data) {
                timer->stop();
                data->saveAs(mainData->getDir("working") + "/2dx_merge.cfg");
                qApp->closeAllWindows();
            });
    
    centralWin_->addWidget(libraryWin_);
    centralWin_->addWidget(mergeWin_);
    centralWin_->addWidget(imageWin_);
    centralWin_->addWidget(spWin_);
    centralWin_->addWidget(projectToolsWin_);
    
    QStringList imagesOpen = ProjectPreferences(mainData).imagesOpen();
    for(int i=0; i<imagesOpen.size(); ++i) showImageWindow(imagesOpen[i], true);

    connect(mergeWin_, SIGNAL(scriptCompletedSignal()), libraryWin_, SLOT(maskResults()));
    connect(spWin_, SIGNAL(scriptCompletedSignal()), libraryWin_, SLOT(maskResults()));
    connect(projectToolsWin_, SIGNAL(scriptCompletedSignal()), libraryWin_, SLOT(maskResults()));
    connect(libraryWin_->getDirView(), SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(showImageWindow(const QModelIndex&)));
    connect(imageWin_, SIGNAL(imagesOpenChanged(QStringList)), libraryWin_, SLOT(setImagesOpen(QStringList)));
}

void mainWindow::showImageWindow(const QModelIndex& index, bool supressWarnings) {
    QString workingDir = libraryWin_->getDirModel()->pathFromIndex(index);
    showImageWindow(workingDir, supressWarnings);
}

void mainWindow::showImageWindow(const QString& workingDir, bool supressWarnings) {
    if (workingDir.isEmpty()) return;

    if (!QFileInfo(workingDir).exists()) {
        if(!supressWarnings) QMessageBox::critical(this, tr("Image folder error"), "The folder does not exist:\n" + workingDir + "\n\nThe image should be properly imported using the import action.");
        return;
    }

    if (!QFileInfo(workingDir + "/" + "2dx_image.cfg").exists()) {
        if(!supressWarnings) QMessageBox::critical(this, tr("Configuration file error"), "No configuration data found in:\n" + workingDir + "\n\nThe image should be properly imported using the import action.");
        return;
    }
    
    openImageWindowAct->setChecked(true);
    centralWin_->setCurrentWidget(imageWin_);
    imageWin_->showImageWindow(workingDir);
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

bool mainWindow::createDir(const QString &dir) {
    QDir directory(dir);
    if (!directory.exists())
        return directory.mkdir(dir);
    return false;
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
    if (!mainData->isModified() && !imageWin_->configModified()) {
        reallyClose(event);
    }
    else {
        int choice = QMessageBox::question(this, tr("Confirm Exit"), tr("Data not saved, exit?"), tr("Save && Quit"), tr("Quit Anyway"), QString("Cancel"), 0, 1);
        if (choice == 0) {
            this->save();
            reallyClose(event);
        } else if (choice == 1)
            reallyClose(event);
        else if (choice == 2)
            event->ignore();
    }
}

void mainWindow::reallyClose(QCloseEvent *event) {
    event->accept();
    UserPreferences(mainData).saveCurrentFontSettings();
    UserPreferences(mainData).saveWindowPreferences(this);
    ProjectPreferences(mainData).setImagesOpen(imageWin_->getImagesOpen());
}

void mainWindow::open(const QString& projectPath) {
    QProcess::startDetached(mainData->getApp("this") + " " + projectPath);
}

void mainWindow::openURL(const QString &url) {
    QProcess::startDetached(mainData->getApp("webBrowser") + " " + url);
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
    if (!preferencesDialogInit_) {
        preferencesDialogInit_ = true;
        preferencesDialog_ = new PreferencesDialog(mainData, this);
    }
    preferencesDialog_->showNormal();
    //new confEditor(mainData->getSubConf("appConf"));
}

void mainWindow::changeProjectName() {
    bool ok;
    QString projectName = QInputDialog::getText(this, "Project Name", "Enter a name for the project", QLineEdit::Normal,
            ProjectPreferences(mainData).projectName(), &ok);

    if (ok && !projectName.isEmpty()) {
        ProjectPreferences(mainData).setProjectName(projectName);
    }
    
    libraryWin_->updateProjectName(projectName);
    updateWindowTitle();
}

void mainWindow::updateWindowTitle() {
    setWindowTitle(ProjectPreferences(mainData).projectName() + " | 2dx (" + mainData->version_number() + ")");
}

void mainWindow::save() {
    if(centralWin_->currentWidget() == projectToolsWin_) return;
    imageWin_->saveConfigs();
    mainData->save();
}