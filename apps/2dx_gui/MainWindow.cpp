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
#include <QToolButton>
#include <QButtonGroup>

#include "BlockContainer.h"
#include "UserPreferences.h"
#include "ProjectPreferences.h"
#include "UserProjects.h"
#include "ProjectData.h"
#include "ApplicationData.h"

#include "MainWindow.h"

MainWindow::MainWindow(QWidget *parent)
: QMainWindow(parent) {
    
    if (!QFileInfo(projectData.projectWorkingDir().canonicalPath() + "/2dx_merge.cfg").exists()) {
        QMessageBox::critical(this, "Configuration Files not found!", "This folder does not have 2DX configuration files. Will quit now.");
        exit(0);
    }

    m_do_autosave = true;
    
    timer_refresh = 10000;
    timer = new QTimer(this);
    
    updateWindowTitle();
    setUnifiedTitleAndToolBarOnMac(true);

    updates = new UpdateWindow(this);
    updates->hide();

    about = new AboutWindow(this);
    about->hide();
    
    imageLibrary = new ImageLibrary(this);
   
    setupWindows();
    setupToolBar();
    setupActions();
    setupMenuBar();
    
    connect(libraryWin_->getDirView(), SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(showImageWindow(const QModelIndex&)));
    connect(projectData.projectParameterData(), SIGNAL(dataModified(bool)), this, SLOT(setSaveState(bool)));
    
    QStringList imagesOpen = projectData.imagesOpen();
    for(int i=0; i<imagesOpen.size(); ++i) showImageWindow(imagesOpen[i], true);
    
    connect(imageLibrary, &ImageLibrary::shouldLoadImage, [=] (const QString& path) {showImageWindow(path, true);});
    connect(openLibraryWindowAct, &QToolButton::toggled, [=] (bool check) {imageLibrary->setHidden(check);});
    connect(imageLibrary->thumbnailContainer()->getModel(), &QStandardItemModel::itemChanged, [=] () {
        imageLibrary->thumbnailContainer()->saveChecks();
        libraryWin_->getDirModel()->loadSelection(); 
    });
     
    openLibraryWindowAct->setChecked(true);
    centralWin_->setCurrentWidget(libraryWin_);
    
    QWidget* mainWidget = new QWidget;
    QGridLayout* mainLayout = new QGridLayout;
    mainWidget->setLayout(mainLayout);
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addWidget(imageLibrary, 0, 0);
    mainLayout->addWidget(centralWin_, 1, 0);
    
    setCentralWidget(mainWidget);
    
    connect(timer, SIGNAL(timeout()), this, SLOT(save()));
    timer->start(timer_refresh);
    
    resize(1300, 900);
}

void MainWindow::setupActions() {
    openAction = new QAction(ApplicationData::icon("open"), tr("&Open Project"), this);
    openAction->setShortcut(tr("Ctrl+O"));
    connect(openAction, SIGNAL(triggered()), this, SLOT(open()));

    saveAction = new QAction(ApplicationData::icon("save"), tr("&Save"), this);
    saveAction->setShortcut(tr("Ctrl+S"));
    connect(saveAction, SIGNAL(triggered()), projectData.projectParameterData(), SLOT(save()));
}

void MainWindow::setupMenuBar() {
    /**
     * Setup File menu
     */
    QMenu *fileMenu = new QMenu("File");
    fileMenu->addAction(openAction);
    
    QStringList recents = UserProjects().projectPaths();
    if (recents.count() > 0) {
        QMenu* openRecentsMenu = new QMenu("Recent Projects");
        openRecentsMenu->setIcon(ApplicationData::icon("recent"));
        for(int i=0; i< recents.size(); ++i) {
            if (recents[i] != "" && recents[i] != projectData.projectDir().canonicalPath()) {
                QAction* act = new QAction(recents[i], this);
                connect(act, &QAction::triggered,
                        [ = ] (bool){open(recents[i]);});
                openRecentsMenu->addAction(act);
            }
        }
        fileMenu->addMenu(openRecentsMenu);
    }
    
    fileMenu->addAction(saveAction);

    QAction *closeAction = new QAction(ApplicationData::icon("quit"), "Quit", this);
    closeAction->setShortcut(tr("Ctrl+Q"));
    connect(closeAction, SIGNAL(triggered()), qApp, SLOT(closeAllWindows()));
    fileMenu->addAction(closeAction);


    /**
     * Setup Options menu
     */
    QMenu *optionMenu = new QMenu("Options");

    QAction *openPreferencesAction = new QAction(ApplicationData::icon("preferences"), "Preferences", this);
    connect(openPreferencesAction, SIGNAL(triggered()), this, SLOT(editHelperConf()));
    optionMenu->addAction(openPreferencesAction);
    
    QAction* setProjectNameAction = new QAction(ApplicationData::icon("change_name"), "Change Project Name", this);
    connect(setProjectNameAction, SIGNAL(triggered()), this, SLOT(changeProjectName()));
    optionMenu->addAction(setProjectNameAction);
    
    QAction* reindeximageAct = new QAction(ApplicationData::icon("refresh"), "Reindex images", this);
    connect(reindeximageAct, &QAction::triggered, [=](){
        projectData.indexImages();
    });
    optionMenu->addAction(reindeximageAct);

    QAction *showAutoSaveAction = new QAction(ApplicationData::icon("autosave"), "Autosave On/Off", this);
    connect(showAutoSaveAction, SIGNAL(triggered()), this, SLOT(toggleAutoSave()));
    optionMenu->addAction(showAutoSaveAction);

    /**
     * Setup Help menu
     */
    QMenu *helpMenu = new QMenu("Help");

    QAction *viewOnlineHelp = new QAction(ApplicationData::icon("manual"), tr("&View Online Help"), this);
    viewOnlineHelp->setCheckable(false);
    connect(viewOnlineHelp, &QAction::triggered, [](){QDesktopServices::openUrl(ApplicationData::helpUrl());});
    helpMenu->addAction(viewOnlineHelp);

    QAction* bugReport = new QAction(ApplicationData::icon("Bug"), tr("&Report Issue/Bug"), this);
    bugReport->setCheckable(false);
    connect(bugReport, &QAction::triggered, [](){QDesktopServices::openUrl(ApplicationData::bugReportUrl());});
    helpMenu->addAction(bugReport);

    QAction *showUpdatesAction = new QAction(ApplicationData::icon("update"), "Update...", this);
    connect(showUpdatesAction, SIGNAL(triggered()), updates, SLOT(show()));
    helpMenu->addAction(showUpdatesAction);

    QAction *showAboutAction = new QAction(ApplicationData::icon("about"), "About", this);
    connect(showAboutAction, SIGNAL(triggered()), about, SLOT(show()));
    helpMenu->addAction(showAboutAction);

    menuBar()->addMenu(fileMenu);
    menuBar()->addMenu(optionMenu);
    menuBar()->addMenu(helpMenu);
}

void MainWindow::setupToolBar() {
    QToolBar* mainToolBar = addToolBar("Navigation");
    mainToolBar->setIconSize(QSize(32, 32));
    mainToolBar->setFloatable(false);
    mainToolBar->setMovable(false);
    
    openLibraryWindowAct = setupMainNavigationButton("library", "Library", "Project Library", true, libraryWin_);
    openImageWindowAct = setupMainNavigationButton("image", "Process", "Process Images", true, imageWin_);
    openMergeWindowAct = setupMainNavigationButton("merge_tool", "Merge", "Merge Tool", true, mergeWin_);
    openSPWindowAct = setupMainNavigationButton("singleparticle", "Particles", "Single Particle Processing", true, spWin_);          
    openProjectToolsAct = setupMainNavigationButton("project_tools", "Project", "Project Tools", true, projectToolsWin_);
    
    QButtonGroup* group = new QButtonGroup(mainToolBar);
    group->addButton(openLibraryWindowAct);
    group->addButton(openImageWindowAct);
    group->addButton(openMergeWindowAct);
    group->addButton(openSPWindowAct);
    group->addButton(openProjectToolsAct);
    group->setExclusive(true);
    
    QToolButton* openPreferencesAction = setupMainNavigationButton("preferences", "Settings", "Preferences", false);
    connect(openPreferencesAction, SIGNAL(clicked()), this, SLOT(editHelperConf()));
    
    QWidget* spacer1 = new QWidget();
    spacer1->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QWidget* spacer2 = new QWidget();
    spacer2->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    
    mainToolBar->addWidget(openProjectToolsAct);
    mainToolBar->addWidget(spacer1);
    mainToolBar->addWidget(openLibraryWindowAct);
    mainToolBar->addWidget(openImageWindowAct);
    mainToolBar->addWidget(openMergeWindowAct);
    mainToolBar->addWidget(openSPWindowAct);
    mainToolBar->addWidget(spacer2);
    mainToolBar->addWidget(openPreferencesAction);
    
}

void MainWindow::setupWindows() {
    centralWin_ = new QStackedWidget(this);

    libraryWin_ = new LibraryTab(this);
    
    QStringList scriptsDir;
    scriptsDir << ApplicationData::scriptsDir().canonicalPath() + "/merge/merge2D" 
               << ApplicationData::scriptsDir().canonicalPath() + "/merge/merge3D" 
               << ApplicationData::scriptsDir().canonicalPath() + "/merge/custom";
    mergeWin_ = new ExecutionWindow(projectData.projectWorkingDir(), scriptsDir, ExecutionWindow::Type::PROJECT, this);
    mergeWin_->runInitialization();
    
    spWin_ = new ExecutionWindow(projectData.projectWorkingDir(),
            QStringList() << ApplicationData::scriptsDir().canonicalPath() + "/singleparticle/frealign" 
                          << ApplicationData::scriptsDir().canonicalPath() + "/singleparticle/relion", ExecutionWindow::Type::PROJECT, this);
    spWin_->runInitialization();
    
    projectToolsWin_ = new ExecutionWindow(projectData.projectWorkingDir(),
            QStringList() << ApplicationData::scriptsDir().canonicalPath() + "/project", ExecutionWindow::Type::PROJECT, this);
    
    imageWin_ = new ImageTab(this);
    
    centralWin_->addWidget(libraryWin_);
    centralWin_->addWidget(mergeWin_);
    centralWin_->addWidget(imageWin_);
    centralWin_->addWidget(spWin_);
    centralWin_->addWidget(projectToolsWin_);

    connect(imageWin_, SIGNAL(imagesOpenChanged(QStringList)), libraryWin_, SLOT(setImagesOpen(QStringList)));
}

void MainWindow::showImageWindow(const QModelIndex& index, bool supressWarnings) {
    QString workingDir = libraryWin_->getDirModel()->pathFromIndex(index);
    showImageWindow(workingDir, supressWarnings);
}

void MainWindow::showImageWindow(const QString& workingDir, bool supressWarnings) {
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

void MainWindow::setSaveState(bool state) {
    if (state == false) {
        saveAction->setChecked(false);
        saveAction->setCheckable(false);
    } else {
        saveAction->setCheckable(true);
        saveAction->setChecked(true);
    }
}

void MainWindow::closeEvent(QCloseEvent *event) {
    if (!projectData.projectParameterData()->isModified() && !imageWin_->configModified()) {
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

void MainWindow::reallyClose(QCloseEvent *event) {
    event->accept();
    UserPreferences().saveCurrentFontSettings();
    UserPreferences().saveWindowPreferences(this);
    projectData.setImagesOpen(imageWin_->getImagesOpen());
}

void MainWindow::open(const QString& projectPath) {
    QProcess::startDetached(ApplicationData::guiApp() + " " + projectPath);
}

void MainWindow::toggleAutoSave() {
    m_do_autosave = !m_do_autosave;

    if (m_do_autosave) {
        QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched on"));
        timer->start(timer_refresh);
    } else {
        QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched off"));
        timer->stop();
    }
}

void MainWindow::editHelperConf() {
    if (!preferencesDialogInit_) {
        preferencesDialogInit_ = true;
        preferencesDialog_ = new PreferencesDialog(this);
    }
    preferencesDialog_->showNormal();
    //new confEditor(mainData->getSubConf("appConf"));
}

void MainWindow::changeProjectName() {
    bool ok;
    QString projectName = QInputDialog::getText(this, "Project Name", "Enter a name for the project", QLineEdit::Normal,
            projectData.projectName(), &ok);

    if (ok && !projectName.isEmpty()) {
        projectData.setProjectName(projectName);
    }
    
    libraryWin_->updateProjectName(projectName);
    updateWindowTitle();
}

void MainWindow::updateWindowTitle() {
    setWindowTitle(projectData.projectName() + " | 2dx (" + ApplicationData::versionNumber() + ")");
}

void MainWindow::save() {
    if(projectToolsWin_->isRunningScript()) return;
    imageWin_->saveConfigs();
    projectData.projectParameterData()->save();
}

QToolButton* MainWindow::setupMainNavigationButton(const QString& ic, const QString& title, const QString& desc, bool checkable, QWidget* connectedWidget) {
    QToolButton* button = new QToolButton();
    button->setIcon(ApplicationData::icon(ic));
    button->setText(title);
    button->setToolTip(desc);
    button->setCheckable(checkable);
    button->setFixedSize(64, 64);
    button->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    if(connectedWidget) {
        connect(button, &QToolButton::toggled,
            [=] () {
                centralWin_->setCurrentWidget(connectedWidget);
                imageLibrary->setHeaderTitle(desc);
            });
    }
            
    return button;
}