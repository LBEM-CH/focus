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
#include "ScriptModuleProperties.h"

MainWindow::MainWindow(const QString& projectPath, QWidget *parent)
: QMainWindow(parent) {
    
    projectData.setParent(this);
        
    UserPreferences().loadMainWindowPreferences(this);
    
    if (!QFileInfo(projectData.projectWorkingDir().canonicalPath() + "/2dx_merge.cfg").exists()) {
        QMessageBox::critical(this, "Configuration Files not found!", "This folder does not have configuration files. Will quit now.");
        exit(0);
    }
    
    updateWindowTitle();
    setUnifiedTitleAndToolBarOnMac(true);

    updates = new UpdateWindow(this);
    updates->hide();

    about = new AboutWindow(this);
    about->hide();
    
    imageLibrary = new SelectionCountHeader(this);
   
    setupWindows();
    setupMenuBar();
    
    connect(libraryWin_->getDirView(), SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(showImageWindow(const QModelIndex&)));
    
    QList<ProjectImage*> imagesOpen = projectData.imagesOpen();
    for(int i=0; i<imagesOpen.size(); ++i) showImageWindow(imagesOpen[i], true);
    
    connect(openLibraryWindowAct, &QToolButton::toggled, [=] (bool check) {imageLibrary->setHidden(check);});
         
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
    
    connect(&projectData, &ProjectData::focusProcessingWindow, [=]{
        openImageWindowAct->setChecked(true);
        centralWin_->setCurrentWidget(imageWin_);
        imageWin_->focusOnProcessingWindow();
    });
    
    connect(&projectData, &ProjectData::imageToBeOpened, [=](ProjectImage* image){
        showImageWindow(image, true);
    });
    
    connect(&projectData, &ProjectData::projectNameChanged, [=](const QString& name) {
       updateWindowTitle(); 
    });
    
    projectData.emitStartupFinished();
    
}

void MainWindow::setupMenuBar() {
    /**
     * Setup File menu
     */
    QAction* openAction = new QAction(ApplicationData::icon("open"), tr("&Open Project"), this);
    openAction->setShortcut(tr("Ctrl+O"));
    connect(openAction, SIGNAL(triggered()), this, SLOT(open()));
    
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
    
    QAction* saveAction = new QAction(ApplicationData::icon("save"), tr("&Save"), this);
    saveAction->setShortcut(tr("Ctrl+S"));
    connect(saveAction, SIGNAL(triggered()), projectData.projectParameterData(), SLOT(save()));
    
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

void MainWindow::setupWindows() {
    mainToolBar = addToolBar("Navigation");
    mainToolBar->setIconSize(QSize(32, 32));
    mainToolBar->setFloatable(false);
    mainToolBar->setMovable(false);
    
    group = new QButtonGroup(mainToolBar);
    group->setExclusive(true);
    
    QWidget* spacer1 = new QWidget();
    spacer1->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QWidget* spacer2 = new QWidget();
    spacer2->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    
    centralWin_ = new QStackedWidget(this);
    
    //Setup project window
    projectToolsWin_ = new ProjectWindow(this);
    centralWin_->addWidget(projectToolsWin_);
    openProjectToolsAct = setupMainNavigationButton("project_tools", "Project", "Manage Project", true, projectToolsWin_);
    mainToolBar->addWidget(openProjectToolsAct);
    group->addButton(openProjectToolsAct);
    mainToolBar->addWidget(spacer1);
    
    //Setup library window
    libraryWin_ = new LibraryTab(this);
    centralWin_->addWidget(libraryWin_);
    openLibraryWindowAct = setupMainNavigationButton("library", "Library", "Project Library", true, libraryWin_);
    mainToolBar->addWidget(openLibraryWindowAct);
    group->addButton(openLibraryWindowAct);
    mainToolBar->addSeparator();
    
    //Setup import window
    importWin_ = new AutoImportWindow(this);
    centralWin_->addWidget(importWin_);
    connect(importWin_, &AutoImportWindow::imageToBeOpened, [=](ProjectImage* image){
        showImageWindow(image, true);
    });
    openImportWindowAct = setupMainNavigationButton("import", "Import", "Import Movies and Images", true, importWin_);
    mainToolBar->addWidget(openImportWindowAct);
    group->addButton(openImportWindowAct);
    
    //Setup process window
    imageWin_ = new ImageTab(this);
    centralWin_->addWidget(imageWin_);
    openImageWindowAct = setupMainNavigationButton("image", "Process", "Process Images", true, imageWin_);
    mainToolBar->addWidget(openImageWindowAct);
    group->addButton(openImageWindowAct);
    
    //Setup extra windows depending on the mode
    addExecutionTab(ApplicationData::scriptsDir().canonicalPath() + "/merge/", "merge_tool", "Merge", "Merge Tool");
    addExecutionTab(ApplicationData::scriptsDir().canonicalPath() + "/singleparticle/", "singleparticle", "Particles", "Single Particle Processing of 2D Crystals"); 
    
    //Setup preferences tab
    QToolButton* openPreferencesAction = setupMainNavigationButton("preferences", "Settings", "Preferences", false);
    connect(openPreferencesAction, SIGNAL(clicked()), this, SLOT(editHelperConf()));
    mainToolBar->addWidget(spacer2);
    mainToolBar->addWidget(openPreferencesAction);
    
}

void MainWindow::addExecutionTab(const QString& scriptsDir, const QString& icon, const QString& title, const QString& desc) {
    QStringList subdirs = ScriptModuleProperties(scriptsDir).subfolders();
    if(subdirs.size() > 0) {
        ExecutionWindow* win = new ExecutionWindow(subdirs);
        centralWin_->addWidget(win);
        QToolButton* button = setupMainNavigationButton(icon, title, desc, true, win);
        mainToolBar->addWidget(button);
        group->addButton(button);
    }
}


void MainWindow::showImageWindow(const QModelIndex& index, bool supressWarnings) {
    QString workingDir = libraryWin_->getDirModel()->pathFromIndex(index);
    showImageWindow(projectData.projectImage(QDir(workingDir)), supressWarnings);
}

void MainWindow::showImageWindow(ProjectImage* image, bool supressWarnings) {
    if (!image) {
        if(!supressWarnings) QMessageBox::critical(this, tr("Open error!"), image->toString() + "is not an image. Only the images can be opened on double click.");
        return;
    }
    
    openImageWindowAct->setChecked(true);
    centralWin_->setCurrentWidget(imageWin_);
    imageWin_->showImageWindow(image);
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
    UserPreferences().saveMainWindowPreferences(this);
}

void MainWindow::open(const QString& projectPath) {
    QProcess::startDetached(ApplicationData::mainApp() + " " + projectPath);
}

void MainWindow::editHelperConf() {
    if (!preferencesDialogInit_) {
        preferencesDialogInit_ = true;
        preferencesDialog_ = new PreferencesDialog(this);
    }
    preferencesDialog_->showNormal();
    //new confEditor(mainData->getSubConf("appConf"));
}

void MainWindow::updateWindowTitle() {
    setWindowTitle(projectData.projectName() + " | Focus (" + ApplicationData::versionNumber() + ")");
}

void MainWindow::save() {
    imageWin_->saveConfigs();
    projectData.projectParameterData()->save();
}

QToolButton* MainWindow::setupMainNavigationButton(const QString& ic, const QString& title, const QString& desc, bool checkable, QWidget* connectedWidget) {
    QToolButton* button = new QToolButton();
    button->setIcon(ApplicationData::icon(ic));
    button->setText(title);
    button->setToolTip(desc);
    button->setCheckable(checkable);
    button->setFixedSize(80, 64);
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