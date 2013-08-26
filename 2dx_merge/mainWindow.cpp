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

#include "mainWindow.h"
#include <QDebug>
#include <QDesktopServices>
#include <QModelIndexList>
#include <QItemSelectionModel>
#include <iostream>
using namespace std;

mainWindow::mainWindow(const QString &directory, QWidget *parent)
          :QMainWindow(parent)
{
  QTime time;
  time.start();
  QDir applicationDir, configDir;

  #ifdef Q_WS_MAC
  applicationDir = QDir(QApplication::applicationDirPath() + "/../../../");
  #else
  applicationDir = QDir(QApplication::applicationDirPath());
  #endif

  configDir = QDir(applicationDir.canonicalPath() + "/../" + "config/");

  QString mergeConfigLocation = directory + "/merge/" + "2dx_merge.cfg";
  QString appConfigLocation = configDir.canonicalPath() + "/" + "2dx_master.cfg";
  if(QFileInfo(mergeConfigLocation).exists())
  {
    mainData = new confData(mergeConfigLocation, appConfigLocation);
    if(QFileInfo(appConfigLocation).exists())
      {
        mainData->updateConf(appConfigLocation);
      }
  }
  else
  {	  
    mainData = new confData(mergeConfigLocation, appConfigLocation);
  }
  mainData->setDir("project",QDir(directory));
  mainData->setDir("working",QDir(directory + "/merge"));

  if(!QFileInfo(mainData->getDir("working") + "/" + "2dx_merge.cfg").exists()) mainData->save();

  QWidget *centralWidget = new QWidget;
  setCentralWidget(centralWidget);
  layout = new QGridLayout(centralWidget);
  layout->setMargin(0);
  layout->setSpacing(0);
  centralWidget->setLayout(layout);

  mainData->setDir("application",applicationDir);

  mainData->setDir("binDir",mainData->getDir("application") + "/bin/");
  mainData->setDir("procDir",mainData->getDir("application") + "../proc/");
  createDir(mainData->getDir("working") + "/config");

  mainData->setDir("config",configDir);
  
  createDir(QDir::homePath() + "/.2dx/");
  QString userPath = QDir::homePath() + "/.2dx";
  createDir(userPath + "/2dx_merge");
  
  confData *cfg = new confData(userPath + "/2dx.cfg", mainData->getDir("config") + "/" + "2dx.cfg");
  if(cfg->isEmpty()) {cerr<<"2dx.cfg not found."<<endl; exit(0);}
  cfg->save();    


  installedVersion = mainData->version();
  setWindowTitle("2dx_merge, Version 2dx-" + installedVersion);

  mainData->setAppConf(cfg);
  
  mainData->setDir("home_2dx",userPath);
  mainData->setDir("pluginsDir",mainData->getDir("application") + "/.." + "/plugins");
  mainData->setDir("translatorsDir",mainData->getDir("pluginsDir") + "/translators");
  mainData->setDir("resource",QDir(mainData->getDir("config") + "/resource/"));
  mainData->setDir("2dx_bin",mainData->getDir("application") + "/.." + "/bin");
  mainData->addApp("this", mainData->getDir("application") + "/../" + "bin/" + "2dx_merge");  
  mainData->addApp("2dx_image", mainData->getDir("2dx_bin") + "/" + "2dx_image");
  mainData->addApp("2dx_merge", mainData->getDir("2dx_bin") + "/" + "2dx_merge");

  createDir(mainData->getDir("working") + "/proc");
  mainData->setDir("remoteProc",mainData->getDir("working") + "/proc/");
  createDir(mainData->getDir("working") + "/LOGS");
  mainData->setDir("logs",mainData->getDir("working") + "/LOGS");
  mainData->setDir("standardScripts",QDir(mainData->getDir("application") + "/" + "scripts-standard/"));
  mainData->setDir("customScripts",QDir(mainData->getDir("application") + "/" + "scripts-custom/"));
  mainData->addImage("appImage",new QImage("resource/icon.png"));
  
  mainData->addApp("logBrowser", mainData->getDir("application") + "/../" + "bin/" + "2dx_logbrowser");

  mainData->setURL("help","http://2dx.org/documentation/2dx-software");
  mainData->setURL("bugReport","http://2dx.org/documentation/2dx-bug-tracker");

  if(!setupIcons(mainData, mainData->getDir("resource"))) cerr<<"Error loading images."<<mainData->getDir("resource").toStdString()<<endl;

  connect(mainData,SIGNAL(dataModified(bool)),this,SLOT(setSaveState(bool)));

  connect(&importProcess,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(importFinished()));

  updates = new updateWindow(mainData,this);
  updates->hide();

  QWidget *headerWidget = setupHeader();

  standardScripts = new scriptModule(mainData,mainData->getDir("standardScripts"),scriptModule::standard);

  connect(standardScripts,SIGNAL(scriptCompleted(QModelIndex)),this,SLOT(standardScriptCompleted(QModelIndex)));
  connect(standardScripts,SIGNAL(reload()),this,SLOT(updateModel()));

  connect(this,SIGNAL(execute(bool)),standardScripts,SLOT(execute(bool)));
  QWidget *standardScriptWidget = setupScriptContainer(standardScripts, "Standard Scripts");
  connect(standardScripts,SIGNAL(progress(int)),progressBar,SLOT(setValue(int)));
  connect(standardScripts,SIGNAL(incrementProgress(int)),progressBar,SLOT(incrementValue(int)));

  customScripts = new scriptModule(mainData,mainData->getDir("customScripts"),scriptModule::custom);

  connect(customScripts,SIGNAL(scriptCompleted(QModelIndex)),this,SLOT(customScriptCompleted(QModelIndex)));
  connect(customScripts,SIGNAL(reload()),this,SLOT(reload()));

  connect(this,SIGNAL(execute(bool)),customScripts,SLOT(execute(bool)));
  QWidget *customScriptWidget = setupScriptContainer(customScripts, "Custom Scripts");
  connect(customScripts,SIGNAL(progress(int)),progressBar,SLOT(setValue(int)));
  connect(customScripts,SIGNAL(incrementProgress(int)),progressBar,SLOT(incrementValue(int)));  
  standardScripts->extendSelectionTo(customScripts);
  customScripts->extendSelectionTo(standardScripts);

  int minWidth = int(QApplication::desktop()->width()/5.00);
  if(minWidth>235) minWidth = 235;  

  standardScripts->setMaximumWidth(minWidth);
  customScripts->setMaximumWidth(minWidth);

  results = new resultsData(mainData, mainData->getDir("working") + "/LOGS/" + "2dx_initialization.results", mainData->getDir("working"), this);

  QWidget *dirWidget = setupDirectoryView(mainData->getDir("project"), mainData->getDir("working") + "/2dx_merge_dirfile.dat");

  logViewer = new LogViewer("Standard Output",NULL);

  connect(standardScripts,SIGNAL(standardOut(const QStringList &)),logViewer,SLOT(insertText(const QStringList &)));
  connect(standardScripts,SIGNAL(standardError(const QByteArray &)),logViewer,SLOT(insertError(const QByteArray &)));
  connect(customScripts,SIGNAL(standardOut(const QStringList &)),logViewer,SLOT(insertText(const QStringList &)));
  connect(customScripts,SIGNAL(standardError(const QByteArray &)),logViewer,SLOT(insertError(const QByteArray &)));
  connect(standardScripts,SIGNAL(scriptLaunched()),logViewer,SLOT(clear()));
  connect(customScripts,SIGNAL(scriptLaunched()),logViewer,SLOT(clear()));

  verbosityControl = new levelGroup(mainData, 4,
      QStringList()<<"Logfile - Silent"<<"Logfile - Low Verbosity"<<"Logfile - Moderate Verbosity"<<"Logfile - Highest Verbosity",
      QStringList()<<"Update only on script completion."<<"Low Verbosity Output"<<"Moderate Verbosity Output"<<"Highest Verbosity Output",
      QStringList()<<"gbAqua"<<"gbBlue"<<"gbOrange"<<"gbRed");

  verbosityControl->setLevel(1);
  
  viewContainer *logWindow = new viewContainer("Logfile - Low Verbosity",viewContainer::data,NULL,viewContainer::grey);
  connect(logWindow,SIGNAL(doubleClicked()),this,SLOT(launchLogBrowser()));
  logWindow->setHeaderWidget(verbosityControl);
  logWindow->addWidget(logViewer);
  connect(verbosityControl, SIGNAL(titleChanged(const QString &)), logWindow, SLOT(setText(const QString &)));
  connect(verbosityControl, SIGNAL(levelChanged(int)), logViewer, SLOT(load(int)));
  connect(verbosityControl, SIGNAL(levelChanged(int)), standardScripts, SLOT(setVerbosity(int)));
  connect(verbosityControl, SIGNAL(levelChanged(int)), customScripts, SLOT(setVerbosity(int)));

  parameterContainer = new viewContainer("Processing Data -- Standard", viewContainer::data);

  localParameters = new resizeableStackedWidget(parameterContainer);
  manuals = new QStackedWidget;
  manuals->hide();
  parameters = new confInterface(mainData,"");
  connect(results,SIGNAL(saved(bool)),parameters,SLOT(load()));

  QWidget *parametersWidget = new QWidget;
  QVBoxLayout *parameterLayout = new QVBoxLayout;
  parameterLayout->setMargin(0);
  parameterLayout->setSpacing(0);
  parametersWidget->setLayout(parameterLayout);
  parameterLayout->addWidget(localParameters);
  parameterLayout->addWidget(parameters);
  parameterLayout->setStretchFactor(localParameters,1);
  parameterLayout->setStretchFactor(parameters,100);
  

  parameterContainer->addSplitterWidget(manuals);
  parameterContainer->addScrollWidget(parametersWidget,true);
//  parameterContainer->addScrollWidget(parameters);

  userLevelButtons = new levelGroup(mainData,2,QStringList()<<"Processing Data -- Standard"<<"Processing Data -- Advanced",
                                                           QStringList()<<"Simplified Parameter List"<<"Full Parameter List",
                                                           QStringList()<<"gbAqua"<<"gbRed");
  parameterContainer->setHeaderWidget(userLevelButtons);
  connect(userLevelButtons,SIGNAL(titleChanged(const QString &)),parameterContainer,SLOT(setText(const QString &)));
  connect(userLevelButtons,SIGNAL(levelChanged(int)),parameters,SLOT(setSelectionUserLevel(int)));

  connect(standardScripts,SIGNAL(currentScriptChanged(QModelIndex)),this,SLOT(standardScriptChanged(QModelIndex)));
  connect(customScripts,SIGNAL(currentScriptChanged(QModelIndex)),this,SLOT(customScriptChanged(QModelIndex)));

  //userData = new confData(QDir::homePath() + "/.2dx/" + "2dx_merge-user.cfg");
  userData = new confData(QDir::homePath() + "/.2dx/" + "2dx_merge-user.cfg", mainData->getDir("config") + "/" + "2dx_merge-user.cfg");
  userData->save();
  mainData->setUserConf(userData);

  int fontSize = mainData->userConf()->get("fontSize","value").toInt();
  if(fontSize!=0)
  {
    QFont mainFont(QApplication::font());
    mainFont.setPointSize(fontSize);
    QApplication::setFont(mainFont);
    updateFontInfo();
  }
  else
  {
    QFont font = QApplication::font();
    mainData->userConf()->set("fontSize",QString::number(font.pointSize()));
    mainData->userConf()->save();
    QApplication::setFont(font);
    updateFontInfo();
  }

  container = new viewContainer(mainData->getDir("working"),viewContainer::data,this,viewContainer::grey);
  container->addSplitterWidget(dirWidget);
  container->addWidget(parameterContainer,true);
//  container->addSplitterWidget(confWidget);
  container->addWidget(logWindow,true);
  container->setMinimumWidth(0);
//  container->setMaximumWidth(1024);
  levelGroup *maximizeControl = new levelGroup(mainData, 4,
                                                           QStringList()<<"Restore"<<"Maximize Project View"<<"Maximize Parameters View"<<"Maximize LogBrowser View",
                                                           QStringList()<<"Restore"<<"Maximize Project Window"<<"Maximize Parameters Window"<<"Maximize Log Browser",
                                                           QStringList()<<"gbAqua"<<"gbBlue"<<"gbGreen"<<"gbOrange");


  container->setHeaderWidget(maximizeControl, Qt::AlignRight);
  container->saveSplitterState();
  connect(maximizeControl,SIGNAL(levelChanged(int)),this,SLOT(maximizeWindow(int)));

  viewContainer *resultsContainer = new viewContainer("Results",viewContainer::data,this,viewContainer::grey);
  resultsView = new resultsModule(mainData,results, resultsModule::results, mainData->getDir("project"));
  resultsContainer->addWidget(resultsView);
  resultsContainer->setMinimumSize(QSize(200,100));

  viewContainer *imagesContainer = new viewContainer("Images",viewContainer::data,this,viewContainer::grey);
  connect(imagesContainer,SIGNAL(doubleClicked()), this, SLOT(launchFileBrowser()));
  resultsModule *imagesView = new resultsModule(mainData,results, resultsModule::images, mainData->getDir("project"));
  imagesContainer->addWidget(imagesView);
  imagesContainer->setMinimumSize(QSize(200,100));

  levelGroup *imageLevelButtons = new levelGroup(mainData,2,QStringList()<<"All Images"<<"Important Images", QStringList()<<"Show all images"<<"Show only important images",QStringList()<<"gbAqua"<<"gbRed");
  levelGroup *imageNamesButtons = new levelGroup(mainData,2,QStringList()<<"Nicknames"<<"Filenames", QStringList()<<"Show Nicknames"<<"Show File Names",QStringList()<<"gbOrange"<<"gbPurple");
  imagesContainer->setHeaderWidget(imageLevelButtons,Qt::AlignLeft);
  imagesContainer->setHeaderWidget(imageNamesButtons,Qt::AlignRight);
  connect(imageLevelButtons,SIGNAL(levelChanged(int)),imagesView,SLOT(setImportant(int)));
  connect(imageNamesButtons,SIGNAL(levelChanged(int)),imagesView,SLOT(setShowFilenames(int)));
  
  
  QSplitter *resultsSplitter = new QSplitter(Qt::Vertical,this);
  resultsSplitter->addWidget(resultsContainer);
  resultsSplitter->addWidget(imagesContainer);

  viewContainer *previewContainer = new viewContainer("Image Preview",viewContainer::image,this,viewContainer::grey);
  preview = new imagePreview(mainData,"",false,menuBar(),previewContainer);
  previewContainer->addWidget(preview);
//  preview->toggleInfo();

  connect(imagesView,SIGNAL(resultChanged(const QString &)),preview,SLOT(setImage(const QString &)));
  connect(dirModel,SIGNAL(currentImage(const QString&)),preview,SLOT(setImage(const QString&)));
  connect(dirModel,SIGNAL(reloading()),this,SLOT(reload()));

  QWidget *footerWidget = setupFooter();

  centerRightSplitter = new QSplitter(this);
  centerRightSplitter->setOrientation(Qt::Horizontal);
  centerRightSplitter->setHandleWidth(4);
  centerRightSplitter->addWidget(container);
  centerRightSplitter->addWidget(resultsSplitter);


  layout->addWidget(headerWidget,0,0,1,2);
  layout->addWidget(standardScriptWidget,1,0,1,1);
  layout->addWidget(customScriptWidget,2,0,1,1);
  layout->addWidget(previewContainer,3,0,1,1);
  layout->addWidget(centerRightSplitter,1,1,3,1);
  layout->addWidget(footerWidget,4,0,1,2);
  layout->setColumnStretch(1,3);
  layout->setRowStretch(1,3);
  layout->setRowStretch(2,3);
//  layout->setColumnStretch(2,1);
//  resize(1024,743);

  about = new aboutWindow(mainData,this);
  about->hide();

	setupActions();
  album = NULL;
  
  importCount = 0;

  verbosityControl->setLevel(1);
  standardScripts->initialize();

}

QWidget *mainWindow::setupScriptContainer(QWidget *widget, const QString &title)
{
  viewContainer *container = new viewContainer(title,this,viewContainer::grey);

  container->addWidget(widget);
  container->resize(200,20);
  container->setMinimumWidth(200);
//  container->setMaximumWidth(200);
  return container;
}

QWidget *mainWindow::setupHeader()
{
  QWidget *header = new QWidget(this);
  header->setFixedHeight(93);
  QHBoxLayout *layout = new QHBoxLayout;
  header->setLayout(layout);

  header->setAutoFillBackground(true);
  QPalette pal(palette());
  QLinearGradient grad(QPoint(0,0),QPoint(0,header->height()));
//  grad.setColorAt(1,QColor(113,113,114));
//  grad.setColorAt(0,QColor(172,172,171));

  int l = 0;
  
  grad.setColorAt(0.0,QColor(69+l,79+l,79+l));
  grad.setColorAt(0.333,QColor(45+l,45+l,35+l));
  grad.setColorAt(0.55,QColor(20+l,23+l,20+l));
  grad.setColorAt(1.0,QColor(33+l,30+l,33+l));


  pal.setBrush(QPalette::Background,QBrush(grad));
  header->setPalette(pal);

  progressBar = new scriptProgress(this);
  progressBar->setText("2dx_Merge");
  progressBar->setMaximum(100);
  progressBar->setProgressType(scriptProgress::ticks);

  playButton = new graphicalButton(mainData->getIcon("play"),this);
  playButton->setToolTip("Run current script");
  playButton->setCheckable(true);
  connect(playButton,SIGNAL(clicked(bool)),this,SIGNAL(execute(bool)));

  saveButton = new graphicalButton(mainData->getIcon("saveDark"),this);
  saveButton->setToolTip("Save");
  saveButton->setChecked(false);
  saveButton->setCheckable(false);
  connect(saveButton,SIGNAL(clicked(bool)),mainData,SLOT(save()));

  updateButton = new graphicalButton(mainData->getIcon("refresh"));
  updateButton->setToolTip("Refresh directory view");
  updateButton->setCheckable(false);
  connect(updateButton,SIGNAL(clicked(bool)),this,SLOT(reload()));

  manualButton = new graphicalButton(mainData->getIcon("help"));
  manualButton->setToolTip("Show/Hide script manual");
  manualButton->setCheckable(true);
  manualButton->setChecked(false);
  connect(manualButton,SIGNAL(toggled(bool)),this,SLOT(showManual(bool)));
  

  layout->insertStretch(0,16);
  layout->addWidget(saveButton);
  layout->insertStretch(2,10);
  layout->addWidget(playButton);
  layout->insertStretch(4,16);
  layout->addWidget(progressBar);
  layout->insertStretch(6,16);
  layout->addWidget(manualButton);
  layout->insertStretch(8,10);
  layout->addWidget(updateButton);
  layout->insertStretch(10,16);
   
  layout->setAlignment(Qt::AlignCenter);
  return header;
}

QWidget *mainWindow::setupDirectoryView(const QDir &dir, const QString &savePath)
{
  if(!dir.exists())
  {
    dirView = NULL;
    return NULL;
  }

  QString projectDir = dir.canonicalPath();
  dirModel = new projectModel(mainData, projectDir,mainData->getDir("working") + "/config/" + "projectMenu.inf",this);
  dirModel->setResultsFile(results);

  if(!savePath.isEmpty())
  {
    dirModel->setSaveName(savePath);
    dirModel->loadSelection();
  }

  sortModel = new QSortFilterProxyModel(this);
  sortModel->setSourceModel(dirModel);
  sortModel->setDynamicSortFilter(true);
  sortModel->setSortRole(projectModel::SortRole);

  dirView = new QTreeView(this);
  dirView->setModel(sortModel);
  dirView->setSelectionMode(QAbstractItemView::ExtendedSelection);
  dirView->setSortingEnabled(true);
  dirView->setAllColumnsShowFocus(true);
  loadProjectState();
  connect(dirView->header(),SIGNAL(sectionMoved(int,int,int)),this,SLOT(saveProjectState()));
  connect(dirView->header(),SIGNAL(sectionResized(int,int,int)),this,SLOT(saveProjectState()));
  connect(dirView->header(),SIGNAL(sortIndicatorChanged(int,Qt::SortOrder)),this,SLOT(saveProjectState()));  
  
  connect(dirView->selectionModel(),SIGNAL(currentRowChanged(const QModelIndex&,const QModelIndex&)),dirModel,SLOT(currentRowChanged(const QModelIndex&,const QModelIndex&)));  

  projectDelegate *delegate = new projectDelegate(mainData);

//  dirView->setItemDelegate(new projectDelegate(mainData));
  QItemDelegate *defaultDelegate = new QItemDelegate(mainData);
  defaultDelegate->setClipping(true);
  dirView->setAlternatingRowColors(true);  
  dirView->setItemDelegateForColumn(0,defaultDelegate);
  dirView->setItemDelegateForColumn(1,defaultDelegate);
  for(int i=2;i<sortModel->columnCount();i++)
    dirView->setItemDelegateForColumn(i, delegate);


  //Right-Click Menu
  QAction *addSelectionAction;
  addSelectionAction = new QAction("add to selection", dirView);
  dirView->addAction(addSelectionAction);
  connect(addSelectionAction,SIGNAL(triggered(bool)),this,SLOT(extendSelection()));
  QAction *removeSelectionAction;
  removeSelectionAction = new QAction("remove from selection", dirView);
  dirView->addAction(removeSelectionAction);
  connect(removeSelectionAction,SIGNAL(triggered(bool)),this,SLOT(reduceSelection()));
  dirView->setContextMenuPolicy(Qt::ActionsContextMenu);



  QAction *action;

  QSignalMapper *mapper = new QSignalMapper(this);


  for(int i=0;i<dirModel->columnCount();i++)
  {
    bool visible = dirModel->getColumnProperty(i,"visible").toBool();
    dirView->setColumnHidden(i,!visible);

    action = new QAction(dirModel->getColumnProperty(i,"shortname").toString(),dirView);
    action->setCheckable(true);

    if(visible) action->setChecked(true);
    connect(action,SIGNAL(triggered(bool)),mapper,SLOT(map()));
    mapper->setMapping(action,i);

    dirView->header()->addAction(action);
  }
  connect(mapper,SIGNAL(mapped(int)),this,SLOT(columnActivated(int)));

  dirView->header()->setContextMenuPolicy(Qt::ActionsContextMenu);
  dirView->expandAll();
  int width = 0;
  for(int i = 0; i<dirView->model()->columnCount(); i++)
  {
    dirView->resizeColumnToContents(i);
    width += dirView->columnWidth(i);
  }
//  dirView->resize((int)width*(1.05),300);
  connect(dirView,SIGNAL(doubleClicked(const QModelIndex&)),dirModel,SLOT(itemActivated(const QModelIndex&)));
//  container->resize(width,dirView->height());
  return dirView;
}

void mainWindow::setupActions()
{
  QMenu *fileMenu = new QMenu("File");

  QAction *openAction = new QAction("Open",this);
  openAction->setShortcut(tr("Ctrl+O"));
  connect(openAction,SIGNAL(triggered()),this,SLOT(open()));
  fileMenu->addAction(openAction);
  
  QAction *saveAction = new QAction("Save",this);
  saveAction->setShortcut(tr("Ctrl+S"));
  connect(saveAction,SIGNAL(triggered()),mainData,SLOT(save()));
  fileMenu->addAction(saveAction);

  QAction *refreshAction = new QAction("Refresh Results",this);
  refreshAction->setShortcut(tr("Ctrl+Shift+r"));
  connect(refreshAction,SIGNAL(triggered()),this,SLOT(reload()));
  fileMenu->addAction(refreshAction);

  QAction *importAction = new QAction("Import Images...",this);
  connect(importAction,SIGNAL(triggered()),this,SLOT(import()));
  fileMenu->addAction(importAction);
  
/*
  QAction *autoImportAction = new QAction("Auto Import... (in development)",this);
  connect(autoImportAction,SIGNAL(triggered()),this,SLOT(autoImport()));
  fileMenu->addAction(autoImportAction);
*/

/*  QAction *removeImagesAction = new QAction("Remove Selected (Experimental/Unstable ****Careful****)...",this);
  connect(removeImagesAction,SIGNAL(triggered()),dirModel,SLOT(removeSelected()));
  fileMenu->addAction(removeImagesAction);
*/
  QAction *closeAction = new QAction("Quit",this);
  closeAction->setShortcut(tr("Ctrl+Q"));
  connect(closeAction,SIGNAL(triggered()),qApp,SLOT(closeAllWindows()));
  fileMenu->addAction(closeAction);

  QMenu *editMenu = new QMenu("Edit");
 
  QAction *increaseFontAction = new QAction("Increase Font Size",this);
  increaseFontAction->setShortcut(tr("]"));
  connect(increaseFontAction,SIGNAL(triggered()),this,SLOT(increaseFontSize()));
  editMenu->addAction(increaseFontAction);

  QAction *decreaseFontAction = new QAction("Decrease Font Size",this);
  decreaseFontAction->setShortcut(tr("["));
  connect(decreaseFontAction,SIGNAL(triggered()),this,SLOT(decreaseFontSize()));
  editMenu->addAction(decreaseFontAction);

  QMenu *optionMenu = new QMenu("Options");

  QAction *openPreferencesAction = new QAction("Preferences",this);
  connect(openPreferencesAction,SIGNAL(triggered()),this,SLOT(editHelperConf()));
  optionMenu->addAction(openPreferencesAction);

  QAction *showUpdatesAction = new QAction("Update...",this);
  connect(showUpdatesAction,SIGNAL(triggered()),updates,SLOT(show()));
  optionMenu->addAction(showUpdatesAction);
  
  QAction *showAboutAction = new QAction("About",this);
  connect(showAboutAction,SIGNAL(triggered()),about,SLOT(show()));
	optionMenu->addAction(showAboutAction);

  QMenu *viewMenu = new QMenu("View");
  
  QAction *showSelectedAction = new QAction("Show Only Selected Directories",this);
  showSelectedAction->setShortcut(tr("Ctrl+D"));
  showSelectedAction->setCheckable(true);
  connect(showSelectedAction,SIGNAL(toggled(bool)),this,SLOT(showSelected(bool)));
  viewMenu->addAction(showSelectedAction);
  
  QAction *viewAlbum = new QAction("Show Reconstruction Album",this);
  viewAlbum->setShortcut(tr("Ctrl+Shift+A"));
  connect(viewAlbum,SIGNAL(triggered()),this,SLOT(showAlbum()));
  viewMenu->addAction(viewAlbum);

//  QMenu *projectMenu = new QMenu("Project");
  
  QMenu *selectMenu = new QMenu("Select");
//  projectMenu->addMenu(selectMenu);
  
  QAction *selectAllAction = new QAction("Select All",this);
  selectAllAction->setShortcut(tr("Ctrl+A"));
  selectMenu->addAction(selectAllAction);
  connect(selectAllAction,SIGNAL(triggered()),dirModel,SLOT(selectAll()));
  
  QAction *invertSelectedAction = new QAction("Invert Selection",this);
  invertSelectedAction->setShortcut(tr("Ctrl+I"));
  selectMenu->addAction(invertSelectedAction);
  connect(invertSelectedAction,SIGNAL(triggered()),dirModel,SLOT(invertSelection()));
  
  QAction *saveDirectorySelectionAction = new QAction("Save Selection As...",this);
  connect(saveDirectorySelectionAction,SIGNAL(triggered()),this,SLOT(saveDirectorySelection()));
  selectMenu->addAction(saveDirectorySelectionAction);  
  
  QAction *loadDirectorySelectionAction = new QAction("Load Selection...",this);
  connect(loadDirectorySelectionAction,SIGNAL(triggered()),this,SLOT(loadDirectorySelection()));
  selectMenu->addAction(loadDirectorySelectionAction);  
 
  menuBar()->addMenu(fileMenu);
  menuBar()->addMenu(editMenu);
  menuBar()->addMenu(optionMenu);
  menuBar()->addMenu(viewMenu);
  menuBar()->addMenu(selectMenu);
}

QWidget *mainWindow::setupConfView(confData *data)
{
  confModel *model = new confModel(data,this);
  
  QTableView *confView = new QTableView;
  confView->setModel(model);
  confView->setItemDelegate(new confDelegate(data));
  confView->setGridStyle(Qt::NoPen);
  confView->resizeRowsToContents();
  confView->setAlternatingRowColors(true);
  confView->horizontalHeader()->hide();
  confView->verticalHeader()->hide();
  confView->setSelectionMode(QAbstractItemView::NoSelection);
  int width = 0;
  for(int i = 0; i<confView->model()->columnCount(); i++)
  {
    confView->resizeColumnToContents(i);
    width += confView->columnWidth(i);
  }
  confView->setFrameStyle(QFrame::Panel | QFrame::Sunken);
  return confView;
}


bool mainWindow::setupIcons(confData *data, const QDir &directory)
{
  if(!directory.exists()) return false;
  QString entry, label, type;
  QHash<QString,QIcon *> icons;
  foreach(entry, directory.entryList(QStringList() << "*", QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted))
  {
    if(entry.contains(QRegExp(".*\\-..\\.png$")))
    {
      label = entry.section(".png",0,0).section("-",0,0).trimmed().toLower();
      type = entry.section(".png",0,0).section("-",1,1).trimmed().toLower();
      if(icons[label] == NULL) icons.insert(label,new QIcon);
      if(type == "ad") icons[label]->addPixmap(directory.canonicalPath() + "/" + entry,QIcon::Active,QIcon::On);
      if(type == "id") icons[label]->addPixmap(directory.canonicalPath() + "/" + entry,QIcon::Normal,QIcon::On);
      if(type == "au") icons[label]->addPixmap(directory.canonicalPath() + "/" + entry,QIcon::Active,QIcon::Off);
      if(type == "iu") icons[label]->addPixmap(directory.canonicalPath() + "/" + entry,QIcon::Normal,QIcon::Off);
    }
    else if(entry.contains(".png",Qt::CaseInsensitive))
    {
      label = entry.section(".png",0,0).trimmed().toLower();
      icons.insert(label,new QIcon);
      icons[label]->addPixmap(directory.canonicalPath() + "/" + entry);
    }
  }

  QHashIterator<QString,QIcon*> it(icons);
  while(it.hasNext())
  {
    it.next();
    data->addIcon(it.key(),it.value());
  }
  return true;
}

void mainWindow::scriptChanged(scriptModule *module, QModelIndex index)
{
//  container->saveSplitterState(1);
  int uid = index.data(Qt::UserRole).toUInt();
  progressBar->setText(module->title(index));

  if(localIndex[uid] == 0 && module->conf(index)->size()!=0)
  {
    confInterface *local = new confInterface(module->conf(index), "");
    localIndex[uid] = localParameters->addWidget(local) + 1;
    if(localParameters->widget(localIndex[uid] - 1) == NULL) cerr<<"Something's very wrong here."<<endl;
//    connect(userLevelButtons,SIGNAL(levelChanged(int)),local,SLOT(setSelectionUserLevel(int)));
  }

  if(manualIndex[uid] == 0 && !module->conf(index)->manual().isEmpty())
    manualIndex[uid] = manuals->addWidget(new confManual(mainData, module->conf(index))) + 1;

  if(localIndex[uid] - 1 < 0)
    localParameters->hide();
  else
  {
    localParameters->show();
    localParameters->setCurrentIndex(localIndex[uid] - 1);
  }

  manuals->setCurrentIndex(manualIndex[uid] - 1);

  parameters->select(module->displayedVariables(index));
  if(verbosityControl->level() != 0)
		logViewer->loadLogFile(module->logFile(index));
  else
		logViewer->clear();
  results->load(module->resultsFile(index));
//  container->restoreSplitterState(1);
}

void mainWindow::standardScriptChanged(QModelIndex index)
{
  scriptChanged(standardScripts, index);
}

void mainWindow::customScriptChanged(QModelIndex index)
{
  scriptChanged(customScripts, index);
}

void mainWindow::scriptLaunched(scriptModule * /*module*/, QModelIndex /*index*/)
{

}

QWidget *mainWindow::setupFooter()
{
  QWidget *footer = new QWidget(this);
  footer->setFixedHeight(41);
  QHBoxLayout *layout = new QHBoxLayout;
  footer->setLayout(layout);

  graphicalButton *infoButton = new graphicalButton(mainData->getIcon("info"),this);
  infoButton->setCheckable(true);
  infoButton->setToolTip("Show/Hide image header information");
//  infoButton->setChecked(true);
  connect(infoButton,SIGNAL(clicked(bool)),preview,SLOT(toggleInfo()));

  graphicalButton *dryRunButton = new graphicalButton(mainData->getIcon("dryRun"),this);
  dryRunButton->setCheckable(true);
  dryRunButton->setChecked(false);
  dryRunButton->setToolTip("Perform \"Dry Run\" without commiting changes to configuration files.");
  connect(dryRunButton,SIGNAL(clicked(bool)),results,SLOT(setDryRunMode(bool)));

  QSignalMapper *mapper = new QSignalMapper(this);

  graphicalButton *webHelp = new graphicalButton(mainData->getIcon("manual"),this);
  webHelp->setToolTip("View online help");
  webHelp->setCheckable(false);
  connect(webHelp,SIGNAL(clicked()),mapper,SLOT(map()));
  mapper->setMapping(webHelp,mainData->getURL("help"));

  graphicalButton *bugReport = new graphicalButton(mainData->getIcon("Bug"),this);
  bugReport->setToolTip("Report Issue/Bug");
  bugReport->setCheckable(false);
  connect(bugReport,SIGNAL(clicked()),mapper,SLOT(map()));
  mapper->setMapping(bugReport,mainData->getURL("bugReport"));

  connect(mapper,SIGNAL(mapped(const QString &)),this,SLOT(openURL(const QString &)));

  layout->addWidget(infoButton);
  layout->addStretch(2);
  layout->addWidget(webHelp);
  layout->addWidget(bugReport);
  layout->addWidget(dryRunButton);

  footer->setAutoFillBackground(true);
  QPalette pal(palette());
  QLinearGradient grad(QPoint(0,0),QPoint(0,footer->height()));
//  grad.setColorAt(1,QColor(113,113,114));
//  grad.setColorAt(0,QColor(172,172,171));

  int l = 0;
  
  grad.setColorAt(0.0,QColor(69+l,79+l,79+l));
  grad.setColorAt(0.333,QColor(45+l,45+l,35+l));
  grad.setColorAt(0.55,QColor(20+l,23+l,20+l));
  grad.setColorAt(1.0,QColor(33+l,30+l,33+l));
  
  pal.setBrush(QPalette::Background,QBrush(grad));
  footer->setPalette(pal);
  return footer;
}

void mainWindow::setSaveState(bool state)
{
  if(state == false)
  {
    saveButton->setChecked(false);
    saveButton->setCheckable(false);
  }
  else
  {
    saveButton->setCheckable(true);
    saveButton->setChecked(true);
  }
}

void mainWindow::extendSelection()
{
    modifySelection(true);
}

void mainWindow::reduceSelection()
{
    modifySelection(false);
}



void mainWindow::modifySelection(bool select)
{
    QModelIndex i;
    QModelIndexList selection  = dirView->selectionModel()->selectedIndexes();

    if(select)
    {
        foreach(i, selection)
        {
                dirModel->itemSelected(sortModel->mapToSource(i));
        }

    }
    else
    {
        foreach(i, selection)
        {
                dirModel->itemDeselected(sortModel->mapToSource(i));
        }
    }
}

void mainWindow::columnActivated(int i)
{
  dirModel->setColumnProperty(i,"visible",dirModel->getColumnProperty(i,"visible").toBool()^true);
  dirModel->saveColumns();
  dirView->setColumnHidden(i,!dirModel->getColumnProperty(i,"visible").toBool());
}

void mainWindow::scriptCompleted(scriptModule *module, QModelIndex index)
{
//  cerr<<"Script completed"<<endl;
  results->load(module->resultsFile(index));
  dirModel->maskResults();
  results->save();
  resultsView->load();
  playButton->setChecked(false);
}

void mainWindow::reload()
{
  results->load();
  dirModel->maskResults();
  updateModel();
  if(album!=NULL) 
    album->reload();
//  results->save();
//  resultsView->load();
}

void mainWindow::standardScriptCompleted(QModelIndex index)
{
//  cerr<<"Standard ";
  scriptCompleted(standardScripts,index);
}

void mainWindow::customScriptCompleted(QModelIndex index)
{
//  cerr<<"Custom ";
  scriptCompleted(customScripts,index);
}

void mainWindow::maximizeWindow(int option)
{
  container->maximizeWindow(option - 1);
  if(option==0) 
  {
    centerRightSplitter->setSizes(QList<int>()<<1<<1);

  }
  else 
    centerRightSplitter->setSizes(QList<int>()<<1<<0);  
}

void mainWindow::initializeDirectory()
{

}

bool mainWindow::createDir(const QString &dir)
{
  QDir directory(dir);
  if(!directory.exists())
    return directory.mkdir(dir);
  return false;
}

void mainWindow::launchAlbum(const QString &path)
{

  if(album==NULL && dirModel!=NULL)
  {
    album = new imageAlbum(dirModel);
    connect(dirView->selectionModel(),SIGNAL(currentRowChanged(const QModelIndex&,const QModelIndex&)),album,SLOT(currentSelectionChanged(const QModelIndex&,const QModelIndex&)));      
//    album->setModel(sortModel);
//    album->setSelectionModel(dirView->selectionModel());
  }
}

void mainWindow::launchFileBrowser()
{
    QString path = QDir::toNativeSeparators(mainData->getDir("working"));
    QDesktopServices::openUrl(QUrl("file:///" + path));
}

void mainWindow::importFiles(const QHash<QString, QHash<QString,QString> > &imageList)
{
  QHashIterator<QString, QHash<QString,QString> > it(imageList);
  importCount=imageList.size();
  while(it.hasNext())
  {
    it.next();
    qDebug()<<"Importing File: "<<it.key();
    importFile(it.key(),it.value());
  }
}

void mainWindow::importFile(const QString &file, const QHash<QString,QString> &imageCodes)
{
  QHashIterator<QString,QString> it(imageCodes);
  QString fileName = file;
  QString pC = imageCodes["protein_code"];
  QString tiltAngle = imageCodes["tilt_angle"];
  QString frame = imageCodes["frame_number"];
  QString subID = imageCodes["sub_image_number"];
  QString ext = QFileInfo(file).completeSuffix();
  
  QDir tiltDir(mainData->getDir("project") + "/" + pC + tiltAngle);
  QString newFile = pC+tiltAngle+frame+subID;
  QString tiltDirectory = pC + tiltAngle;
  QString tiltConfigLocation = mainData->getDir("project") + "/" + tiltDirectory + "/2dx_master.cfg";
  if(!tiltDir.exists()) 
  {
    qDebug()<<pC+tiltAngle<<" does not exist...creating.";
    tiltDir.setPath(mainData->getDir("project"));
    tiltDir.mkdir(tiltDirectory);
    //confData tiltData(tiltConfigLocation);
    confData tiltData(mainData->getDir("project") + "/" + tiltDirectory + "/2dx_master.cfg", mainData->getDir("project") + "/2dx_master.cfg");
    tiltData.save();
    tiltData.setSymLink("../2dx_master.cfg", mainData->getDir("project") + "/" + tiltDirectory + "/2dx_master.cfg");
  }

  tiltDir.setPath(mainData->getDir("project") + "/" + tiltDirectory);
  tiltDir.mkdir(newFile);

  QFile::copy(fileName,tiltDir.path() + "/" + newFile + "/" + newFile + '.' + ext);
  QString newFilePath = tiltDir.path() + "/" + newFile;
  QString newFileConfigPath = newFilePath + "/2dx_image.cfg";
  if(!QFileInfo(newFileConfigPath).exists())
  {
      qDebug() << "creating " << newFileConfigPath;
      if(!QFile::copy(tiltConfigLocation, newFileConfigPath))
          qDebug() << "Failed to copy " << tiltConfigLocation << " to " << newFileConfigPath;
  }
  importProcess.start(mainData->getApp("2dx_image") + " " + newFilePath + " " + "\"2dx_initialize\"");
  importProcess.waitForFinished(8*60*60*1000);
}

void mainWindow::importFinished()
{
  importCount--;
  if(importCount<=0) updateModel();
}

void mainWindow::updateModel()
{
  dirModel->reload();

  for(int i=0;i<dirModel->columnCount();i++)
  {
    bool visible = dirModel->getColumnProperty(i,"visible").toBool();
    dirView->setColumnHidden(i,!visible);
  }

  dirView->expandAll();
  int width = 0;
  for(int i = 0; i<dirView->model()->columnCount(); i++)
  {
    dirView->resizeColumnToContents(i);
    width += dirView->columnWidth(i);
  }
  dirModel->loadSelection();
  dirView->expandAll();  
}

void mainWindow::import()
{
  QStringList fileList = QFileDialog::getOpenFileNames(NULL,"Choose image files to add",mainData->getDir("project"),"Images (*.tif *.mrc)");
  if(fileList.isEmpty()) return;
  QStringList importList;
//  foreach(file, fileList)
//  {
//    if(QFileInfo(file).isDir()) 
//      importList<<subDirFiles(file);
//    else
//      importList<<file;
      
//  }
  importTool *import = new importTool(mainData,fileList);
  connect(import,SIGNAL(acceptedImages( const QHash< QString, QHash < QString , QString > >& )),this,SLOT(importFiles(const QHash<QString, QHash<QString,QString> > &)));  
}

void mainWindow::autoImport()
{
  if(!autoImportMonitor)
  {
    QString dir = QFileDialog::getExistingDirectory(NULL,"Choose import directory",mainData->getDir("project"));
    autoImportMonitor = new autoImportTool(mainData,dir);
  }
  else
    autoImportMonitor->show();
}

void mainWindow::open()
{
  QProcess::startDetached(mainData->getApp("this"));
}

void mainWindow::openURL(const QString &url)
{
  QProcess::startDetached(mainData->getApp("webBrowser") + " " + url);
}

void mainWindow::showManual(bool show)
{
  if(show) {parameterContainer->restoreSplitterState(0); manuals->show();}
  else {parameterContainer->saveSplitterState(0); parameterContainer->maximizeWindow(1); manuals->hide();}
}

void mainWindow::increaseFontSize()
{
  QFont font = QApplication::font();
  font.setPointSize(font.pointSize()+1);
  mainData->userConf()->set("fontSize",QString::number(font.pointSize()));
  mainData->userConf()->save();
  QApplication::setFont(font);
  updateFontInfo();
}

void mainWindow::decreaseFontSize()
{
  QFont font = QApplication::font();
  font.setPointSize(font.pointSize()-1);
  mainData->userConf()->set("fontSize",QString::number(font.pointSize()));
  mainData->userConf()->save();
  QApplication::setFont(font);
  updateFontInfo();
}

void mainWindow::updateFontInfo()
{
  parameters->updateFontInfo();
  logViewer->updateFontInfo();
  //  emit fontInfoUpdated();
}

void mainWindow::saveDirectorySelection()
{
  QString saveName = QFileDialog::getSaveFileName(this,"Save Selection As...",mainData->getDir("working") + "/2dx_merge_dirfile.dat");
  if(QFileInfo(saveName).exists()) QFile::remove(saveName);
  QFile::copy(mainData->getDir("working") + "/2dx_merge_dirfile.dat",saveName);
}

void mainWindow::loadDirectorySelection()
{
  QString loadName = QFileDialog::getOpenFileName(this,"Save Selection As...",mainData->getDir("working") + "/2dx_merge_dirfile.dat");
  dirModel->loadSelection(loadName);
}

void mainWindow::showSelected(bool enable)
{
  sortModel->setFilterRole(Qt::CheckStateRole);
  sortModel->setDynamicSortFilter(true);
  if(enable)
  {
    sortModel->setFilterRegExp((QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked)));
  }
  else
  {
    sortModel->setFilterRegExp(".*");
  }
}

void mainWindow::showAlbum(bool show)
{
  if(album==NULL)
    launchAlbum(mainData->getDir("project"));

  album->setHidden(!show);
}

void mainWindow::editHelperConf()
{
  new confEditor(mainData->getSubConf("appConf")); 
}

void mainWindow::launchLogBrowser()
{
  QProcess::startDetached(mainData->getApp("logBrowser") + " " +logViewer->getLogFile());
}



void mainWindow::loadProjectState()
{
  QString projectHeaderState = mainData->getDir("working") + "/config/projectHeaderState.dat";
  if(QFileInfo(projectHeaderState).exists())
  {
    QFile f(projectHeaderState);
    if(!f.open(QIODevice::ReadOnly)) return;
    dirView->header()->restoreState(f.readAll());
    f.close();
  }
}

void mainWindow::saveProjectState()
{
  QString projectHeaderState = mainData->getDir("working") + "/config/projectHeaderState.dat";
  if(!projectHeaderState.isEmpty())
  {
    QFile f(projectHeaderState);
    if(!f.open(QIODevice::WriteOnly)) return;
    f.write(dirView->header()->saveState());
    f.close();
  }
}
