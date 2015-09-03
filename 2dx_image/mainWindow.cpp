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
#include <iostream>
using namespace std;

mainWindow::mainWindow(char *dirArg)
{
  bool initialize = false;
  QString workingDir;

  m_do_autosave = true;
  
 QDir applicationDir, configDir;

 #ifdef Q_OS_MAC
  applicationDir = QDir(QApplication::applicationDirPath() + "/../../../");
 #else
  applicationDir = QDir(QApplication::applicationDirPath());
 #endif

  createDir(QDir::homePath() + "/.2dx/");
  QString userPath = QDir::homePath() + "/.2dx/";

  configDir = QDir(applicationDir.absolutePath() + "/../" + "config/");

  // read user config to get the latest image dir
  userData = new confData(QDir::homePath() + "/.2dx/" + "2dx_image-user.cfg", configDir.absolutePath() + "/" + "2dx_image-user.cfg");
  userData->save();
  
  if(dirArg==NULL || !QFileInfo(dirArg).exists())
  {
    if(!QFileInfo(dirArg).exists()) cout<<dirArg<<" does not exist."<<endl;
    QString userDirPath = userData->get("workingDir","value");
    cout << "The last used working dir is: " << userDirPath.toStdString() << endl;
    if(userDirPath.isEmpty()) 
      userDirPath = QDir::homePath(); 
    workingDir=QFileDialog::getExistingDirectory(this, "Select an Image Folder", userDirPath);
    if(workingDir.isEmpty()) exit(0);
  }
  else
  {
    workingDir = dirArg;
  }
  userData->set("workingDir", workingDir);
  userData->save();

  if(!QFileInfo(workingDir + "/" + "2dx_image.cfg").exists())
  {
    if(QMessageBox::question(this,tr("Confirm Processing new Image?"),"No configuration data found in:\n"+workingDir+"\n\nCreate new config files in this directory?",tr("Create"),tr("Cancel"),QString(),0,1) == 1) exit(0);
    initialize = true;
  }

  
  QString applicationConfigPath =  configDir.canonicalPath() + "/" + "2dx_master.cfg";
  confData masterData(userPath + "/2dx_master.cfg", applicationConfigPath);
  if(QFileInfo(applicationConfigPath).exists())
  {
    masterData.updateConf(applicationConfigPath);
  }
  masterData.save();
   
  if(initialize)
  {
    data = new confData(workingDir + "/" + "2dx_image.cfg", userPath + "/2dx_master.cfg");
  }
  else
  {
    data = new confData(workingDir + "/" + "2dx_image.cfg");
    QString userConfigPath = userPath + "/2dx_master.cfg";
    if(QFileInfo(userConfigPath).exists())
      data->updateConf(userConfigPath);
  }
  data->setUserConf(userData);
  data->setDir("application",applicationDir);
  data->setDir("plugins",QDir(applicationDir.absolutePath() + "/../plugins"));
  data->setDir("tools",data->getDir("plugins") + "/tools/");
  data->setDir("standardScripts",applicationDir.absolutePath() + "/../kernel/2dx_image" + "/scripts-standard/");
  data->setDir("customScripts",applicationDir.absolutePath() + "/../kernel/2dx_image" + "/scripts-custom/");
  data->setDir("config",configDir);
  data->setDir("project",QDir(workingDir + "/../"));
  data->setDir("icons",data->getDir("config") + "/resource");
  data->setDir("working",workingDir);

  createDir(workingDir + "/proc");
  data->setDir("remoteProc",workingDir + "/proc/");
  createDir(workingDir + "/LOGS");
  data->setDir("logs",workingDir + "/LOGS");

  data->setDir("binDir",data->getDir("application") + "/bin/");
  data->setDir("procDir",data->getDir("application") + "/../proc/");



  confData *cfg = new confData(QDir::homePath() + "/.2dx/" + "2dx.cfg", data->getDir("config") + "/" + "2dx.cfg");
  if(cfg->isEmpty()) {cerr<<"2dx.cfg not found."<<endl; exit(0);}
  cfg->save();

  data->setAppConf(cfg);
  //data->addApp("scriptEditor",cfg.get("scriptEditor","value"));
  //data->addApp("webBrowser",cfg.get("webBrowser","value"));
  //data->addApp("psViewer",cfg.get("psViewer","value"));
  data->addApp("this", data->getDir("application") + "/../" + "bin/" + "2dx_image");
  data->addApp("2dx_image", data->getDir("application") + "/../" + "bin/" + "2dx_image");
  data->addApp("2dx_merge", data->getDir("application") + "/../" + "bin/" + "2dx_merge");
  data->addApp("logBrowser", data->getDir("application") + "/../" + "bin/" + "2dx_logbrowser");

  data->setURL("help","http://2dx.org/documentation/2dx-software");
  data->setURL("bugReport","https://github.com/C-CINA/2dx/issues");

  data->addImage("appImage", new QImage(data->getDir("application") + "/resource/" + "icon.png"));

//  if(initialize)
//		data->syncWithUpper("INHERITABLE_UPON_INIT");
  data->syncWithUpper();
  data->loadDefaults();
  if(!QFileInfo(workingDir + "/" + "2dx_image.cfg").exists()) data->save();
  if(!setupIcons(data,data->getDir("icons"))) cerr<<"Icon loading failed"<<endl;

  QMenuBar *m = new QMenuBar;
  setMenuBar(m);

  centerWin = new centralWindow(data,this);
  setCentralWidget(centerWin);
  setWindowTitle(data->getDir("working"));

    connect(centerWin, SIGNAL(scriptCompletedSignal()), this, SLOT(stopPlay()));

  int fontSize = data->userConf()->get("fontSize","value").toInt();
  if(fontSize!=0)
  {
    QFont mainFont(QApplication::font());
    mainFont.setPointSize(fontSize);
    QApplication::setFont(mainFont);
    centerWin->updateFontInfo();
  }
  else
  {
    QFont font = QApplication::font();
    data->userConf()->set("fontSize",QString::number(font.pointSize()));
    data->userConf()->save();
    QApplication::setFont(font);
    centerWin->updateFontInfo();
  }

  updates = new updateWindow(data,this);
  updates->hide();

  about = new aboutWindow(data,this);
  about->hide();

  setupActions();
  setupToolBar();
  setupMenuBar();
  
  resize(1000, 720);
}

void mainWindow::setupActions()
{
  openAction = new QAction(*(data->getIcon("open")), tr("&Open"), this);
  connect(openAction,SIGNAL(triggered()),this,SLOT(open()));

  saveAction = new QAction(*(data->getIcon("save")), tr("&Save"), this);
  connect(saveAction,SIGNAL(triggered()),this,SLOT(save()));
  
  timer_refresh = 10000;
  timer = new QTimer(this);
  connect(timer, SIGNAL(timeout()), this, SLOT(save()));
  timer->start(timer_refresh);

  saveAsDefaultAction = new QAction("Save As Tilt-Range Default",this);
  saveAsDefaultAction->setShortcut(tr("Ctrl+D"));
  connect(saveAsDefaultAction,SIGNAL(triggered()),this,SLOT(saveAsTiltRangeDefault()));

  saveAsProjectDefaultAction = new QAction("Save As Project Default",this);
  saveAsProjectDefaultAction->setShortcut(tr("Ctrl+P"));
  connect(saveAsProjectDefaultAction,SIGNAL(triggered()),this,SLOT(saveAsProjectDefault()));

  saveAsAction = new QAction("Save Config As ...",this);
  saveAsAction->setShortcut(tr("Ctrl+A"));
  connect(saveAsAction,SIGNAL(triggered()),this,SLOT(saveAs()));

  autoSave = new QAction("Auto Save Changes",this);
  autoSave->setCheckable(true);
  connect(autoSave,SIGNAL(triggered()),this,SLOT(updateAutoSave()));

  revertAction = new QAction("Revert to Saved Data",this);
  revertAction->setShortcut(tr("Ctrl+R"));
  connect(revertAction,SIGNAL(triggered()),this,SLOT(revert()));

  closeAction = new QAction("Quit",this);
  closeAction->setShortcut(tr("Ctrl+Q"));
  connect(closeAction,SIGNAL(triggered()),qApp,SLOT(closeAllWindows()));

  increaseFontAction = new QAction("Increase Font Size",this);
  increaseFontAction->setShortcut(tr("]"));
  connect(increaseFontAction,SIGNAL(triggered()),this,SLOT(increaseFontSize()));

  decreaseFontAction = new QAction("Decrease Font Size",this);
  decreaseFontAction->setShortcut(tr("["));
  connect(decreaseFontAction,SIGNAL(triggered()),this,SLOT(decreaseFontSize()));

  showUpdatesAction = new QAction("Update...",this);
  connect(showUpdatesAction,SIGNAL(triggered()),this,SLOT(showUpdates()));

  showAboutAction = new QAction("About",this);
  connect(showAboutAction,SIGNAL(triggered()),about,SLOT(show()));
  
  playAction = new QAction(*(data->getIcon("play")), tr("&Run selected script(s)"), this);
  playAction->setCheckable(true);
  connect(playAction, SIGNAL(toggled(bool)), centerWin, SLOT(execute(bool)));
  
  refreshAction = new QAction(*(data->getIcon("refresh")), tr("&Refresh Results"), this);
  connect(refreshAction, SIGNAL(triggered()), centerWin, SLOT(reload()));
  
  manualAction = new QAction(*(data->getIcon("help")), tr("Show Manual"), this);
  manualAction->setCheckable(true);
  connect(manualAction, SIGNAL(triggered(bool)), centerWin, SLOT(showManual(bool)));
  
  actionList.insert("useNewViewerAction",new QAction("Enable Threaded Viewer (Experimental)...",this));
  actionList["useNewViewerAction"]->setCheckable(true);
  actionList["useNewViewerAction"]->setChecked(false);
  connect(actionList["useNewViewerAction"],SIGNAL(toggled(bool)),centerWin,SLOT(useNewViewer(bool)));
  
  actionList.insert("openPreferencesAction",new QAction("Preferences",this));
  connect(actionList["openPreferencesAction"],SIGNAL(triggered()),this,SLOT(editHelperConf()));

}

void mainWindow::setupToolBar() 
{
    QToolBar* fileToolBar = addToolBar(tr("File"));
    fileToolBar->addAction(openAction);
    fileToolBar->addAction(saveAction);

    QToolBar* actionToolBar = addToolBar(tr("Action"));
    actionToolBar->addAction(playAction);
    actionToolBar->addAction(refreshAction);

    QToolBar* helpToolBar = addToolBar(tr("Help"));
    helpToolBar->addAction(manualAction);
}

void mainWindow::setupMenuBar()
{
  QMenu *fileMenu = new QMenu("File");
  fileMenu->addAction(openAction);
  fileMenu->addAction(saveAction);
  fileMenu->addAction(saveAsDefaultAction);
  fileMenu->addAction(saveAsProjectDefaultAction);
  fileMenu->addAction(saveAsAction);
  fileMenu->addAction(autoSave);
  fileMenu->addAction(revertAction);
  fileMenu->addAction(closeAction);

  QMenu *editMenu =new QMenu("Edit");
  editMenu->addAction(increaseFontAction);
  editMenu->addAction(decreaseFontAction);
  
  QMenu *actionMenu = new QMenu("Action");
  actionMenu->addAction(playAction);
  actionMenu->addAction(refreshAction);
  
  QMenu *optionsMenu = new QMenu("Options");
  optionsMenu->addAction(actionList["openPreferencesAction"]);
  optionsMenu->addAction(showUpdatesAction);
  optionsMenu->addAction(showAboutAction);
  optionsMenu->addAction(actionList["useNewViewerAction"]);
  
  QAction *showAutoSaveAction = new QAction("Autosave On/Off",this);
  connect(showAutoSaveAction,SIGNAL(triggered()),this,SLOT(toggleAutoSave()));
  optionsMenu->addAction(showAutoSaveAction);
  
    QMenu *helpMenu = new QMenu("Help");

    QSignalMapper *mapper = new QSignalMapper(this);

    QAction *viewOnlineHelp = new QAction(*(data->getIcon("manual")), tr("&View Online Help"), this);
    viewOnlineHelp->setCheckable(false);
    connect(viewOnlineHelp, SIGNAL(triggered()), mapper, SLOT(map()));
    mapper->setMapping(viewOnlineHelp, data->getURL("help"));
    helpMenu->addAction(viewOnlineHelp);

    QAction* bugReport = new QAction(*(data->getIcon("Bug")), tr("&Report Issue/Bug"), this);
    bugReport->setCheckable(false);
    connect(bugReport, SIGNAL(triggered()), mapper, SLOT(map()));
    mapper->setMapping(bugReport, data->getURL("bugReport"));
    helpMenu->addAction(bugReport);

    helpMenu->addAction(manualAction);

    connect(mapper, SIGNAL(mapped(const QString &)), this, SLOT(openURL(const QString &)));
    helpMenu->addAction(showUpdatesAction);
    helpMenu->addAction(showAboutAction);

    menuBar()->addMenu(fileMenu);
    menuBar()->addMenu(editMenu);
    menuBar()->addMenu(actionMenu);
    menuBar()->addMenu(optionsMenu);
    menuBar()->addMenu(helpMenu);
}

void mainWindow::open()
{
  QProcess::startDetached(data->getApp("this"));
}

void mainWindow::toggleAutoSave()
{
  m_do_autosave = !m_do_autosave;
  
  if (m_do_autosave)
  {
	  QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched on"));
	  timer->start(timer_refresh);
  }
  else
  {
	  QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched off"));
	  timer->stop();
  }
}

void mainWindow::save()
{
  data->save();
}

void mainWindow::saveAs()
{
  QString saveName = QFileDialog::getSaveFileName(this,"Save config file as...","./");
  if(!saveName.isEmpty())
    data->saveAs(saveName);
}

void mainWindow::saveAsProjectDefault()
{
  QString confFile = QFileInfo(data->getDir("working") + "/../../").absolutePath() + "/" + "2dx_master.cfg";
  confFile.replace(QRegExp("/+"),"/");
  if(QMessageBox::question(this,
			   tr("Save as default?"),"Saving as \n" + confFile + "\n will set default values for all other images in this project. \n\n Make sure you have QUIT the program 2dx_merge.exe before continuing here. Otherwise, 2dx_merge.exe would overwrite the default values again with its own set of values. \n\n Proceed?",
			   tr("Yes"),
			   tr("No"),
			   QString(),0,1) == 0)
    data->saveAs(confFile);
}

void mainWindow::saveAsTiltRangeDefault()
{
  QString confFile = QFileInfo(data->getDir("working") + "/../").absolutePath() + "/" + "2dx_master.cfg";
  confFile.replace(QRegExp("/+"),"/");
  if(QMessageBox::question(this,
			   tr("Save as default?"),"Saving as \n" + confFile + "\n will set default values for all other images in this project. \n\n Make sure you have QUIT the program 2dx_merge.exe before continuing here. Otherwise, 2dx_merge.exe would overwrite the default values again with its own set of values. \n\n Proceed?",
			   tr("Yes"),
			   tr("No"),
			   QString(),0,1) == 0)
    data->saveAs(confFile);
}


void mainWindow::closeEvent(QCloseEvent *event)
{
  if(!data->isModified())
    event->accept();
  else
  {
    int choice = QMessageBox::question(this,tr("Confirm Exit"),tr("Data not saved, exit?"),tr("Save && Quit"),tr("Quit Anyway"),QString("Cancel"),0,1);
    if(choice == 0)
    {
      data->save();
      event->accept();
    }
    else if(choice == 1)
      event->accept();
    else if(choice == 2)
      event->ignore();
  }
}

void mainWindow::increaseFontSize()
{
  QFont font = QApplication::font();
  font.setPointSize(font.pointSize()+1);
  data->userConf()->set("fontSize",QString::number(font.pointSize()));
  data->userConf()->save();
  QApplication::setFont(font);
  centerWin->updateFontInfo();
}

void mainWindow::decreaseFontSize()
{
  QFont font = QApplication::font();
  font.setPointSize(font.pointSize()-1);
  data->userConf()->set("fontSize",QString::number(font.pointSize()));
  data->userConf()->save();
  QApplication::setFont(font);
  centerWin->updateFontInfo();
}

void mainWindow::updateAutoSave()
{
  data->setAutoSave(autoSave->isChecked());
}

void mainWindow::revert()
{
  int choice = QMessageBox::question(this,tr("Revert Data"),tr("Reload database from last save?"),tr("Revert"),tr("Cancel"),QString(),0,1);
  if(choice == 0)
    centerWin->revert();
}

void mainWindow::showUpdates()
{
  updates->show();
}

void mainWindow::stopPlay() 
{
    playAction->setChecked(false);
}

void mainWindow::editHelperConf()
{
  new confEditor(data->getSubConf("appConf")); 
}

confData *mainWindow::getUserConf(const QString &fileName)
{
  QString confFile = data->getDir("application") + "../config/" + "2dx_image-user.cfg";
  confData *userInf = new confData(confFile);
  QFileInfo file(fileName);
  if(!file.absoluteDir().exists()) QDir().mkpath(file.absolutePath());
  if(!file.exists())
    QFile::copy(confFile,fileName);

  confData localInf(fileName);
  userInf->loadConf(&localInf);
  userInf->setSaveName(fileName);
  userInf->save();

  return userInf;
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

bool mainWindow::createDir(const QString &dir)
{
  QDir directory(dir);
  if(!directory.exists())
    return directory.mkdir(dir);
  return false;
}

void mainWindow::openURL(const QString &url) {
    QProcess::startDetached(data->getApp("webBrowser") + " " + url);
}
