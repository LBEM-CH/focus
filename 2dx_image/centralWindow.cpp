/***************************************************************************
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

#include "centralWindow.h"
#include <iostream>
using namespace std;

centralWindow::centralWindow(confData *conf, QWidget *parent)
              :QWidget(parent)
{
  data = conf;
  QGridLayout *layout = new QGridLayout(this);
  layout->setMargin(0);
  layout->setSpacing(0);
  setLayout(layout);

  QMainWindow *mainWin = static_cast<QMainWindow*>(parent);

  int maxWidth = int(QApplication::desktop()->width()/5.00);
  if(maxWidth>235) maxWidth = 235;

  headers << new controlActionsGroup(data, controlActionsGroup::actions)
          << new controlActionsGroup(data, controlActionsGroup::progress)
          << new controlActionsGroup(data, controlActionsGroup::viewActions);

  connect(data,SIGNAL(dataModified(bool)),headers[0],SLOT(saveAvailable(bool)));
  connect(headers[0],SIGNAL(save()),data,SLOT(save()));
  connect(headers[2],SIGNAL(toggleManual(bool)),this,SLOT(showManual(bool)));
  connect(headers[2],SIGNAL(refresh()),this,SLOT(reload()));

  QHBoxLayout *headerLayout = new QHBoxLayout;

  for(int i=0;i<3;i++)
  {
    headerLayout->addWidget(headers[i]);
    headerLayout->setStretchFactor(headers[i],1);
  }

  layout->addLayout(headerLayout,0,0,1,3);

  standardScripts = new scriptModule(data, data->getDir("standardScripts"), scriptModule::standard);
  standardScripts->setMaximumWidth(maxWidth);
  viewContainer *standardScriptsContainer = new viewContainer("Standard Scripts");
  standardScriptsContainer->setToolTip("Double click to select all scripts.");
  standardScriptsContainer->addWidget(standardScripts);

  QAction *selectAllScriptsAction = new QAction("Select All Scripts",standardScriptsContainer);
  connect(selectAllScriptsAction,SIGNAL(triggered()),standardScripts,SLOT(selectAll()));
  standardScriptsContainer->addAction(selectAllScriptsAction);
  standardScriptsContainer->setContextMenuPolicy(Qt::ActionsContextMenu);

  connect(standardScriptsContainer,SIGNAL(doubleClicked()),standardScripts,SLOT(selectAll()));
  connect(headers[0],SIGNAL(execute(bool)),standardScripts,SLOT(execute(bool)));
  connect(standardScripts,SIGNAL(currentScriptChanged(QModelIndex)),this,SLOT(standardScriptChanged(QModelIndex)));
  connect(standardScripts,SIGNAL(scriptCompleted(QModelIndex)),this,SLOT(standardScriptCompleted(QModelIndex)));
  connect(standardScripts,SIGNAL(runningScriptChanged(QModelIndex)),this,SLOT(standardRunningScriptChanged(QModelIndex)));
  connect(standardScripts,SIGNAL(progress(int)),headers[1],SLOT(setProgress(int)));
  connect(standardScripts,SIGNAL(incrementProgress(int)),headers[1],SLOT(incrementProgress(int)));  
//  connect(standardScripts,SIGNAL(reload()),this,SLOT(reload()));

  customScripts = new scriptModule(data, data->getDir("customScripts"), scriptModule::custom);
  customScripts->setMaximumWidth(maxWidth);
  viewContainer *customScriptsContainer = new viewContainer("Custom Scripts",this);
  customScriptsContainer->addWidget(customScripts);

  connect(headers[0],SIGNAL(execute(bool)),customScripts,SLOT(execute(bool)));
  connect(customScripts,SIGNAL(currentScriptChanged(QModelIndex)),this,SLOT(customScriptChanged(QModelIndex)));
  connect(customScripts,SIGNAL(scriptCompleted(QModelIndex)),this,SLOT(customScriptCompleted(QModelIndex)));
  connect(customScripts,SIGNAL(runningScriptChanged(QModelIndex)),this,SLOT(customRunningScriptChanged(QModelIndex)));
  connect(customScripts,SIGNAL(progress(int)),headers[1],SLOT(setProgress(int)));
  connect(customScripts,SIGNAL(incrementProgress(int)),headers[1],SLOT(incrementProgress(int)));    
//  connect(customScripts,SIGNAL(reload()),this,SLOT(reload()));

  standardScripts->extendSelectionTo(customScripts);
  customScripts->extendSelectionTo(standardScripts);
  standardScripts->setMaximumWidth(maxWidth);
  customScripts->setMaximumWidth(maxWidth);

  layout->addWidget(standardScriptsContainer,1,0,1,1);
  layout->addWidget(customScriptsContainer,2,0,1,1);
  layout->setColumnStretch(1,1);

  viewContainer *previewContainer = new viewContainer("Image Preview",viewContainer::image,this);
  preview = new imagePreview(data, "",true, mainWin->menuBar(), previewContainer);
  connect(preview,SIGNAL(load()),this,SLOT(refresh()));
  previewContainer->addWidget(preview);
  layout->addWidget(previewContainer,3,0,1,1);

  layout->setRowStretch(1,1);
  layout->setRowStretch(2,1);
  layout->setRowStretch(3,0);

  parameterContainer = new viewContainer("Processing Data -- Standard", viewContainer::data);

  localParameters = new resizeableStackedWidget(parameterContainer);
//  localParameters->setSizePolicy(QSizePolicy::Expanding,QSizePolicy::Fixed);
  manuals = new QStackedWidget;
  parameters = new confInterface(data,"");


  QSplitter *globalSplitter = new QSplitter(this);
  globalSplitter->setOrientation(Qt::Horizontal);
  globalSplitter->setHandleWidth(4);

  /* Central Interface including Parameters and Log Browser */

  parametersWidget = new QWidget;
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

  userLevelButtons = new levelGroup(data,2,QStringList()<<"Processing Data -- Standard"<<"Processing Data -- Advanced",
                                                           QStringList()<<"Simplified Parameter List"<<"Full Parameter List",
                                                           QStringList()<<"gbAqua"<<"gbRed");
  parameterContainer->setHeaderWidget(userLevelButtons);
  connect(userLevelButtons,SIGNAL(titleChanged(const QString &)),parameterContainer,SLOT(setText(const QString &)));
  connect(userLevelButtons,SIGNAL(levelChanged(int)),parameters,SLOT(setSelectionUserLevel(int)));

  logViewer = new LogViewer("Standard Output",NULL);
  bridgeScriptLogConnection(true);

  QAction *viewLog = new QAction("Launch Log Browser",logViewer);
  connect(viewLog,SIGNAL(triggered()),this,SLOT(launchLogBrowser()));
  logViewer->addAction(viewLog);
  logViewer->setContextMenuPolicy(Qt::ActionsContextMenu);

  graphicalButton *historyButton = new graphicalButton(data->getIcon("gbPurple"));
  historyButton->setCheckable(true);
  historyButton->setToolTip("Toggle between Logfile and Processing History");
  connect(historyButton,SIGNAL(toggled(bool)),this,SLOT(toggleHistoryView(bool)));

  verbosityControl = new levelGroup(data, 4,
      QStringList()<<"Logfile - Silent"<<"Logfile - Low Verbosity"<<"Logfile - Moderate Verbosity"<<"Logfile - Highest Verbosity",
      QStringList()<<"Update only on script completion."<<"Low Verbosity Output"<<"Moderate Verbosity Output"<<"Highest Verbosity Output",
      QStringList()<<"gbAqua"<<"gbBlue"<<"gbOrange"<<"gbRed");

  viewContainer *logWindow = new viewContainer("Logfile - Low Verbosity",viewContainer::data,NULL,viewContainer::grey);
  logWindow->setHeaderWidget(verbosityControl);
  logWindow->setHeaderWidget(historyButton,Qt::AlignRight);
  logWindow->addWidget(logViewer);
  logWindow->setHeaderToolTip("Double click to to launch log browser.");
  connect(logWindow,SIGNAL(doubleClicked()),this,SLOT(launchLogBrowser()));
  connect(verbosityControl, SIGNAL(titleChanged(const QString &)), logWindow, SLOT(setText(const QString &)));
  connect(verbosityControl, SIGNAL(levelChanged(int)), logViewer, SLOT(load(int)));
  connect(verbosityControl, SIGNAL(levelChanged(int)), standardScripts, SLOT(setVerbosity(int)));
  connect(verbosityControl, SIGNAL(levelChanged(int)), customScripts, SLOT(setVerbosity(int)));

  warningWindow = new warningBox;
  logWindow->addWidget(warningWindow);


  QSplitter *centralSplitter = new QSplitter(this);
  centralSplitter->setOrientation(Qt::Vertical);

  centralSplitter->addWidget(parameterContainer);
  centralSplitter->addWidget(logWindow);
  globalSplitter->addWidget(centralSplitter);
  /*           Results View Information               */

  QSplitter *resultsSplitter = new QSplitter(Qt::Vertical);

  viewContainer *resultsContainer = new viewContainer("Results", viewContainer::data);
  results = new resultsParser(data,QStringList()<<"",resultsParser::results);
  results->setContextMenuPolicy(Qt::ActionsContextMenu);

  QAction *resultsLoadAction = new QAction("Re-Evaluate Results",results);
  results->addAction(resultsLoadAction);
  connect(resultsLoadAction,SIGNAL(triggered()),this,SLOT(reload()));

  resultsContainer->addWidget(results);

  resultsSplitter->addWidget(resultsContainer);


  viewContainer *imagesContainer = new viewContainer("Images", viewContainer::data);

  imageParser = new resultsParser(data,QStringList()<<"",resultsParser::images);
  connect(imageParser,SIGNAL(imageSelected(const QString &)), preview, SLOT(setImage(const QString&)));
  connect(imageParser,SIGNAL(cellActivated(int,int)), preview, SLOT(launchNavigator()));

  imagesContainer->addWidget(imageParser);

  levelGroup *imageLevelButtons = new levelGroup(conf,2,QStringList()<<"All Images"<<"Important Images", QStringList()<<"Show all images"<<"Show only important images",QStringList()<<"gbAqua"<<"gbRed");
  levelGroup *imageNamesButtons = new levelGroup(conf,2,QStringList()<<"Nicknames"<<"Filenames", QStringList()<<"Show Nicknames"<<"Show File Names",QStringList()<<"gbOrange"<<"gbPurple");
  imagesContainer->setHeaderWidget(imageLevelButtons,Qt::AlignLeft);
  imagesContainer->setHeaderWidget(imageNamesButtons,Qt::AlignRight);
  connect(imageLevelButtons,SIGNAL(levelChanged(int)),imageParser,SLOT(setImportant(int)));
  connect(imageNamesButtons,SIGNAL(levelChanged(int)),imageParser,SLOT(setShowFilenames(int)));

  resultsSplitter->addWidget(imagesContainer);

  viewContainer *statusContainer = new viewContainer("Status",viewContainer::data);
  //statusParser = new statusViewer(data->getDir("working") + "/2dx_status.html", data->getDir("config") + "/2dx_image/2dx_status.html");
  statusParser = new statusViewer(data->getDir("config") + "/2dx_image/2dx_status.html");
  statusParser->setConf(data);
  statusParser->load();
  statusContainer->addWidget(statusParser);

  QWidget *rightContainer = new QWidget;
  QVBoxLayout *rightLayout = new QVBoxLayout;
  rightLayout->setMargin(0);
  rightLayout->setSpacing(0);
  rightContainer->setLayout(rightLayout);
  rightLayout->addWidget(resultsSplitter);
  rightLayout->addWidget(statusContainer);
  rightLayout->setStretchFactor(resultsSplitter,1);
  rightLayout->setStretchFactor(statusContainer,0);


  globalSplitter->addWidget(rightContainer);

  /*           Footer layout Information              */

  layout->addWidget(globalSplitter, 1,1,3,1);

  headers<<new controlActionsGroup(data, controlActionsGroup::footer);
  layout->addWidget(headers.last(),4,0,1,3);

  connect(headers[3],SIGNAL(toggleInfo()),preview,SLOT(toggleInfo()));
  connect(headers[3],SIGNAL(hideWidget()),preview,SLOT(shade()));
  connect(headers[3],SIGNAL(viewHelp()),this,SLOT(viewHelp()));
  connect(headers[3],SIGNAL(reportBug()),this,SLOT(reportBug()));

  connect(standardScripts,SIGNAL(initialized()),data,SLOT(save()));

  manuals->hide();
  verbosityControl->setLevel(1);
  standardScripts->initialize();
  headers[1]->setText("2dx_image, Version " + VERSION_2DX);
  standardScripts->setFocus(Qt::ActiveWindowFocusReason);

  QList<int> s = globalSplitter->sizes();

	s[0] = 10000;
  s[1] = 100;
  globalSplitter->setSizes(s);

}

void centralWindow::bridgeScriptLogConnection(bool bridge)
{
  if(bridge)
  {
    connect(standardScripts,SIGNAL(standardOut(const QStringList &)),logViewer,SLOT(insertText(const QStringList &)));
    connect(standardScripts,SIGNAL(standardError(const QByteArray &)),logViewer,SLOT(insertError(const QByteArray &)));
    connect(customScripts,SIGNAL(standardOut(const QStringList &)),logViewer,SLOT(insertText(const QStringList &)));
    connect(customScripts,SIGNAL(standardError(const QByteArray &)),logViewer,SLOT(insertError(const QByteArray &)));
    connect(standardScripts,SIGNAL(scriptLaunched()),logViewer,SLOT(clear()));
    connect(customScripts,SIGNAL(scriptLaunched()),logViewer,SLOT(clear()));
  }
  else
  {
    disconnect(standardScripts,SIGNAL(standardOut(const QStringList &)),logViewer,SLOT(insertText(const QStringList &)));
    disconnect(standardScripts,SIGNAL(standardError(const QByteArray &)),logViewer,SLOT(insertError(const QByteArray &)));
    disconnect(customScripts,SIGNAL(standardOut(const QStringList &)),logViewer,SLOT(insertText(const QStringList &)));
    disconnect(customScripts,SIGNAL(standardError(const QByteArray &)),logViewer,SLOT(insertError(const QByteArray &)));
    disconnect(standardScripts,SIGNAL(scriptLaunched()),logViewer,SLOT(clear()));
    disconnect(customScripts,SIGNAL(scriptLaunched()),logViewer,SLOT(clear()));
  }
}

void centralWindow::scriptChanged(scriptModule *module, QModelIndex index)
{
  int uid = index.data(Qt::UserRole).toUInt();
  currentResults = module->resultsFile(index);

  if((module == customScripts && !standardScripts->isRunning()) || (module == standardScripts && !customScripts->isRunning())) headers[1]->setText(module->title(index));

  if(localIndex[uid] == 0 && module->conf(index)->size()!=0)
  {
    confInterface *local = new confInterface(module->conf(index), "");
    connect(this,SIGNAL(fontInfoUpdated()),local,SLOT(updateFontInfo()));
    localIndex[uid] = localParameters->addWidget(local) + 1;
  }

  if(manualIndex[uid] == 0 && !module->conf(index)->manual().isEmpty())
    manualIndex[uid] = manuals->addWidget(new confManual(data, module->conf(index))) + 1;

  if(localIndex[uid] - 1 < 0)
    localParameters->hide();
  else
  {
    localParameters->show();
    localParameters->setCurrentIndex(localIndex[uid] - 1);
  }

  manuals->setCurrentIndex(manualIndex[uid] - 1);

  parameters->select(module->globalVariables(index));
  currentLog = module->logFile(index);
  if(!visible["historyview"]) logViewer->loadLogFile(currentLog);
  results->setResult(module->resultsFile(index));
  imageParser->setResult(module->resultsFile(index));
  warningWindow->load(module->resultsFile(index));
  parametersWidget->update();
}

void centralWindow::standardScriptChanged(QModelIndex index)
{
  scriptChanged(standardScripts, index);
}

void centralWindow::customScriptChanged(QModelIndex index)
{
  scriptChanged(customScripts, index);
}

bool centralWindow::parseResults(confData *conf, const QString &results)
{
  currentResults = results;
  return scriptParser::parseResults(conf,results);
}

bool centralWindow::parseResults()
{
  return parseResults(data, currentResults);
}

void centralWindow::scriptCompleted(scriptModule *module, QModelIndex index)
{
  if(!module->isRunning())
    headers[0]->scriptFinished();
  parseResults(data,module->resultsFile(index));
//  results->setResult(module->resultsFile(index));
  imageParser->setResult(module->resultsFile(index));
  parameters->load();
  //statusParser->load();
}

void centralWindow::standardScriptCompleted(QModelIndex index)
{
  scriptCompleted(standardScripts,index);
}

void centralWindow::customScriptCompleted(QModelIndex index)
{
  scriptCompleted(customScripts,index);
}

void centralWindow::runningScriptChanged(scriptModule *module, QModelIndex index)
{
//  int uid = index.data(Qt::UserRole).toUInt();
  headers[1]->setText(module->title(index));
}

void centralWindow::customRunningScriptChanged(QModelIndex index)
{
  runningScriptChanged(customScripts,index);
}

void centralWindow::standardRunningScriptChanged(QModelIndex index)
{
  runningScriptChanged(standardScripts,index);
}

void centralWindow::reload()
{
  parseResults();
  refresh();
}

void centralWindow::refresh()
{
  parameters->load();
  //statusParser->load();
  results->load();
  imageParser->load();
  //statusParser->load();
}

void centralWindow::showManual(bool show)
{
  if(show) {parameterContainer->restoreSplitterState(0); manuals->show();}
  else {parameterContainer->saveSplitterState(0); parameterContainer->maximizeWindow(1); manuals->hide();}
}

void centralWindow::viewHelp()
{
  QProcess::startDetached(data->getApp("webBrowser") + " " + data->getURL("help"));
}

void centralWindow::reportBug()
{
  QProcess::startDetached(data->getApp("webBrowser") + " " + data->getURL("bugReport"));
}

void centralWindow::revert()
{
  data->reload();
  parameters->load();
  //statusParser->load();
}

void centralWindow::updateFontInfo()
{
  parameters->updateFontInfo();
  logViewer->updateFontInfo();
  results->updateFontInfo();
  imageParser->updateFontInfo(); 
  emit fontInfoUpdated();
}

void centralWindow::launchLogBrowser()
{
  QProcess::startDetached(data->getApp("logBrowser") + " " + logViewer->getLogFile());
}

void centralWindow::toggleHistoryView(bool show)
{
  visible["historyview"] = show;
  if(show)
  {
    verbosityControl->setTitleNames(QStringList()<<"History - Silent"<<"History - Low Verbosity"<<"History - Moderate Verbosity"<<"History - Highest Verbosity");
    bridgeScriptLogConnection(false);
    logViewer->loadLogFile(data->getDir("working") + "/" + "History.dat");
  }
  else
  {
    verbosityControl->setTitleNames(QStringList()<<"Logfile - Silent"<<"Logfile - Low Verbosity"<<"Logfile - Moderate Verbosity"<<"Logfile - Highest Verbosity");
    logViewer->loadLogFile(currentLog);	
    bridgeScriptLogConnection(true);		
  }
  verbosityControl->setLevel(verbosityControl->level());
}

void centralWindow::useNewViewer(bool enable)
{
  preview->enableNewViewer(enable);
}

