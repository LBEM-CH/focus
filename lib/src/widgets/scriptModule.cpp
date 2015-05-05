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

#include <iostream>
#include <scriptModule.h>
using namespace std;

scriptModule::scriptModule(confData *conf, const QDir &directory, scriptModule::moduleType type, QWidget *parent)
             :QWidget(parent)
{
  scriptDir = directory;
  scriptType = type;
  data = conf;

  runningScript = NULL;
  currentlyRunning = false;
  verbosity = 1;

  QGridLayout *layout = new QGridLayout(this);
  layout->setMargin(0);
  layout->setSpacing(0);
  setLayout(layout);

  view = setupModule();
  if(view!=NULL)
  {
    if(type == scriptModule::standard) view->setSelectionMode(QAbstractItemView::ExtendedSelection);
    if(type == scriptModule::custom) view->setSelectionMode(QAbstractItemView::SingleSelection);
    if(type == scriptModule::singleparticle) view->setSelectionMode(QAbstractItemView::ExtendedSelection);
    layout->addWidget(view,0,0,1,1);
    //connect(selection,SIGNAL(currentChanged(const QModelIndex &, const QModelIndex &)),this,SLOT(clearExtendedSelections()));
    //connect(view,SIGNAL(pressed(QModelIndex)),this,SLOT(clearExtendedSelections()));
    //connect(view,SIGNAL(pressed(QModelIndex)),this,SLOT(select(QModelIndex)));//currentScriptChanged(QModelIndex)));
    //connect(view->selectionModel(),SIGNAL(currentChanged(const QModelIndex &, const QModelIndex &)),this,SLOT(select(QModelIndex)));
    connect(view->selectionModel(),SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection&)),this,SLOT(select(const QItemSelection &, const QItemSelection&)));
    connect(&process,SIGNAL(readyReadStandardOutput()),this,SLOT(readStdOut()));
    connect(&process,SIGNAL(readyReadStandardError()),this,SLOT(readStdErr()));
    connect(&process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(scriptFinished(int)));
    connect(view,SIGNAL(doubleClicked(QModelIndex)),this,SLOT(scriptActivated(QModelIndex)));
  }
  view->selectionModel()->clearSelection();
}

QTreeView *scriptModule::setupModule()
{
  if(!scriptDir.exists() || data->isEmpty()) return NULL;

  QTreeView *view = new QTreeView;
  model = new QStandardItemModel;

  view->header()->hide();
  view->setIndentation(8);
  view->setItemDelegate(new SpinBoxDelegate);

  QString entry;
  QStandardItem *item;
  QMap<int, QList<QStandardItem *> > map;
  quint32 sortOrder, uid;
  bool counterPresent = false;
  foreach(entry, scriptDir.entryList(QStringList() << "*.script", QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted))
  {
    QList<QStandardItem *> itemList;
    QStringList titleList;
    confData *scriptData = new confData(scriptDir.path() + "/" + entry, data);
    sortOrder = scriptData->property("sortOrder").toUInt();

    uid = qHash(scriptDir.canonicalPath() + "/" + entry)^qHash(sortOrder);

    localConf[uid] = scriptData;

    titleList<<scriptData->property("title").simplified();

    item = new QStandardItem(titleList.first());
    itemList<<item;

    item->setToolTip(entry);

    addScriptProperty(uid,"path",scriptDir.absolutePath() + "/");
    addScriptProperty(uid,"fileName",entry);
    addScriptProperty(uid,"filePath",scriptDir.canonicalPath() + "/" + entry);
    addScriptProperty(uid,"title",scriptData->property("title"));
    addScriptProperty(uid,"displayedVars",QVariant(scriptData->propertyList("display")));
    addScriptProperty(uid,"logFile", data->getDir("working") + "/LOGS/" +  entry.section('.',0,-2) + ".log");
    addScriptProperty(uid,"resultsFile", data->getDir("working") + "/LOGS/" +  entry.section('.',0,-2) + ".results");

    item->setData(scriptDir.canonicalPath() + "/" + entry,Qt::UserRole + 5);

    item->setData(uid,Qt::UserRole);
    item->setEditable(false);
    if(scriptType == standard)
      item->setIcon(*data->getIcon("scriptIcon"));
    else if(scriptType == custom)
      item->setIcon(*data->getIcon("customScriptIcon"));
    else if(scriptType == singleparticle)
      item->setIcon(*data->getIcon("spScriptIcon"));

    item->setTextAlignment(Qt::AlignVCenter);
    
    QFont itemFont;
    itemFont.setBold(true);
    itemFont.setStretch(QFont::SemiExpanded);
    itemFont.setStyleHint(QFont::Times);
    itemFont.setPixelSize(13);
    item->setFont(itemFont);
    
    QSetIterator<QString> it(scriptData->subScripts());
    QString subScriptTitle;
    QStandardItem *subItem;
    while(it.hasNext())
    {
      subScriptTitle = it.next();
      subItem = new QStandardItem(subScriptTitle);
      subItem->setIcon(*data->getIcon("subScript"));

      subItem->setData(data->getDir("procDir") + "/" + subScriptTitle,Qt::UserRole + 5);
      if(!QFileInfo(data->getDir("procDir") + "/" + subScriptTitle).exists())
        subItem->setForeground(QColor(255,0,0));
      
      subItem->setEditable(false);
      subItem->setData(item->data(Qt::UserRole), Qt::UserRole);
      QList<QStandardItem*> subItems;
      subItems<<subItem;
      item->appendRow(subItems);
    }

    map.insert(sortOrder,itemList);
  }

  QMapIterator<int,QList<QStandardItem*> > it(map);
  while(it.hasNext())
  {
    it.next();
//    view->addTopLevelItem(it.value());
    model->appendRow(it.value());
  }

  view->setModel(model);

  selection = view->selectionModel();

  view->setFrameStyle(QFrame::Panel | QFrame::Sunken);
  view->setAlternatingRowColors(true);
  view->resizeColumnToContents(0);
  view->resizeColumnToContents(1);
  if(!counterPresent)
    view->setColumnHidden(1,true);

  clearSelection();
  return view;
}

void scriptModule::addScriptProperty(quint32 uid, const QString &property, const QVariant &value)
{
  scriptData[uid][property.toLower()] = value;
}

const QVariant & scriptModule::getScriptProperty(quint32 uid, const QString &property)
{
  return scriptData[uid][property.toLower()];
}

QItemSelectionModel* scriptModule::getSelection()
{
    return selection;
}

void scriptModule::clearSelection()
{
  if(selection->hasSelection())
	selection->clearSelection();
	
}

void scriptModule::extendSelectionTo(scriptModule *module)
{
  selectionObjects<<module;
}

void scriptModule::clearExtendedSelections()
{
//  if(!selection->hasSelection()) return;
  QListIterator<scriptModule *> it(selectionObjects);
  while(it.hasNext())
    it.next()->clearSelection();
  
  QListIterator<scriptModule *> it2(selectionObjects);
  while(it2.hasPrevious())
    it2.previous()->clearSelection();
}

bool scriptModule::initializeExecution()
{

  if(executionList.isEmpty())
  {
    if(selection->selectedIndexes().isEmpty()) {return false; }
    runningIndex = 0;
    QMap<int,char> s;
    foreach(QModelIndex index, selection->selectedIndexes())
    {
      if(!index.parent().isValid())
      {
        model->item(index.row())->setForeground(QColor(0,186,0));
        s.insert(index.row(),0);
      }
      else
      {
        selection->select(index.parent(), QItemSelectionModel::Current);
        model->item(index.parent().row())->setForeground(QColor(0,186,0));
        s.insert(index.parent().row(),0);
      }
    }
    executionList = s.keys();
  }
  else if(runningScript == NULL)
  {
    runningIndex++;
    if(runningIndex>=executionList.size())
    {
      currentlyRunning = false;
      emit scriptCompleted(model->item(executionList.last())->index());
      cleanupExecution();
      if(!selection->selectedRows().isEmpty())
        emit currentScriptChanged(selection->selectedRows().last());
      return false;
    }
  }

  runningScript = model->item(executionList[runningIndex]);
  if(runningScriptSelected())
    select(runningScript->index());
  else
    emit runningScriptChanged(runningScript->index());
  
  return true;
}

void scriptModule::cleanupExecution()
{
  foreach(int i, executionList)
  {
    model->item(i)->setForeground(Qt::black);
  }
  executionList.clear();
  runningScript = NULL;
}

void scriptModule::execute(bool run)
{
  QString scriptPath, scriptName;
  QString scriptHeader(78,'#');
  if(run)
  {
    QStandardItem *it;

    currentlyRunning = initializeExecution();
    if(!currentlyRunning)
    {
        return;
    }
    it = runningScript;

    it->setForeground(QColor(0,0,186));
    currentUid = it->data(Qt::UserRole).toUInt();
    scriptName = getScriptProperty(currentUid,"fileName").toString();
    scriptPath = getScriptProperty(currentUid,"path").toString();
    QString scriptTitle = scriptName.section('.',0,-2);

    scriptParser parser(QList<confData *>()<<localConf[currentUid]<<data);

    if(!clean(currentUid)) cerr<<"Error removing "<<getScriptProperty(currentUid,"logFile").toString().toStdString()<<endl;

    if(runningScriptSelected()) emit scriptLaunched();

    scriptHeader.replace((scriptHeader.size()-scriptTitle.size())/2,scriptTitle.size(),scriptTitle);
    scriptHeader = "::" + scriptHeader + "\n";

    if(runningScriptSelected()) emit standardOut(QStringList()<<scriptHeader);
    writeToLog(scriptHeader);

    parser.parse(scriptPath + "/" + scriptName, data->getDir("remoteProc") + "/" + scriptName.section('.',0,-2) + ".com");
    process.setWorkingDirectory(data->getDir("working"));
    process.start('"' + parser.executionString() + '"', QIODevice::ReadOnly);
  }
  else if(process.state() == QProcess::Running)
  {
    scriptName = getScriptProperty(currentUid,"fileName").toString();
    scriptPath = getScriptProperty(currentUid,"path").toString();
    QString scriptTitle = scriptName.section('.',0,-2) + " Halted by User";

    scriptHeader.replace((scriptHeader.size()-scriptTitle.size())/2,scriptTitle.size(),scriptTitle);
    scriptHeader = scriptHeader + "\n";

    if(runningScriptSelected())  emit standardError(scriptHeader.toAscii());
    writeToLog("<error>" + scriptHeader + "</error>" + '\n');

    disconnect(&process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(scriptFinished(int)));
    process.kill();
    while(!process.waitForFinished())
      process.kill();
    scriptFinished(-1);
    connect(&process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(scriptFinished(int)));
  }
}

bool scriptModule::clean(int uid)
{
  bool result = true;
  QFile logFile(getScriptProperty(uid,"logFile").toString());
  if(logFile.exists())
    result = logFile.remove();
  logFile.close();
  return result;
}

void scriptModule::readStdOut()
{
  QStringList text;
  QString line,lineTrimmed,lineLower;
  while(!process.atEnd())
  {
    line = process.readLine();
    lineTrimmed = line.trimmed();
    lineLower = lineTrimmed.toLower();
    if(lineLower.startsWith("<<@progress:"))
    {
      QStringList value = lineTrimmed.split(':');
      value.last().remove('>');
      if(value.last().contains('+'))
      {
	value.last().remove('+');
	emit incrementProgress(value.last().toInt());
      }
      else
        emit progress(value.last().toInt());
    }
    else if(lineLower.startsWith("<<@evaluate>>"))
    {
      emit reload();
    }
    else
      text<<line;
  }

  if(runningScriptSelected() && verbosity != 0) emit standardOut(text);

  writeToLog(text.join(""));
}

void scriptModule::readStdErr()
{
	QString text = process.readAllStandardError();

	if(runningScriptSelected() && verbosity != 0) emit standardError(text.toAscii());

	writeToLog("<error>" + text + "</error>");
}

QStringList scriptModule::displayedVariables(QModelIndex index)
{
	return getScriptProperty(index.data(Qt::UserRole).toUInt(),"displayedVars").toStringList();
}

QString scriptModule::logFile(QModelIndex index)
{
	return getScriptProperty(index.data(Qt::UserRole).toUInt(),"logFile").toString();
}

QString scriptModule::logFile()
{
	return getScriptProperty(uid(),"logFile").toString();
}

QString scriptModule::resultsFile(QModelIndex index)
{
	return getScriptProperty(index.data(Qt::UserRole).toUInt(),"resultsFile").toString();
}

QString scriptModule::resultsFile()
{
	return getScriptProperty(model->item(0)->data(Qt::UserRole).toUInt(),"resultsFile").toString();
}

void scriptModule::scriptActivated(QModelIndex item)
{
	//QProcess::startDetached(data->getApp("scriptEditor") + " " + getScriptProperty(item.data(Qt::UserRole).toUInt(),"filePath").toString());
	QProcess::startDetached(data->getApp("scriptEditor") + " " + item.data(Qt::UserRole + 5).toString());
}

bool scriptModule::runningScriptSelected()
{
	if(selection->selectedIndexes().isEmpty()) return false;
	//if(currentUid==selection->selectedIndexes().first().data(Qt::UserRole).toUInt()) return true;
	bool selected = selection->isSelected(runningScript->index());
	for(int i=0;i<runningScript->rowCount();i++)
		selected = selected || selection->isSelected(runningScript->child(i)->index()); 
	return selected; 
	return false;
}

bool scriptModule::writeToLog(const QString &logText)
{
	QString logFilename = getScriptProperty(currentUid,"logFile").toString();
	QFile log(logFilename);
	if(!log.open(QIODevice::Append | QIODevice::Text))
            {cerr<<"Failed to write to "<<getScriptProperty(currentUid,"logFile").toString().toStdString()<<endl; return false;}
	log.write(logText.toAscii());
	log.close();
	return true;
}

void scriptModule::scriptFinished(int exitCode)
{
	QString scriptTitle = getScriptProperty(currentUid,"fileName").toString().section('.',0,-2);
	QString scriptHeader(78,'#');
	scriptHeader.replace((scriptHeader.size()-(scriptTitle + " finished.").size())/2,(scriptTitle + " finished.").size(),scriptTitle + " finished.");
	scriptHeader = "::" + scriptHeader + "\n";
	if(runningScriptSelected()) emit standardOut(QStringList()<<scriptHeader);
	writeToLog(scriptHeader);

	if(runningScript == NULL)
		cerr<<"Running script is NULL and should not be!"<<endl;
	else
	{
		if(runningScript->row() == 0) emit initialized();
	}

	
	
		
		runningScript->setForeground(Qt::black);
		
		if(executionList.isEmpty())
		{
			currentlyRunning = false;
			emit scriptCompleted(runningScript->index());
			if(exitCode == -1) cleanupExecution();
			if(!selection->selectedRows().isEmpty())
				emit currentScriptChanged(selection->selectedRows().last());
		}
		else
		{
			if(exitCode == -1)
			{
				cleanupExecution();
				//if(!selection->selectedRows().isEmpty())
				//  emit currentScriptChanged(selection->selectedRows().last());
			}
			else
			{
				emit scriptCompleted(runningScript->index());
				runningScript = NULL;
				execute(true);
			}
		}
}

uint scriptModule::uid()
{
	if(selection->hasSelection())
		return selection->selectedIndexes().first().data(Qt::UserRole).toUInt();
	else
		cerr<<"No selection!"<<endl;
	return 0;
}

confData *scriptModule::conf(QModelIndex index)
{

	return localConf[index.data(Qt::UserRole).toUInt()];
}

QString scriptModule::title(QModelIndex index)
{
	if(runningScript==NULL)
		return getScriptProperty(index.data(Qt::UserRole).toUInt(),"title").toString();
	else
		return getScriptProperty(runningScript->data(Qt::UserRole).toUInt(),"title").toString();
}

void scriptModule::initialize()
{       
	setVerbosity(1);
	select(model->item(0)->index());
	execute(true);
}

void scriptModule::select(QModelIndex index)
{
	clearExtendedSelections();
	if(!selection->isSelected(index))  selection->select(index,QItemSelectionModel::Select);
	if(index.parent().isValid()) selection->select(index.parent(),QItemSelectionModel::Current);
	if(selection->selectedIndexes().isEmpty()) cerr<<"No selection for some reason..."<<endl;
	emit currentScriptChanged(index);
}

void scriptModule::select(const QItemSelection& selected)
{
    if(selected.indexes().isEmpty()) return;
    select(selected.indexes().first());
}


void scriptModule::select(const QItemSelection &selected, const QItemSelection &deselected)
{
    select(selected);
}

void scriptModule::selectAll()
{
	select(model->item(0)->index());
	view->selectAll();
}

bool scriptModule::isRunning()
{
	return (currentlyRunning || process.state() == QProcess::Running);
}

void scriptModule::setVerbosity(int value)
{
	verbosity = value;
}

scriptModule::moduleType scriptModule::type()
{
    return scriptType;
}

