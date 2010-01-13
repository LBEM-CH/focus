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


#include "projectModel.h"
#include <iostream>
using namespace std;

bool projectModel::saveColumns(const QString &columnsFile)
{
  QFile modelInfo(columnsFile);
  if(!modelInfo.open(QIODevice::WriteOnly | QIODevice::Text)) return false;  
  foreach(QString line, columnsFileHeader)
  {
    line+='\n';
    modelInfo.write(line.toAscii());
  }
    
  for(int i=2;i<columns.size();i++)
  {
    QHash<QString,QVariant> column = columns[i];
    QString line;
    
    if(column["visible"].toBool()) line+='*';
    
    line+=column["uid"].toString() + ' '; 
    line+='(';
    line+=column["shortname"].toString() + ',';
    line+="mouseover=\"" + column["tooltip"].toString() + '\"' + ',';
    line+=column["lockable"].toString() + ',';
    line+=column["sortable"].toString();
    if(!column["format"].toString().isEmpty())
    {
      line+=',';
      if(column["format"].toString().startsWith('%')) line+='"';
      line+=column["format"].toString();
      if(column["format"].toString().startsWith('%')) line+='"';      
    }
    line+=')';
    line+='\n';
    modelInfo.write(line.toAscii());
  }
  modelInfo.close();
  return true;
}

bool projectModel::saveColumns()
{
  return saveColumns(columnsDataFile);
}

bool projectModel::loadColumns(const QString &columnsFile)
{
  QString line, parameters;
  QFile modelInfo(columnsFile);
  if(!modelInfo.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
  int i=0;
  columns[i]["visible"] = true; columns[i]["uid"] = "merge"; columns[i]["shortname"] = "Merge"; columns[i]["tooltip"] = "Merge";i++;
  columns[i]["visible"] = true; columns[i]["uid"] = "directory";  columns[i]["shortname"] = "Directory"; columns[i]["tooltip"] = "Directory";i++;

  columnsFileHeader.clear();
  
  while(!modelInfo.atEnd())
  {
    line = modelInfo.readLine().trimmed();
    if(line.contains(QRegExp("^\\s*#")))
    {
      columnsFileHeader<<line;
    }
    else if(line.contains(QRegExp("\\S+")))
    {
      parameters = line;
      line.remove(QRegExp("\\(.*\\)"));

      if(line.contains("*"))
      {
        line.remove("*");
        columns[i]["visible"] = true;
      }
      else columns[i]["visible"] = false;

      columns[i]["uid"] = line.trimmed();

      if(parameters.contains(QRegExp("\\(.*\\)")))
      {
        QRegExp pattern("\\(([^,]*),mouseover\\s*=\\s*\"?([^,\\\"]*)\"?,([^,]*),([^,]*)?(,.*)?\\)",Qt::CaseInsensitive);
       if(pattern.indexIn(parameters)==-1) 
         qDebug()<<"Couldn't parse: "<<parameters;

        columns[i]["shortname"] = pattern.cap(1).trimmed();
        columns[i]["tooltip"] = pattern.cap(2).trimmed();
        columns[i]["lockable"] = pattern.cap(3).trimmed();
        columns[i]["sortable"] = pattern.cap(4).trimmed();
        QString format=pattern.cap(5).trimmed().remove(QRegExp("^,|\\\""));

        columns[i]["format"] = format;
        if(columns[i]["shortname"].toString().isEmpty()) columns[i]["shortname"] = data->get(line.trimmed(),"label").trimmed();
      }
      else
        columns[i]["shortname"] = data->get(line.trimmed(),"label").trimmed();
      i++;
    }
  }
  modelInfo.close();

  return true;
}

bool projectModel::loadHidden(const QString &columnsFile)
{
  QFile modelInfo(columnsFile);
  if(!modelInfo.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
  QStringList visible, hidden;
  QString line;
  while(!modelInfo.atEnd())
  {
    line = modelInfo.readLine().trimmed();
    line.remove(QRegExp("\\(.*\\)"));
    if(line.contains("*"))
    {
      line.remove('*');
      visible<<line.trimmed();
    }
    else if(!line.trimmed().isEmpty())
      hidden<<line.trimmed();
  }    
  
  QMutableMapIterator<quint32, QHash<QString,QVariant> > it(columns);
  while(it.hasNext())
  {
    it.next();
    if(visible.contains(it.value()["uid"].toString()))
      it.value()["visible"] = true;
    
    if(hidden.contains(it.value()["uid"].toString()))
      it.value()["visible"] = false;
    
  }
  
  
  modelInfo.close();  
  return true;
}

void projectModel::initDir(const QString &path, QStandardItem *parent)
{
  QDir dir(path);
  dir.setFilter(QDir::NoDotAndDotDot | QDir::Dirs);

  QString entry, entryString, name;
  QStandardItem *entryItem;
  QStringList names, labels;

  foreach(entry, dir.entryList())
  {
    QString configFile = path + "/" + entry + "/" + "2dx_image.cfg";
    QString masterFile = path + "/" + entry + "/" + "2dx_master.cfg";

    if(QFileInfo(configFile).exists() || QFileInfo(masterFile).exists())
    {
      uint uid = qHash(QDir(path + "/" + entry + "/").canonicalPath());

      QList<QStandardItem*> entryItems;
      QStandardItem* include = new QStandardItem;
      items[uid][columns[0]["uid"].toString()] = include;
      initDir(path + "/" + entry + "/",include);
      entryItems << include << new QStandardItem(entry);
      foreach(entryItem, entryItems)
      {
        entryItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
        entryItem->setData(uid);
        entryItem->setData(entry,SortRole);
        entryItem->setData(path + "/" + entry + "/",Qt::UserRole + 5);        
      }

      if(QFileInfo(configFile).exists())
      {
        //watcher.addPath(configFile);
        confData localData(configFile);
        localData.syncWithUpper();
        foreach(quint32 c, columns.keys())
        {
          if(!columns[c]["uid"].toString().isEmpty() && c!=0 && c!=1)
          {
            entryItem = new QStandardItem;
            entryItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
            entryItem->setData(uid);

            if(!columns[c]["format"].toString().isEmpty())
            {
              if(columns[c]["format"].toString().contains(QRegExp("\\%\\S*[cdeEifgGosuxXpn]")))
              {
                QString fwpl = columns[c]["format"].toString();
                fwpl.replace(".*(\\%\\S*[cdeEifgGosuxXpn]).*","\\1");
                if(columns[c]["format"].toString().contains(','))
                {
                  QString format = columns[c]["format"].toString();
                  QStringList formats = format.remove('"').split(',');
                  QStringList entries = localData.get(columns[c]["uid"].toString(),"value").split(',');
                  entryString = "";
                  int k=0;
                  foreach(QString entry, entries)
                  {
                    if(k<entries.size() && k<formats.size())
                    {                      
                      format = formats[k];
                      if(format.contains(QRegExp("[diouxX]$")))
                        entryString += QString().sprintf(format.toAscii(),entry.toInt());
                      else if(fwpl.contains(QRegExp("[eEfgG]$")))                    
                        entryString += QString().sprintf(format.toAscii(),entry.toDouble());                    
                      else
                        entryString += entry;
                      k++;                    
                      if(k<entries.size()) entryString+=',';
                    }
                  }
                  if(format.contains(QRegExp("[diouxX]$")))
                    entryItem->setData(entries[0].toInt(),SortRole);                  
                  else if(fwpl.contains(QRegExp("[eEfgG]$")))                    
                    entryItem->setData(entries[0].toFloat(),SortRole);
                  else
                    entryItem->setData(entries[0],SortRole);                  
                }
                else if(fwpl.contains(QRegExp("[diouxX]$")))
                {
                  entryString = QString().sprintf(columns[c]["format"].toString().toAscii(),localData.get(columns[c]["uid"].toString(),"value").toInt());
                  entryItem->setData(localData.get(columns[c]["uid"].toString(),"value").toInt(),SortRole);
                }
                else if(fwpl.contains(QRegExp("[eEfgG]$")))
                {
                  entryString = QString().sprintf(columns[c]["format"].toString().toAscii(),localData.get(columns[c]["uid"].toString(),"value").toDouble());
                  entryItem->setData(localData.get(columns[c]["uid"].toString(),"value").toDouble(),SortRole);
                }
                else
                {
                  entryString = localData.get(columns[c]["uid"].toString(),"value");
                  entryItem->setData(entryString,SortRole);
                }
              }
              else if(columns[c]["format"].toString().trimmed().toLower() == "checkbox")
              {
                entryString.clear();
                if(localData.get(columns[c]["uid"].toString(),"value").trimmed().contains(QRegExp("^[yY]")))
                  entryItem->setIcon(*data->getIcon("gbRed"));
                else
                  entryItem->setIcon(*data->getIcon("gbBlue"));
              }
              else
                entryString = localData.get(columns[c]["uid"].toString(),"value");
            }
            else
              entryString = localData.get(columns[c]["uid"].toString(),"value");

//            if(entryItem->data(Qt::DisplayRole).isNull())
              entryItem->setText(entryString);            
            entryItem->setData(path + "/" + entry + "/",Qt::UserRole + 5);
            if(columns[c]["lockable"] == "lockable")
              entryItem->setCheckable(true);
            items[uid][columns[c]["uid"].toString()] = entryItem;
            entryItems<<entryItem;
          }
        }
      }

      include->setCheckable(true);
      if(parent != NULL)
        parent->appendRow(entryItems);
      else
        appendRow(entryItems);
      paths.insert(uid, QDir(path + "/" + entry + "/").absolutePath());
    }
  }
}

bool projectModel::loadSelection(const QString &fileName)
{
  if(items.isEmpty()) return false;
  QString selectedFile = fileName, line;
  quint32 uid;
  QStandardItem *item = NULL;
  if(fileName.isEmpty()) selectedFile = saveFileName;

  QFile file(selectedFile);
  if(!file.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
  
  clearSelection(false);
  
  disconnect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));
  while(!file.atEnd())
  {
    line = file.readLine().trimmed();
    uid = qHash(QDir(line).canonicalPath());
    item = items[uid][columns[0]["uid"].toString()];
    if(item!=NULL) item->setCheckState(Qt::Checked);
    else cerr<<line.toStdString()<<" not found."<<endl;
  }
  connect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));
  submit();
  file.close();
  return true;
}


projectModel::projectModel(confData *conf, const QString &path, const QString &columnsFile, QObject *parent)
             :QStandardItemModel(parent)
{
  data = conf;
  resultData = NULL;

  QDir dir(path);
  projectPath = dir.canonicalPath();
  columnsDataFile = columnsFile;
  dir.setFilter(QDir::NoDotAndDotDot | QDir::Dirs);

  load();
  
  connect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));
  connect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(updateItems(QStandardItem *)));
  //connect(&watcher,SIGNAL(fileChanged(const QString &)),this,SLOT(confChanged(const QString &)));
}

void projectModel::load()
{
  loadColumns(data->getDir("config") + "/2dx_merge/" + "projectMenu.inf");
  if(!QFileInfo(columnsDataFile).exists())
    saveColumns(columnsDataFile);
  loadHidden(columnsDataFile);
  
  initDir(projectPath);

  quint32 var;
  QStandardItem *item;
  foreach(var, columns.keys())
  {
    item = new QStandardItem(columns[var]["shortname"].toString());
    item->setData(columns[var]["tooltip"],Qt::ToolTipRole);
    setHorizontalHeaderItem(var,item);
  }
}

void projectModel::reload()
{
  clear();
  initDir(projectPath);

  quint32 var;
  QStandardItem *item;
  foreach(var, columns.keys())
  {
    item = new QStandardItem(columns[var]["shortname"].toString());
    item->setData(columns[var]["tooltip"],Qt::ToolTipRole);
    setHorizontalHeaderItem(var,item);
  }
}

QString projectModel::pathFromIndex(const QModelIndex& index)
{
  if(!index.isValid()) return QString();
  return paths[index.sibling(index.row(),0).data(Qt::UserRole + 1).toUInt()];
}

void projectModel::itemActivated(const QModelIndex& index)
{
  QString dirName = pathFromIndex(index);
  QProcess::startDetached(data->getApp("2dx_image"), QStringList()<<dirName);
}

QStringList projectModel::parentDirs()
{
  QStringList dirs;
  for(int i=0;i<rowCount();i++)
    dirs<<pathFromIndex(index(i,0));
  return dirs;
}

void projectModel::setSaveName(const QString &saveName)
{
  saveFileName = saveName;
}

bool projectModel::save(QStandardItem *currentItem, int itemCount, QFile &saveFile)
{
  QModelIndex i;
  if(currentItem==NULL) return false;
  foreach(i, match(currentItem->index(), Qt::CheckStateRole, QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked), itemCount, Qt::MatchRegExp))
  {
//    qDebug()<<pathFromIndex(i);
    if(i.isValid())
    if(i.child(0,0).isValid()) save(itemFromIndex(i.child(0,0)), itemFromIndex(i)->rowCount(), saveFile);
    else saveFile.write((pathFromIndex(i) + '\n').toAscii());
  }
  return true;
}

bool projectModel::submit()
{
  if(saveFileName.isEmpty()) return false;
  QFile saveFile(saveFileName);
  if(!saveFile.open(QIODevice::WriteOnly | QIODevice::Text)) return false;
  save(item(0), rowCount(), saveFile);
  saveFile.close();
  return true;
}

void projectModel::updateItems(QStandardItem *element)
{
  disconnect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(updateItems(QStandardItem *)));

  if(element->checkState() != Qt::PartiallyChecked)
    for(int i=0;i<element->rowCount();i++)
      element->child(i)->setCheckState(element->checkState());

  QStandardItem *parent = element->parent();

  if(parent != NULL)
  {
    bool checked = true;
    bool unchecked = true;

    for(int i=0;i<parent->rowCount();i++)
    {
      checked = checked && (parent->child(i)->checkState() == Qt::Checked);
      unchecked = unchecked && (parent->child(i)->checkState() == Qt::Unchecked);
    }

    if(checked && !unchecked) parent->setCheckState(Qt::Checked);
    else if(!checked && unchecked) parent->setCheckState(Qt::Unchecked);
    else parent->setCheckState(Qt::PartiallyChecked);
  }

  connect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(updateItems(QStandardItem *)));
}

QList<bool> projectModel::visibleColumns()
{
  QList<bool> v;
  foreach(quint32 value, columns.keys())
  {
    v<<columns[value]["visible"].toBool();
  }
  return v;
}

const QVariant &projectModel::getColumnProperty(int i, const QString &property)
{
  return columns[i][property.toLower()];
}

void projectModel::setColumnProperty(int i, const QString &property, const QVariant &value)
{
  columns[i][property.toLower()] = value;
}

void projectModel::setResultsFile(resultsData *resultsFile)
{
  if(resultData!=NULL) disconnect(resultData,SIGNAL(saved(bool)),this,SLOT(update()));
  resultData = resultsFile;
  connect(resultData,SIGNAL(saved(bool)),this,SLOT(update()));
}

void projectModel::update()
{
  if(resultData == NULL) return;

  QStandardItem *item;
  quint32 uid;

  QMapIterator<QString, QMap<QString, QString> > it(resultData->results);
  while(it.hasNext())
  {
    it.next();
    
    uid = qHash(QDir(it.key()).canonicalPath());

    QMapIterator<QString, QString> j(it.value());
    while(j.hasNext())
    {
      j.next();
      item = items[uid][j.key()];
      if(item != NULL && !j.key().contains(QRegExp("^##\\w*##$")))
      {
        if(item->checkState() != Qt::Checked && item->checkState() != Qt::PartiallyChecked)
          item->setText(j.value());
      }
    }
  }
}

void projectModel::maskResults()
{
  resultData->clearMasked();
  maskResults(item(0), rowCount());
}

void projectModel::maskResults(QStandardItem *currentItem, int itemCount)
{
  QModelIndex i,j;
  if(currentItem==NULL) return;
  foreach(i, match(currentItem->index(), Qt::CheckStateRole, ".*" /*QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked)*/, itemCount, Qt::MatchRegExp))
  {
    if(i.isValid())
    {
      if(i.child(0,0).isValid()) maskResults(itemFromIndex(i.child(0,0)), itemFromIndex(i)->rowCount());
      else
        for(int j = 2; j<columnCount(); j++)
          if(i.parent().child(i.row(),j).data(Qt::CheckStateRole) == Qt::Checked ||i.parent().child(i.row(),j).data(Qt::CheckStateRole) == Qt::PartiallyChecked)
            resultData->setMasked(pathFromIndex(i),columns[j]["uid"].toString());
    }
  }
}

void projectModel::invertSelection(bool commit)
{
  disconnect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));  
  changeSelection(item(0),rowCount(),"invert");
  if(commit) submit();  
	connect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));  
}

void projectModel::selectAll(bool commit)
{
  disconnect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));  
  changeSelection(item(0),rowCount(),"selectAll");
  if(commit) submit();
	connect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));    
}

void projectModel::clearSelection(bool commit)
{
  disconnect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));  
  changeSelection(item(0),rowCount(),"clear");
  if(commit) submit();
	connect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));    
}

bool projectModel::removeSelected()
{
  disconnect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));  
  changeSelection(item(0),rowCount(),"removeItems");
//  submit();
	connect(this,SIGNAL(itemChanged(QStandardItem *)),this,SLOT(submit()));    
  emit reloading();
   return true;
}

void projectModel::changeSelection(QStandardItem *currentItem, int itemCount, const QString &action)
{
  if(currentItem==NULL) return;
  QModelIndex i;
  QString checkAction = action.trimmed().toLower();
  QList<QModelIndex> list = match(currentItem->index(), Qt::CheckStateRole, ".*" /*QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked) + "|" + QString::number(Qt::Unchecked)*/, itemCount, Qt::MatchRegExp);
  foreach(i, list)
  {
    if(i.isValid())
    if(i.child(0,0).isValid()) changeSelection(itemFromIndex(i.child(0,0)), itemFromIndex(i)->rowCount(),action);
    else if(i.column() == 0)
    {
      if(checkAction == "invert")
      {
        if(i.data(Qt::CheckStateRole) == Qt::Checked) setData(i,Qt::Unchecked,Qt::CheckStateRole);
        else if(i.data(Qt::CheckStateRole) == Qt::Unchecked) setData(i,Qt::Checked,Qt::CheckStateRole);
      }
      else if(checkAction == "selectall")
      {
        if(i.data(Qt::CheckStateRole) != Qt::Checked) setData(i,Qt::Checked,Qt::CheckStateRole);
      }
      else if(checkAction == "clear")
        setData(i,Qt::Unchecked,Qt::CheckStateRole);
      else if(checkAction == "removeitems")
      {
        if(i.data(Qt::CheckStateRole) == Qt::Checked) 
        {
          QString path = pathFromIndex(i);
					qDebug()<<"Removing: "<<path;
          QProcess p;
          p.setStandardOutputFile(data->getDir("working") + "/config/out.out");          
          p.setStandardErrorFile(data->getDir("working") + "/config/out.err");
          p.start("rm -rf " + path);
          p.waitForFinished(30000);
          

///					QModelIndex parent = i.parent();

//					delete itemFromIndex(i);  
//          itemFromIndex(parent)->removeRow(i.row());
/*					if(parent.isValid() && !parent.child(0,0).isValid())
					{
            qDebug()<<"Removing: "<<path;
            QProcess p;
            p.setStandardOutputFile(data->getDir("working") + "/config/out.err");          
            p.setStandardErrorFile(data->getDir("working") + "/config/out.err");
            p.start("rm -rf " + path);
            p.waitForFinished(30000);            
					}
*/
				}
			}
		}
	}
}

void projectModel::currentRowChanged(const QModelIndex&i,const QModelIndex&)
{
	if(!i.sibling(i.row(),1).isValid()) return;
	//QString image = pathFromIndex(i) + "/" + i.sibling(i.row(),1).data().toString() + "-p1.mrc";
	QString image = pathFromIndex(i) + "/final_map.mrc";
	emit currentImage(image);
}

void projectModel::confChanged(const QString &path)
{
  qDebug()<<path;
}

