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
#include "resultsModule.h"
using namespace std;

resultsModule::resultsModule(confData *conf, resultsData *resultsInfo, resultsModule::type moduleType, const QString &projectDir, QWidget *parent)
              :QWidget(parent)
{
  data = resultsInfo;
  config = conf;
  viewType = moduleType;
  mainDir = projectDir;
  
  showImportant = false;
  showFileNames = false;

  QGridLayout *layout = new QGridLayout(this);
  layout->setMargin(0);
  layout->setSpacing(0);
  setLayout(layout);

  view = new QTreeWidget;
  view->setIndentation(8);
  view->setTextElideMode(Qt::ElideLeft);
  connect(view,SIGNAL(currentItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)),this,SLOT(itemSelected(QTreeWidgetItem*)));
  connect(view,SIGNAL(itemActivated(QTreeWidgetItem*,int)),this,SLOT(itemActivated(QTreeWidgetItem*)));

  load();

  editor = new translator(config,config->getDir("home_2dx") + "/2dx.cfg", config->getDir("translatorsDir"));

  connect(data,SIGNAL(loaded(bool)),this,SLOT(load()));

  layout->addWidget(view);
}

void resultsModule::load()
{
  QTreeWidgetItem *item, *variable;
  QString title;
  view->clear();
  bool dryRun = data->dryRunMode();
  QMap<QString, QMap<QString, QString> > *map = NULL;
  if(viewType == resultsModule::results)
  {
    view->setHeaderLabels( QStringList() << "Parameter" << "Value");
    view->setColumnCount(2);    
    map = &data->results;
  }
  else if(viewType == resultsModule::images)
  {
    view->setColumnCount(1);
    view->header()->hide();
    map = &data->images;
  }
    QMapIterator<QString, QMap<QString, QString> > i(*map);
    while(i.hasNext())
    {
      i.next();
      if(!i.value().isEmpty() && i.value().size() >= 1 )
      {
        title = i.key();
        title.remove(mainDir);
        if(title.contains(QRegExp("merge/*$")))
          item = NULL;
        else
          item = new QTreeWidgetItem(view, QStringList()<<title);
        QMapIterator<QString, QString> j(i.value());
        while(j.hasNext())
        {
          j.next();
          if(!j.key().contains(QRegExp("^##\\w*##$")))
          {
            if(viewType == resultsModule::results)
            {
              if(item!=NULL)
                variable = new QTreeWidgetItem(item, QStringList()<<j.key()<<j.value());
              else
                variable = new QTreeWidgetItem(view, QStringList()<<j.key()<<j.value());
              if(data->masked(i.key(),j.key()) || dryRun)
              {
                variable->setForeground(0,Qt::red);
                variable->setForeground(1,Qt::red);
                item->setForeground(0,QColor(150,0,0));
              }
            }
            else if(viewType == resultsModule::images)
            {
              QString fileBaseName = j.key();
              fileBaseName.remove(QRegExp("<<@\\d+>>"));
              bool important = false;
              QString itemName,fileName,toolTipText;
              itemName = j.value();
              if(itemName.startsWith("<<@important>>")) 
              {
               important = true; 
               itemName.remove("<<@important>>");
              }
              toolTipText = fileBaseName;
              if(itemName.trimmed().isEmpty() || showFileNames) itemName = fileBaseName;
              QString path;
              
              path = QDir(i.key() + "/" + fileBaseName).canonicalPath();
                 
              if(!path.isEmpty())
              {
                if(!showImportant || (showImportant && important))
                {
                  if(item!=NULL)
                    variable = new QTreeWidgetItem(item,QStringList()<<itemName);
                  else
                    variable = new QTreeWidgetItem(view,QStringList()<<itemName);
                  if(important) variable->setForeground(0,QColor(198,25,10));
                  variable->setToolTip(0,toolTipText);
                  fullPath.insert(variable,path);
                }
              }
              else
                qDebug()<<i.key() + "/" + fileBaseName<<" does not exist!";
              if(item!=NULL && item->childCount() == 0) item->setHidden(true);
            }
          }
        }
      }
    }
  view->expandAll();
  resetColumnSize();
}

void resultsModule::itemSelected(QTreeWidgetItem *item)
{
  emit resultChanged(fullPath[item]);
}

void resultsModule::itemActivated(QTreeWidgetItem *item)
{
  if(viewType == resultsModule::images)
  {
    QString result = fullPath[item];
    editor->open(result);
  }
}

void resultsModule::setImportant(int value)
{
	showImportant = (bool)value;
	load();
}

void resultsModule::setImportant(bool value)
{
	showImportant = value;
	load();
}

void resultsModule::setShowFilenames(int value)
{
	showFileNames = (bool)value;
	load();
}

void resultsModule::resetColumnSize()
{
  for(int col=0; col<view->columnCount(); col++)
  {
      view->resizeColumnToContents(col);
  }
}