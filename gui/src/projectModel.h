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

#ifndef PROJECTMODEL_H
#define PROJECTMODEL_H

#include <QStandardItemModel>
#include <QItemSelectionModel>
#include <QProcess>
#include <QDir>
#include <QFileDialog>
#include <QFileSystemWatcher>
#include <QCheckBox>
#include <QStyle>
#include <QDebug>
#include <QModelIndexList>
#include <QProgressDialog>
//#include <imageItem.h>
#include <confData.h>
#include <resultsData.h>
#include <mrcImage.h>

class projectModel : public QStandardItemModel
{
  Q_OBJECT

  public slots:
  void itemActivated(const QModelIndex& index);
  void itemSelected(const QModelIndex &index);
  void itemDeselected(const QModelIndex &index);
  void changeItemCheckedRole(const QModelIndex &index, bool check = true);
  void currentRowChanged(const QModelIndex&,const QModelIndex&) ;
  void confChanged(const QString &path);
  bool removeSelected();

  bool save(QStandardItem *currentItem, int itemCount, QFile &saveFile);
  void maskResults();
  bool submit();

  void updateItems(QStandardItem *element);
  void update();

  void reload();
  
  void invertSelection(bool commit = true);
  void selectAll(bool commit = true);
  void clearSelection(bool commit = true);
  void changeSelection(QStandardItem *currentItem, int itemCount, const QString &action = QString());
  void autoSelection(QStandardItem *currentItem, int itemCount, int minTilt, int maxTilt, bool useAbsolute);
  void autoSelect(int minTilt, int maxTilt, bool useAbsolute);
  bool loadSelection(const QString &fileName = "");  
  bool loadHidden(const QString &columnsFile);
  bool saveColumns();
  bool saveColumns(const QString &columnsFile);
  
  signals:
  void currentImage(const QString&);
  void reloading();
  void submitting();
  
  private:

  confData *data;
  resultsData *resultData;
  QFileSystemWatcher watcher;
  
  QProgressDialog* loadDialog;
  int loadProgress;
  
  QHash<quint32,QString> paths;
  QMap<quint32, QHash<QString,QVariant> > columns;
  QMap<QString, quint32> parameterToColId; 
  QHash<quint32, QHash<QString,QStandardItem*> >items;

  QString projectPath;
  QString columnsDataFile;
  QStringList columnsFileHeader;
  QString saveFileName;
  QStringList labels;
  
  QModelIndex currentIndex;

  bool loadColumns(const QString &columnsFile);
  void initDir(const QString &path, QStandardItem *parent = NULL);
  

  void load();
  
  void maskResults(QStandardItem *currentItem, int itemCount);
  uint uid(const QString & path);


  void getSelection(QStandardItem *currentItem, int itemCount, QStringList & selected);

  public:

  enum projectModelSortRole {SortRole = Qt::UserRole + 2};

  projectModel(confData *conf, const QString &path, const QString &columnsFile, QObject *parent = NULL);

  void setSaveName(const QString &saveName);
  void setResultsFile(resultsData *resultsFile);

  QString pathFromIndex(const QModelIndex &index);
  QString relativePathFromIndex(const QModelIndex& index);
  QStringList getSelectionNames();
  QString getProjectPath() const; 
  QStringList parentDirs();
  QList<bool> visibleColumns();

  const QVariant &getColumnProperty(int i, const QString &property);
  void setColumnProperty(int i, const QString &property, const QVariant &value);
  
  QVariant getCurrentRowParameterValue(const QString& parameter);
  bool isCurrentRowValidImage();
};

#endif

