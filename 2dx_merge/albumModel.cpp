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

#include "albumModel.h"
#include <iostream>
using namespace std;

void albumModel::init(const projectModel *pModel)
{
   
  if(pModel)
  {
    QString path = pModel->getProjectPath();
   
    QStringList imageMaps;
    getImagePaths(path,QStringList()<<"final_map.mrc", imageMaps);
  }
}

void albumModel::getImagePaths(QString path, const QStringList &nameFilter, QStringList &list)
{
    QDir dir(path);
    QStringList images = dir.entryList(nameFilter, QDir::Files | QDir::Readable);
    foreach(QString image, images)
    {
      //qDebug()<<" " << image;
      QString location = QDir(path+ "/" + image).absolutePath();
      locations<<location;
      confData localData(path + "/" + "2dx_image.cfg");
      names<<localData.get("imagenumber","value");
      qDebug()<<" " << localData.get("imagenumber","value");
      mrcImage thumb(location,true);
      pixmaps<<thumb.getPixmap();
    }
    QStringList subdirs = dir.entryList(QStringList()<<"*", QDir::Dirs | QDir::NoDotAndDotDot | QDir::Readable);
    foreach(QString subdir, subdirs)
    {
        //qDebug()<<" " << subdir;
        getImagePaths(path+"/"+subdir, nameFilter, list);
    }
}

albumModel::albumModel(const projectModel *pModel, QObject *parent)
          : QAbstractListModel(parent)
{
  qDebug()<<"albumModel::albumModel";
  if(pModel != NULL)
  {
    project = pModel;
    rootPath = pModel->getProjectPath();
    init(pModel);
  }
}

int albumModel::rowCount(const QModelIndex &parent) const
{
  if(parent.isValid()) return 0;
  else return locations.size();
}

void albumModel::clear()
{
  locations.clear();
  names.clear();
  pixmaps.clear();
}

void albumModel::reload()
{
  clear();
  init(project);
}

QModelIndex albumModel::index(const QString &path)
{
  int l = locations.indexOf(QDir(path).absolutePath());
/*  if(l!=-1)
    return index(l);
  else
    return QModelIndex;
*/
  return QModelIndex();
}

QVariant albumModel::data(const QModelIndex &index, int role) const
{
  if(!index.isValid()) return QVariant();
  if(role == Qt::DecorationRole)
  {
    return QIcon(pixmaps.value(index.row()));//.scaled(100,100,Qt::KeepAspectRatio, Qt::SmoothTransformation));
  }
  if(role == Qt::DisplayRole)
    return names[index.row()];//index.row(); QFileInfo(locations[index.row()]).fileName();
  if(role == Qt::UserRole+1)
    return locations[index.row()];
  return QVariant();
}

