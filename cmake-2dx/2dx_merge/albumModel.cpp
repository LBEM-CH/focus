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

#include <albumModel.h>
#include <iostream>
using namespace std;

void albumModel::init(const QString &path)
{
  QDir dir(path);
  QStringList images = dir.entryList(QStringList()<<"final_map.mrc", QDir::Files | QDir::Readable);
  QStringList dirs = dir.entryList(QStringList()<<"*", QDir::Dirs | QDir::NoDotAndDotDot | QDir::Readable);

  QString location;
  foreach(QString image, images)
  {
    location = QDir(path + "/" + image).absolutePath();
    locations<<location;
    confData localData(path + "/" + "2dx_image.cfg");
    names<<localData.get("imagenumber","value");
    mrcImage thumb(location,true);
    pixmaps<<thumb.getPixmap();
  }

  foreach(QString subDir, dirs)
  {
    init(path + "/" + subDir);
  }
}

albumModel::albumModel(const QString &path, QObject *parent)
          : QAbstractListModel(parent)
{
  rootPath = path;
  init(path);
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
  init(rootPath);
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

