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

#include "resultsFile.h"
#include <iostream>
using namespace std;

resultsFile::resultsFile(QString file, QObject *parent)
            :QObject(parent)
{
  fileName = file;
  // if(!load()) cerr<<"Couldn't open "<<file.toStdString()<<endl;
}

bool resultsFile::load()
{
  clear();
  QFile dataFile(fileName);
  if(!dataFile.open(QIODevice::ReadOnly | QIODevice::Text)) return false;

  QString line;
  while(!dataFile.atEnd())
  {
    line = dataFile.readLine();
    line = line.remove('#').trimmed();
    if(line.toLower().startsWith("image:"))
    {
      QStringList cell = line.split(':');
      QString imageName = cell[1].simplified();
      images<<imageName;
    }

    if(line.toLower().startsWith("warning:"))
    {
//      QStringList cell = line.split(':');
      QString warning = line;
      warning.remove(0,8);
      warnings<<warning.simplified();
    }
  }

  dataFile.close();
  emit loaded();
  return true;
}

void resultsFile::clear()
{
  images.clear();
  warnings.clear();
}

const QStringList &resultsFile::imageList()
{
  return images;
}

const QStringList &resultsFile::warningList()
{
  return warnings;
}
