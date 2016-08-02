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

#ifndef RESULTSFILE_H
#define RESULTSFILE_H

#include <QFile>
#include <QStringList>

class resultsFile : public QObject
{

  Q_OBJECT

  public slots:

  bool load();
  void clear();

  signals:

  void loaded();

  private:

  QString fileName;
  QStringList images, warnings;

  public:
  resultsFile(QString file, QObject *parent = NULL);
  const QStringList &imageList();
  const QStringList &warningList();
};

#endif
