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

#ifndef RESULTSDATA_H
#define RESULTSDATA_H

#include <QFile>
#include <QFileSystemWatcher>
#include <QHash>
#include <QSet>
#include <QVariant>
#include <QStringList>
#include <QDebug>
#include <confData.h>
#include <fileWatcher.h>

class resultsData : public QObject
{
  Q_OBJECT

  public slots:
  bool load(const QString &fileName);
  bool load();
  bool save();
  void setDryRunMode(bool value);

  signals:
  void loaded(bool successful);
  void saved(bool successful);

  private:

  confData *data;
  
  fileWatcher watcher;

  QString mainDir;
  QString fileName;
  bool dryRun;

  void printValues();

  QSet<quint32> maskedResults;

  quint32 maskHash(const QString &directory, const QString &variable);

  public:
  QMap<QString, QMap<QString, QString> > results;
  QMap<QString,QMap<QString, QString> > images;

  resultsData(confData *conf, const QString &fileName, const QString &defaultDir = "@default", QObject *parent = NULL);
  void setMasked(const QString &directory, const QString &variable);
  bool masked(const QString &directory, const QString &variable);
  void clearMasked();
  bool dryRunMode();
};

#endif
