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

#ifndef RESULTSMODULE_H
#define RESULTSMODULE_H

#include <QGridLayout>
#include <QProcess>
#include <QTreeWidget>
#include <QHeaderView>
#include <QStandardItem>
#include <QDebug>
#include <resultsData.h>
#include <confData.h>
#include <translator.h>

class resultsModule : public QWidget
{
  Q_OBJECT

  public:
  enum type{results, images};

  public slots:
  void load();
  void itemSelected(QTreeWidgetItem *item);
  void itemActivated(QTreeWidgetItem *item);
  void setImportant(int);
  void setShowFilenames(int);
  void resetColumnSize();

  signals:
  void resultChanged(const QString &result);

  private:
  confData *config;
  resultsData *data;
  QTreeWidget *view;
  type viewType;
  
  bool showImportant, showFileNames;

  translator *editor;

  QHash<QTreeWidgetItem*,QString> fullPath;

  QString mainDir;

  public:
  resultsModule(confData *conf, resultsData *resultsInfo, resultsModule::type moduleType = resultsModule::results, const QString &projectDir = "", QWidget *parent = NULL);
};

#endif
