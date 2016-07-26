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

#ifndef RESULTSPARSER_H
#define RESULTSPARSER_H

#include <QTableWidget>
#include <QListWidget>
#include <QHeaderView>
#include <QGridLayout>
#include <QScrollBar>
#include <QFile>
#include <QFileInfo>
#include <QFileSystemWatcher>
#include <QLabel>
#include <QButtonGroup>
#include <QPushButton>
#include <QAction>
#include "confData.h"

class resultsParser : public QTableWidget
{
  Q_OBJECT

  public slots:
  void load();
  void selectImage(QTableWidgetItem * item, QTableWidgetItem* previous);
  void updateFontInfo();
  void openFile();
  void resizeContents();
  void setImportant(int value);
  void setImportant(bool value);
  void setShowFilenames(int value);
  void setResult(const QString &results);
  void refreshWatcher();

  signals:
  void imageSelected(const QString &image);

  public:
  enum type {results,images,initialization};

  private:
  QGridLayout *layout;
  
  QFileSystemWatcher watcher;

  QStringList titles;

  confData *conf;
  QStringList resultsList;
  type view;
  bool important;
  bool showFilenames;

  QList<QTableWidgetItem *> output;
  int loadResults(resultsParser::type viewType);


  public:
  resultsParser(confData *data, QStringList results, resultsParser::type viewType, QWidget *parent = NULL);

  ~resultsParser();

  protected:
  void resizeEvent(QResizeEvent *event);

};

#endif
