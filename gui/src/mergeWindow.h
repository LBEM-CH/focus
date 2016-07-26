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

#ifndef MERGEWINDOW_H
#define MERGEWINDOW_H

#include <QApplication>
#include <QDir>
#include <QPointer>
#include <QTreeView>
#include <QListWidget>
#include <QStandardItemModel>
#include <QSortFilterProxyModel>
#include <QSignalMapper>
#include <QTableView>
#include <QHeaderView>
#include <QGridLayout>
#include <QSplitter>
#include <QDebug>
#include <QFile>
#include <QFileDialog>
#include <QModelIndexList>
#include <QStatusBar>
#include <QProgressBar>
#include <QMessageBox>

#include "confData.h"
#include "resultsModule.h"
#include "libraryContainer.h"
#include "executionContainer.h"

class mergeWindow : public QWidget
{
    Q_OBJECT
    
public:
    mergeWindow(confData* data, QWidget *parent = NULL);
    
    projectModel* getDirModel();
    QTreeView* getDirView();
    
public slots:
    void imageLibraryDisplayed(bool show);
    void showSelected(bool show);
    void reload();
    void loadDirectorySelection();
    void itemActivated(const QModelIndex&);
    
signals:
    void imageActivated(const QString& imageDir);
    
private:

    confData *mainData;
    resultsData *results;  
    libraryContainer* albumCont;
    executionContainer* executionCont;

};

#endif
