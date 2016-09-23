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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QApplication>
#include <QDir>
#include <QMainWindow>
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
#include <QMap>
#include <QStringList>
#include <QMenuBar>
#include <QInputDialog>

#include "ParameterConfiguration.h"
#include "ScriptModule.h"
#include "ParameterWidget.h"
#include "ProjectDelegate.h"
#include "LogViewer.h"
#include "ResultsModule.h"
#include "ProjectModel.h"
#include "AboutWindow.h"
#include "UpdateWindow.h"
#include "LibraryTab.h"
#include "ExecutionWindow.h"
#include "ImageTab.h"
#include "PreferencesDialog.h"
#include "ImageLibrary.h"

class MainWindow : public QMainWindow
{
    Q_OBJECT
public:
    MainWindow(const QString& projectPath, QWidget *parent = NULL);

public slots:

    void editHelperConf();

    void open(const QString& projectPath="");
    void save();

    void showImageWindow(const QModelIndex&, bool supressWarnings=false);
    void showImageWindow(const QString&, bool supressWarnings=false);
    
    void changeProjectName();
    void updateWindowTitle();
    
signals:
    void saveConfig();

protected:
    void closeEvent(QCloseEvent *event);
    
private:
    void setupMenuBar();
    void setupWindows();
    void setupToolBar();
    
    QToolButton* setupMainNavigationButton(const QString& icon, const QString& title, const QString& desc, bool checkable = true, QWidget* connectedWidget = 0);

    void reallyClose(QCloseEvent *event);

    
    ResultsData* results;

    UpdateWindow *updates;
    AboutWindow *about;
    PreferencesDialog* preferencesDialog_;
    
    QStackedWidget* centralWin_;
    ImageLibrary* imageLibrary;
    LibraryTab* libraryWin_; 
    ImageTab* imageWin_;
    ExecutionWindow* mergeWin_;
    ExecutionWindow* spWin_;
    ExecutionWindow* projectToolsWin_;
    
    QToolButton* openLibraryWindowAct;
    QToolButton* openImageWindowAct;
    QToolButton* openMergeWindowAct;
    QToolButton* openSPWindowAct;
    QToolButton* openProjectToolsAct;

    bool preferencesDialogInit_ = false;
};

#endif
