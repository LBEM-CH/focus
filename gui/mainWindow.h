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

#include <confData.h>
#include <viewContainer.h>
#include <resizeableStackedWidget.h>
#include <scriptModule.h>
#include <confInterface.h>
#include <confModel.h>
#include <confDelegate.h>
#include <projectDelegate.h>
#include <LogViewer.h>
#include <resultsModule.h>
#include <projectModel.h>
#include <imagePreview.h>
#include <eulerWindow.h>
#include <reprojectWindow.h>
#include <aboutWindow.h>
#include <updateWindow.h>
#include <confEditor.h>

#include "libraryContainer.h"
#include "mergeContainer.h"
#include "imageContainer.h"

class mainWindow : public QMainWindow
{
    Q_OBJECT
public:
    mainWindow(const QString &directory, QWidget *parent = NULL);

public slots:

    void editHelperConf();

    void setSaveState(bool state);

    void showEuler(bool show = true);
    void showReproject(bool show = true);

    void import();
    void importFiles(const QHash<QString, QHash<QString, QString> > &imageList);
    void importFile(const QString & fileName, const QHash<QString, QString> &imageCodes);
    void importFinished();
    void open();
    void openURL(const QString &url);
    void toggleAutoSave();

    void launchEuler();
    void launchReproject();

    void showImageWindow(const QModelIndex&);
    
signals:
    void saveConfig();

protected:
    void closeEvent(QCloseEvent *event);
    
private:
    QWidget *setupConfView(confData *data);
    bool setupIcons(confData *data, const QDir &directory);
    void setupActions();
    void setupMenuBar();
    void setupWindows();
    void setupToolBar();
    confData* setupMainConfiguration(const QString &directory);
    
    bool createDir(const QString &dir);
    
    /*
     * All configuration data
     */
    confData *mainData;
    confData *userData;
    resultsData *results; 

    QProcess importProcess;

    updateWindow *updates;
    aboutWindow *about;

    QString installedVersion;

    eulerWindow *euler;
    reprojectWindow *reproject;
    
    QStackedWidget* centralWin_;
    libraryContainer* libraryWin_;
    mergeContainer* mergeWin_;
    imageContainer* imageWin_;
    
    

    /**
     * Standard actions
     */
    QAction* openAction;
    QAction* saveAction;
    QAction* importAction;
    QAction* viewAlbum;
    
    QAction* openLibraryWindowAct;
    QAction* openImageWindowAct;
    QAction* openMergeWindowAct;

    int importCount;

    bool m_do_autosave;
    QTimer *timer;
    int timer_refresh;

};

#endif
