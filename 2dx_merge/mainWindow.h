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
#include <confData.h>
#include <confManual.h>
#include <scriptProgress.h>
#include <viewContainer.h>
#include <resizeableStackedWidget.h>
#include <scriptModule.h>
#include <confInterface.h>
#include <confModel.h>
#include <confDelegate.h>
#include <projectDelegate.h>
#include <LogViewer.h>
#include <controlBar.h>
#include <levelGroup.h>
#include <resultsModule.h>
#include <projectModel.h>
#include <imagePreview.h>
#include <imageAlbum.h>
#include <eulerWindow.h>
#include <reprojectWindow.h>
#include <importBox.h>
#include <importTool.h>
#include <aboutWindow.h>
#include <updateWindow.h>
#include <autoImportTool.h>
#include <confEditor.h>
#include <scriptTab.h>

class mainWindow : public QMainWindow
{
  Q_OBJECT

  public slots:

  void tabChanged(int currentIndex);
      
  void scriptChanged(scriptModule *module, QModelIndex index);
  void standardScriptChanged(QModelIndex index);
  void customScriptChanged(QModelIndex index);
  void singleParticleScriptChanged(QModelIndex index);
  void scriptLaunched(scriptModule *module, QModelIndex index);
  void scriptCompleted(scriptModule *module, QModelIndex index);
  void standardScriptCompleted(QModelIndex index);
  void customScriptCompleted(QModelIndex index);
  void singleParticleScriptCompleted(QModelIndex index);
  void saveProjectState();
  void loadProjectState();
  void editHelperConf();
  void launchLogBrowser();

  void setSaveState(bool state);
  
  void showAlbum(bool show = true);
  void showEuler(bool show = true);
  void showReproject(bool show = true);

  void columnActivated(int i);

  void extendSelection();
  void reduceSelection();
  void copyImage();

  void import();
  void autoImport();
  void importFiles(const QHash<QString, QHash<QString,QString> > &imageList);
  void importFile(const QString & fileName, const QHash<QString, QString> &imageCodes);
  void importFinished();
  void updateModel();
  void updateFontInfo();
  void increaseFontSize();
  void decreaseFontSize();
  void open();
  void reload();
  void openURL(const QString &url);
  void toggleAutoSave();

  void launchAlbum(const QString &path);
  void launchEuler();
  void launchReproject();
  void launchFileBrowser();
  void hideManual(bool hide);
  void saveDirectorySelection();
  void loadDirectorySelection();
  void showSelected(bool enable);
  void execute(bool halt);

  signals:
  void saveConfig();

  private:
  confData *mainData;
  confData *userData;

  resultsData *results;

  QProcess importProcess;

  imagePreview *preview;

  QPointer<autoImportTool> autoImportMonitor;

  projectModel *dirModel;
  QTreeView *dirView;

  updateWindow *updates;
  aboutWindow *about;

  QString installedVersion;

  imageAlbum *album;
  eulerWindow *euler;
  reprojectWindow *reproject;

  scriptModule *standardScripts;
  scriptModule *customScripts;
  scriptModule *singleParticleScripts;
  
  scriptTab *standardScriptsTab;
  scriptTab *customScriptsTab;
  scriptTab *singleParticleScriptsTab;
  
  QTabWidget* scriptsWidget;

  resultsModule *resultsView;
  QSortFilterProxyModel *sortModel;

  LogViewer *logViewer;

  graphicalButton *saveButton;
  graphicalButton *playButton;
  scriptProgress *progressBar;
  graphicalButton *updateButton;
  graphicalButton *manualButton;

  levelGroup *verbosityControl;

  QHash<uint,int> localIndex;
  QHash<uint,int> manualIndex;
  
  int importCount;
  
  bool m_do_autosave;
  QTimer *timer;
  int timer_refresh;

  QWidget *setupHeader();
  QWidget *setupFooter();
  QWidget *setupDirectoryView(const QDir &dir, const QString &savePath = "");
  QWidget *setupConfView(confData *data);
  bool setupIcons(confData *data, const QDir &directory);
  void setupActions();
  
  void initializeDirectory();
  bool createDir(const QString &dir);
  void modifySelection(bool select = true);
  
  public:
  mainWindow(const QString &directory, QWidget *parent = NULL);
  
  protected:
  void closeEvent(QCloseEvent *event);
};

#endif
