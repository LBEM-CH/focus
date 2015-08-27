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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QMenuBar>
#include <QAction>
#include <QFileDialog>
#include <QDir>
#include <QMessageBox>
#include <QProcess>
#include <QToolBar>
#include <confData.h>
#include <centralWindow.h>
#include <updateWindow.h>
#include <aboutWindow.h>
#include <confEditor.h>

class mainWindow : public QMainWindow
{
  Q_OBJECT

  public slots:
  void open();
  void save();
  void saveAsProjectDefault();
  void saveAsTiltRangeDefault();  
  void saveAs();
  void increaseFontSize();
  void decreaseFontSize();
  void updateAutoSave();
  void revert();
  void showUpdates();
  void editHelperConf();
  void toggleAutoSave();
  void openURL(const QString &url);
  void stopPlay();

  private:

  updateWindow *updates;
  aboutWindow *about;

  centralWindow *centerWin;
  QHash<QString,QAction *> actionList;
  QAction *openAction;
  QAction *saveAction;
  QAction *saveAsDefaultAction;
  QAction *saveAsProjectDefaultAction;
  QAction *saveAsAction;
  QAction *autoSave;
  QAction *closeAction;
  QAction *increaseFontAction;
  QAction *decreaseFontAction;
  QAction *revertAction;
  QAction *showUpdatesAction;
  QAction *showAboutAction;
  QAction* playAction;
  QAction* refreshAction;
  QAction* manualAction;

  void setupActions();
  void setupToolBar();
  void setupMenuBar();

  confData *data;
  confData *userData;
  
  bool m_do_autosave;
  QTimer *timer;
  int timer_refresh;


  confData *getUserConf(const QString &fileName);
  bool setupIcons(confData *data, const QDir &directory);

  public:
  mainWindow(char *dirArg);

  bool createDir(const QString &dir);

  protected:
  void closeEvent(QCloseEvent *event);
};

#endif
