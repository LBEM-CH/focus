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

#ifndef CENTRALWINDOW_H
#define CENTRALWINDOW_H

#include <QWidget>
#include <QWebView>
#include <QGridLayout>
#include <confData.h>
#include <viewContainer.h>
#include <confInterface.h>
#include <scriptModule.h>
#include <controlActionsGroup.h>
#include <resultsParser.h>
#include <imagePreview.h>
#include <levelGroup.h>
#include <LogViewer.h>
#include <statusViewer.h>
#include <confManual.h>
#include <resizeableStackedWidget.h>
#include <warningBox.h>
#include <scriptParser.h>

class centralWindow : public QWidget
{
  Q_OBJECT

  public slots:

  void scriptChanged(scriptModule *module, QModelIndex index);
  void standardScriptChanged(QModelIndex index);
  void customScriptChanged(QModelIndex index);

  void scriptCompleted(scriptModule *module, QModelIndex index);
  void standardScriptCompleted(QModelIndex index);
  void customScriptCompleted(QModelIndex index);

  void runningScriptChanged(scriptModule *module, QModelIndex index);
  void customRunningScriptChanged(QModelIndex index);
  void standardRunningScriptChanged(QModelIndex index);

  bool parseResults(confData *conf, const QString &results);
  bool parseResults();
  
  void bridgeScriptLogConnection(bool bridge);

  void showManual(bool show);
  
  void useNewViewer(bool enable);

  void reload();
  void refresh();
  void revert();

  void viewHelp();
  void reportBug();
  void launchLogBrowser();

  void updateFontInfo();
  
  void toggleHistoryView(bool show);

  signals:

  void fontInfoUpdated();

  private:
  QString workingDir;
  confData *data;

  QList<controlActionsGroup *> headers;

  QHash<uint,int> localIndex;
  QHash<uint,int> manualIndex;

  scriptModule *standardScripts;
  scriptModule *customScripts;

  imagePreview *preview;

  LogViewer *logViewer;

  resultsParser *results;
  resultsParser *imageParser;
  warningBox *warningWindow;
  QString currentResults;
  QString currentLog;
  
  levelGroup *verbosityControl;

  statusViewer *statusParser;

  confInterface *parameters;
  resizeableStackedWidget *localParameters;
  QStackedWidget *manuals;
  levelGroup *userLevelButtons;

  QWidget *parametersWidget;
  viewContainer *parameterContainer;
  
  QHash<QString,bool> visible;

  public:
  centralWindow(confData *data, QWidget *parent = NULL);

};

#endif
