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

#ifndef IMAGEWINDOW_H
#define IMAGEWINDOW_H

#include <QWidget>
#include <QWebView>
#include <QGridLayout>
#include <QStackedWidget>
#include <QComboBox>
#include <QToolButton>
#include <QToolBar>
#include <QStatusBar>
#include <confData.h>
#include <blockContainer.h>
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

class imageWindow : public QWidget {
    Q_OBJECT

public slots:

    void scriptChanged(scriptModule *module, QModelIndex index);
    void standardScriptChanged(QModelIndex index);
    void customScriptChanged(QModelIndex index);
    void subscriptActivated(QModelIndex item);

    void scriptCompleted(scriptModule *module, QModelIndex index);
    void standardScriptCompleted(QModelIndex index);
    void customScriptCompleted(QModelIndex index);

    void runningScriptChanged(scriptModule *module, QModelIndex index);
    void customRunningScriptChanged(QModelIndex index);
    void standardRunningScriptChanged(QModelIndex index);

    void setStandardMode();
    void setCustomMode();

    bool parseResults(confData *conf, const QString &results);
    bool parseResults();

    void bridgeScriptLogConnection(bool bridge);

    void showSubTitle(bool show);

    void useNewViewer(bool enable);

    void execute(bool halt);
    void reload();
    void refresh();
    void revert();
    void stopPlay();

    void launchLogBrowser();
    void launchFileBrowser();

    void updateFontInfo();

    void toggleHistoryView(bool show);

    void updateScriptLabel(const QString& label);
    void increaseScriptProgress(int increament);
    void setScriptProgress(int progress);

    void maximizeLogWindow(bool maximize);
    void maximizeParameterWindow(bool maximize);

signals:
    void executing(bool);
    void scriptCompletedSignal();
    void fontInfoUpdated();

private:
    blockContainer* setupLogWindow();
    blockContainer* setupParameterWindow();
    QToolBar* setupToolbar();

    QString workingDir;
    confData *data;

    QHash<uint, int> localIndex;

    QStackedWidget* scriptsWidget;
    QListView* subscriptWidget;
    
    scriptModule *standardScripts;
    scriptModule *customScripts;

    QToolButton* showStandardScripts;
    QToolButton* showCustomScripts;

    imagePreview *preview;

    blockContainer *logWindow;
    LogViewer *logViewer;

    resultsParser *results;
    resultsParser *imageParser;
    //warningBox *warningWindow;
    QString currentResults;
    QString currentLog;

    QComboBox* verbosityControl;

    statusViewer *statusParser;

    confInterface *parameters;
    resizeableStackedWidget *localParameters;
    QComboBox* userLevelButtons;

    QWidget *parametersWidget;
    blockContainer *parameterContainer;

    QSplitter* centralSplitter;
    QSplitter *centerRightSplitter;

    QHash<QString, bool> visible;

    QProgressBar* progressBar;
    
    QLabel* scriptLabel;
    QLabel* subTitleLabel;
    QPushButton* runButton;
    QPushButton* refreshButton;
    QPushButton* manualButton;

public:
    imageWindow(confData *data, QWidget *parent = NULL);

};

#endif
