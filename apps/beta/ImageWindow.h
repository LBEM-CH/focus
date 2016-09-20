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
#include <QGridLayout>
#include <QStackedWidget>
#include <QComboBox>
#include <QToolButton>
#include <QToolBar>
#include <QStatusBar>
#include <QSlider>
#include <QSplitter>

#include "ParameterConfiguration.h"
#include "BlockContainer.h"
#include "ParameterWidget.h"
#include "ScriptModule.h"
#include "ResultsParser.h"
#include "ImagePreview.h"
#include "LogViewer.h"
#include "StatusViewer.h"
#include "ScriptParser.h"

class ImageWindow : public QWidget {
    
    Q_OBJECT
    
public:
    ImageWindow(ParametersConfiguration *data, const QString& workDir, QWidget *parent = NULL);   
    ParametersConfiguration* getConf();

public slots:

    void scriptChanged(ScriptModule *module, QModelIndex index);
    void standardScriptChanged(QModelIndex index);
    void customScriptChanged(QModelIndex index);
    void subscriptActivated(QModelIndex item);

    void scriptCompleted(ScriptModule *module, QModelIndex index);
    void standardScriptCompleted(QModelIndex index);
    void customScriptCompleted(QModelIndex index);

    void setStandardMode();
    void setCustomMode();

    bool parseResults(ParametersConfiguration *conf, const QString &results);
    bool parseResults();

    void bridgeScriptLogConnection(bool bridge);

    void showSubTitle(bool show);

    void execute(bool halt);
    void reload();
    void refresh();
    void revert();
    void stopPlay();
    
    void save();
    bool modified();

    void launchLogBrowser();
    void launchFileBrowser();

    void toggleHistoryView(bool show);

    void updateScriptLabel(const QString& label);
    void setScriptProgress(int progress);

    void maximizeLogWindow(bool maximize);
    void maximizeParameterWindow(bool maximize);
    
    void saveAsProjectDefault();
    
    bool isRunningScript();

signals:
    void executing(bool);
    void scriptCompletedSignal();
    
private:
    BlockContainer* setupLogWindow();
    BlockContainer* setupParameterWindow();
    QToolBar* setupToolbar();

    QString workingDir;
    ParametersConfiguration *data;

    QStackedWidget* scriptsWidget;
    QListView* subscriptWidget;
    
    ScriptModule *standardScripts;
    ScriptModule *customScripts;

    QToolButton* showStandardScripts;
    QToolButton* showCustomScripts;

    ImagePreview *preview;

    BlockContainer *logWindow;
    LogViewer *logViewer;

    ResultsParser *results;
    ResultsParser *imageParser;
    QString currentResults;
    QString currentLog;

    QSlider* verbosityControl;

    StatusViewer *statusParser;

    ParametersWidget *parameters;
    QLineEdit* parameterSearchBox;

    BlockContainer *parameterContainer;

    QSplitter* centralSplitter;
    QSplitter *centerRightSplitter;

    QHash<QString, bool> visible;

    int runningTabIndex = -1;
    
    QProgressBar* progressBar;
    
    QLabel* scriptLabel;
    QLabel* subTitleLabel;
    QPushButton* runButton;
    QPushButton* refreshButton;
    QPushButton* saveCfgButton;
    QPushButton* manualButton;

};

#endif
