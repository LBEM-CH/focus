/* 
 * File:   executionContainer.h
 * Author: biyanin
 *
 * Created on August 21, 2015, 11:30 AM
 */

#ifndef EXECUTIONCONTAINER_H
#define	EXECUTIONCONTAINER_H

#include <QWidget>
#include <QProcess>
#include <QDir>
#include <QSignalMapper>
#include <QGridLayout>
#include <QSplitter>
#include <QDebug>
#include <QFile>
#include <QFileDialog>
#include <QDesktopServices>
#include <QStackedWidget>
#include <QComboBox>
#include <QToolButton>
#include <QSpacerItem>
#include <QScrollArea>
#include <QToolBar>
#include <confData.h>
#include <confManual.h>
#include <scriptProgress.h>
#include <viewContainer.h>
#include <resizeableStackedWidget.h>
#include <scriptModule.h>
#include <confInterface.h>
#include <confModel.h>
#include <LogViewer.h>
#include <controlBar.h>
#include <levelGroup.h>
#include <resultsModule.h>
#include <reprojectWindow.h>
#include <confEditor.h>

#include "blockContainer.h"

class executionContainer : public QWidget
{
    Q_OBJECT

public:
    executionContainer(confData* data, resultsData *results, QWidget *parent = NULL);

public slots:

    void setStandardMode();
    void setCustomMode();
    void setSPMode();
    
    void scriptChanged(scriptModule *module, QModelIndex index);
    void standardScriptChanged(QModelIndex index);
    void customScriptChanged(QModelIndex index);
    void singleParticleScriptChanged(QModelIndex index);

    void scriptCompleted(scriptModule *module, QModelIndex index);
    void standardScriptCompleted(QModelIndex index);
    void customScriptCompleted(QModelIndex index);
    void singleParticleScriptCompleted(QModelIndex index);

    //void maximizeWindow(int option);
    void maximizeLogWindow(bool maximize);
    void maximizeParameterWindow(bool maximize);

    void reload();

    void launchFileBrowser();
    void launchLogBrowser();

    void showManual(bool show);
    
    void updateFontInfo();
    
    void execute(bool halt);

signals:
    void scriptChangedSignal(const QString& scriptTitle);
    void scriptCompletedSignal();
    void progress(int progress);
    void incrementProgress(int);

private:
    
    blockContainer* setupLogWindow();
    blockContainer* setupParameterWindow();
    QToolBar* setupToolbar();
    
    void addToScriptsWidget(QWidget *widget);
    
    confData *mainData;

    scriptModule *standardScripts;
    scriptModule *customScripts;
    scriptModule *singleParticleScripts;

    QStackedWidget* scriptsWidget;

    QToolButton* showStandardScripts;
    QToolButton* showCustomScripts;
    QToolButton* showSPScripts;
    
    resultsData *results;

    confInterface *parameters;

    QSplitter* centralContainer;
    QSplitter *centerRightSplitter;

    resizeableStackedWidget *localParameters;
    QStackedWidget *manuals;

    resultsModule *resultsView;

    LogViewer *logViewer;

    QComboBox* userLevelButtons;
    QComboBox* verbosityControl;

    QHash<uint, int> localIndex;
    QHash<uint, int> manualIndex;

};


#endif	/* EXECUTIONCONTAINER_H */

