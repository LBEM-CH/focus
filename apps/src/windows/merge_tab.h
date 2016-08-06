/* 
 * File:   executionContainer.h
 * Author: biyanin
 *
 * Created on August 21, 2015, 11:30 AM
 */

#ifndef MERGETAB_H
#define	MERGETAB_H

#include <QWidget>
#include <QProcess>
#include <QDir>
#include <QSignalMapper>
#include <QGridLayout>
#include <QHBoxLayout>
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
#include <QStatusBar>
#include <QProgressBar>
#include <QToolBar>
#include <QPushButton>
#include <QListWidget>
#include <QListWidgetItem>

#include <confData.h>
#include <viewContainer.h>
#include <resizeableStackedWidget.h>
#include <scriptModule.h>
#include <confInterface.h>
#include <confModel.h>
#include <log_viewer.h>
#include <resultsModule.h>
#include <reprojectWindow.h>

#include "blockContainer.h"

class MergeTab : public QWidget
{
    Q_OBJECT

public:
    MergeTab(confData* data, resultsData *results, const QStringList& scriptDirs, const QList<scriptModule::moduleType>& moduleTypes, QWidget *parent = NULL);

public slots:
    
    void scriptChanged(scriptModule *module, QModelIndex index);
    void scriptCompleted(scriptModule *module, QModelIndex index);
    void subscriptActivated(QModelIndex item);
    
    void maximizeLogWindow(bool maximize);
    void maximizeParameterWindow(bool maximize);

    void reload();

    void launchFileBrowser();
    void launchLogBrowser();

    void showSubTitle(bool show);
    
    void execute(bool halt);
    void stopPlay();
    
    void updateScriptLabel(const QString& label);
    void increaseScriptProgress(int increament);
    void setScriptProgress(int progress);

signals:
    void scriptCompletedSignal();

private:
    
    blockContainer* setupLogWindow();
    blockContainer* setupParameterWindow();
    
    confData *mainData;

    scriptModule* defaultModule;
    QAction* defaultAction;

    QStackedWidget* scriptsWidget;
    QListView* subscriptWidget;
    
    resultsData *results;

    ParametersWidget *parameters;

    QSplitter* centralSplitter;
    QSplitter *centerRightSplitter;

    resultsModule *resultsView;

    LogViewer *logViewer;

    QComboBox* userLevelButtons;
    QComboBox* verbosityControl;
    
    QProgressBar* progressBar;
    
    QLabel* scriptLabel;
    QLabel* subTitleLabel;
    QPushButton* runButton;
    QPushButton* refreshButton;
    QPushButton* manualButton;

};


#endif	/* EXECUTIONCONTAINER_H */

