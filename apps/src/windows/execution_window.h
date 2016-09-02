#ifndef EXECUTION_WINDOW_H
#define	EXECUTION_WINDOW_H

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
#include <QToolBox>

#include <confData.h>
#include <viewContainer.h>
#include <resizeableStackedWidget.h>
#include <scriptModule.h>
#include <parameter_widget.h>
#include <log_viewer.h>
#include <resultsModule.h>
#include <reprojectWindow.h>

#include "blockContainer.h"

class ExecutionWindow : public QWidget
{
    Q_OBJECT

public:
    ExecutionWindow(confData* data, resultsData *results, const QStringList& scriptDirs, QWidget *parent = NULL);

public slots:
    
    void scriptChanged(scriptModule *module, QModelIndex index);
    void scriptCompleted(scriptModule *module, QModelIndex index);
    void subscriptActivated(QModelIndex item);
    
    void maximizeLogWindow(bool maximize);
    void maximizeParameterWindow(bool maximize);

    void reload();

    void launchFileBrowser();
    void launchLogBrowser();

    void showScriptHelp();
    
    void execute(bool halt);
    void stopPlay();
    
    void setScriptProgress(int progress);
    
    void runInitialization();

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
    
    int runningTabIndex = -1;
    
    QProgressBar* progressBar;
    
    QLabel* scriptLabel;
    
    QString scriptHelp;
    
    QPushButton* runButton;
    QPushButton* refreshButton;
    QPushButton* manualButton;

};


#endif	/* EXECUTIONCONTAINER_H */

