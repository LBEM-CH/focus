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
#include <QButtonGroup>
#include <QToolButton>
#include <QSpacerItem>
#include <QScrollArea>
#include <QStatusBar>
#include <QProgressBar>
#include <QToolBar>
#include <QPushButton>
#include <QListWidget>
#include <QListWidgetItem>
#include <QLineEdit>
#include <QSlider>
#include <QMutex>

#include "ParameterConfiguration.h"
#include "ScriptModule.h"
#include "ParameterWidget.h"
#include "LogViewer.h"
#include "ResultsModule.h"
#include "ImageViewer.h"
#include "ResultsData.h"
#include "BlockContainer.h"
#include "ScriptHelp.h"
#include "ProjectImage.h"

class ExecutionWindow : public QWidget
{
    Q_OBJECT

public:
    
    enum class Type {
        PROJECT, IMAGE
    };
    
    ExecutionWindow(const QDir& moduleDir, ProjectImage* image=0, QWidget *parent = NULL);
    
    bool isRunningScript();
    ParametersConfiguration* getConf();
    QDir workingDirectory();
    QToolBar* getToolBar();
    static QToolButton* getToolButton(const QIcon& icon, const QString& text, bool checkable);
    void addToMainToolBar(QWidget* associatedWidget, const QIcon& icon, const QString& text, bool startWithSeperator=false);

public slots:
    
    void scriptChanged(ScriptModule *module, QModelIndex index);
    void scriptCompleted(ScriptModule *module, QModelIndex index);
    void subscriptActivated(QModelIndex item);

    void reloadAndSave(const QString& resultsFile="", bool save=true);

    void launchFileBrowser();
    void launchLogBrowser();

    void showScriptHelp();
    
    void execute(bool halt);
    void stopPlay();
    
    void setScriptProgress(int progress);
    
    void runInitialization();
    
    void save();
    bool modified();
    void saveAsProjectDefault();
    
signals:
    void executing(bool);
    void scriptCompletedSignal();

private:
    
    BlockContainer* setupLogWindow();
    BlockContainer* setupHistoryWindow();
    BlockContainer* setupParameterWindow();
    QWidget* setupScriptsWidget(const QStringList& scriptDirs);
    QWidget* setupTitleContainer();
    QSplitter* setupResultsContainer();
    QWidget* spacer();
    QToolButton* addVisibilityButton(QString title, QWidget* widgetToLink, bool initialState);
    
    void setLastChangedInConfig();

    static QMutex lock_;
    
    ProjectImage* image_ = 0;
    ExecutionWindow::Type type_;
    
    ScriptModule* defaultModule;
    QToolButton* defaultButton;

    QStackedWidget* mainWidget;
    QWidget* executionWidget;
    QButtonGroup* mainToolBarButtonGroup;
    QToolBar* mainToolBar;
    QStackedWidget* scriptsWidget;
    QListView* subscriptWidget;
    
    ResultsData *results;

    ParametersWidget *parameters;
    QLineEdit* parameterSearchBox;

    QSplitter* centralSplitter;
    QSplitter* mainSplitter;
    QSplitter* resultsSplitter;

    ResultsModule *resultsView;

    LogViewer *logViewer;
    LogViewer *historyViewer;
    BlockContainer* logWindow;

    QSlider* outputVerbosityControl;
    QSlider* historyVerbosityControl;
    
    int runningTabIndex = -1;
    
    QProgressBar* progressBar;
    QLabel* scriptLabel;
    ScriptHelp* scriptHelpDialog;
    QPushButton* runButton;
    
    QToolBar* panelVisibilityToolBar;
    QToolButton* showOutputButton;
    QToolButton* showResultsButton;
    
    ImageViewer* viewer;
};


#endif	/* EXECUTIONCONTAINER_H */

