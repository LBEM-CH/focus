
#ifndef PROJECTTOOLS_H
#define PROJECTTOOLS_H

#include <QDialog>
#include <QComboBox>
#include <QPushButton>
#include <QLabel>
#include <QFont>
#include <QPalette>
#include <QProgressBar>
#include <QToolButton>
#include <QSplitter>
#include <QScrollArea>
#include <QProcess>

#include "confData.h"
#include "scriptModule.h"
#include "parameter_widget.h"
#include "log_viewer.h"
#include "blockContainer.h"
#include "resizeableStackedWidget.h"

class ProjectTools : public QDialog {
    Q_OBJECT

public:

    ProjectTools(confData* data, QWidget *parent = NULL);
    
public slots:
    
    void scriptChanged(QModelIndex index);
    void subscriptActivated(QModelIndex item);
    void updateScriptLabel(const QString& label);
    void setScriptProgress(int progress);
    
    void launchLogBrowser();
    
    void showSubTitle(bool show);
    
    void execute(bool run);
    void stopPlay();

private:

    blockContainer* setupLogWindow();
    blockContainer* setupParameterWindow();

    confData* mainData;
    
    scriptModule* toolsScriptModule;
    QListView* subscriptWidget;

    ParametersWidget *parameters;
    LogViewer *logViewer;

    QComboBox* userLevelButtons;
    QComboBox* verbosityControl;

    QProgressBar* progressBar;
    QLabel* scriptLabel;
    QLabel* subTitleLabel;
    QPushButton* runButton;
    QPushButton* manualButton;

};


#endif /* PROJECTTOOLS_H */

