#ifndef AUTOIMPORTWINDOW_H
#define AUTOIMPORTWINDOW_H

#include <QWidget>
#include <QDebug>
#include <QTimer>
#include <QMap>
#include <QString>
#include <QLabel>
#include <QTableWidget>
#include <QProgressBar>

#include "BlockContainer.h"
#include "BrowserWidget.h"
#include "LineEditSet.h"
#include "NoScrollComboBox.h"
#include "ProjectData.h"
#include "LogViewer.h"

class AutoImportWindow : public QWidget {
public:

    AutoImportWindow(QWidget* parent);

private:

    QWidget* setupInputContainer();
    QWidget* setupInputFolderContainer();
    QWidget* setupOptionsContainter();
    QWidget* setupScriptsContainer();
    QTableWidget* setupFilesTable();
    QWidget* setupStatusContinaer();
    
    void analyzeImport();
    QString introText();
    QStringList imageGroups();
    
    void executeImport(bool execute=true);
    void importImage();
    void continueExecution(int exitCode);
    
    void addStatusToList(const QString& text, bool error=false);

    QStringList selectedScriptPaths();
    void resetSelectedScriptsContainer(QList<QListWidget*> availCont, QStringList availScripts);
    
    bool isSafeToCopy(const QString& imageName);
    
    QTimer timer_; //Timer to check reanalyze if a file is being changed
    QProcess process_;
    QFileSystemWatcher watcher_; //Watcher to check the current averages folder

    //Widgets
    QTableWidget *resultsTable_;
    QLabel* statusLabel_;
    QListWidget* statusEntryList_;
    QListWidget* selectedScriptsCont;
    QWidget* inputContiner_;
    QProgressBar* progressBar_; 
    QPushButton* importButton_;
    QPushButton* refreshButton_;
    
    //Data
    QMap<QString, QStringList> toBeImported_;
    QStringList scriptsToBeExecuted_;
    QDir currentWorkingDir_;
    QString scriptExecuting_;
    QString fileExecuting_;
    QString numberExecuting_;
    QMap<QString, int> dirToRowNumber_;
    bool currentlyExecuting_ = false;

};

#endif /* AUTOIMPORTWINDOW_H */

