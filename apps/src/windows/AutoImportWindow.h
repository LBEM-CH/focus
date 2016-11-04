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
#include <QTabWidget>
#include <QListWidget>
#include <QMutex>
#include <QProcess>
#include <QFileSystemWatcher>

#include "BlockContainer.h"
#include "BrowserWidget.h"
#include "LineEditSet.h"
#include "NoScrollComboBox.h"
#include "ProjectData.h"

class AutoImportWindow : public QWidget {
    
    Q_OBJECT
    
public:

    AutoImportWindow(QWidget* parent);
    
signals:
    void imageToBeOpened(ProjectImage* image);

private:

    QWidget* setupInputContainer();
    QWidget* setupInputFolderContainer();
    QWidget* setupOptionsContainter();
    QWidget* setupScriptsContainer();
    QTableWidget* setupFilesTable();
    QWidget* setupStatusContinaer();
    
    void analyzeImport(bool force = false);
    QString introText();
    
    void executeImport(bool execute=true);
    void importImage();
    void continueExecution();
    void finishExecution();

    void resetSelectedScriptsContainer();
    bool isSafeToCopy(const QString& imageName);
    void setupWatcherPaths();
    void resetState();
    QStringList selectedScriptPaths();
    
    QTimer timer_; //Timer to check reanalyze if a file is being changed
    QFileSystemWatcher watcher_; //Watcher to check the current averages folder
    QProcess process_;

    //Widgets
    QTableWidget* resultsTable_;
    QLabel* statusLabel_;
    QListWidget* selectedScriptsCont;
    QWidget* inputContiner_;
    QPushButton* importButton_;
    QPushButton* refreshButton_;
    QCheckBox* importLastFirstOption_;
    QCheckBox* continuous;
    
    //Data
    
    /**
     * Each Target number is mapped to a list of strings containing:
     * 0. OriName (the target images are named using this)
     * 1. If averaged file is present then it's full file path else empty
     * 2. If aligned file is present then it's full file path else empty
     * 3. If raw file is present then it's full file path else empty
     */
    QMap<QString, QStringList> toBeImported_;
    QMap<QString, int> dirToRowNumber_;
    QStringList rowToImagePaths_;
    bool currentlyExecuting_ = false;
    QStringList scriptsToBeExecuted_;
    ProjectImage* imageExecuting_=0;
};

#endif /* AUTOIMPORTWINDOW_H */

