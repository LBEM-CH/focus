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
#include <QSpinBox>

#include "BlockContainer.h"
#include "BrowserWidget.h"
#include "LineEditSet.h"
#include "NoScrollComboBox.h"
#include "ProjectData.h"
#include "FileNameParserDialog.h"

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
    QWidget* setupStatusContainer();
    
    void analyzeImport(bool force = false);
    void resetImport(bool force = false);
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
    static QMutex mutex_;
    
    //Widgets
    QTableWidget* resultsTable_;
    QLabel* statusLabel_;
    QLabel* deleteLabel_;
    QListWidget* selectedScriptsCont;
    QWidget* inputContainer_;
    QPushButton* importButton_;
    QPushButton* refreshButton_;
    QPushButton* resetImportButton_;
    QCheckBox* priorityQueueOption_;
    QCheckBox* continuous;
    QCheckBox* deleteCheck;
    QCheckBox* EPUCheck;
    QSpinBox* safeIntervalBox;
    
    FileNameParserDialog* fileNameParser_;
    QLabel* filePatternLabel_;
    
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
    QString commandExecuting_;
    int whileImportScriptCount_ = 0;
};

#endif /* AUTOIMPORTWINDOW_H */

