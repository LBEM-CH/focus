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

#include "BlockContainer.h"
#include "BrowserWidget.h"
#include "LineEditSet.h"
#include "NoScrollComboBox.h"
#include "ProjectData.h"
#include "ImageScriptProcessor.h"

class AutoImportWindow : public QWidget {
    
    Q_OBJECT
    
public:

    AutoImportWindow(QWidget* parent);
    
signals:
    void imageToBeOpened(const QString& imPath);

private:

    QWidget* setupInputContainer();
    QWidget* setupInputFolderContainer();
    QWidget* setupJobsContainer();
    QWidget* setupOptionsContainter();
    QWidget* setupScriptsContainer();
    QTableWidget* setupFilesTable();
    QWidget* setupStatusContinaer();
    
    void analyzeImport();
    QString introText();
    QStringList imageGroups();
    
    void executeImport(bool execute=true);
    void importImage(ImageScriptProcessor* processor);
    
    void addStatusToTable(int processId, const QString& image, const QString& text, bool error=false);

    QStringList selectedScriptPaths();
    void resetSelectedScriptsContainer(QStringList availScripts);
    
    bool isSafeToCopy(const QString& imageName);
    
    void setupWatcherPaths();
    
    QTimer timer_; //Timer to check reanalyze if a file is being changed
    QList<ImageScriptProcessor*> processors_;
    QMap<ImageScriptProcessor*, int> processorId_;
    QMutex mutex_;
    QFileSystemWatcher watcher_; //Watcher to check the current averages folder

    //Widgets
    QTableWidget *resultsTable_;
    QLabel* statusLabel_;
    QTableWidget* statusEntryTable_;
    QListWidget* selectedScriptsCont;
    QTabWidget* availaleScriptsBox;
    QWidget* inputContiner_;
    QProgressBar* progressBar_; 
    QPushButton* importButton_;
    QPushButton* refreshButton_;
    
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

};

#endif /* AUTOIMPORTWINDOW_H */

