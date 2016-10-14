#ifndef PARALLELPROCESSINGWINDOW_H
#define PARALLELPROCESSINGWINDOW_H

#include <QWidget>
#include <QDebug>
#include <QTimer>
#include <QMap>
#include <QString>
#include <QLabel>
#include <QTableWidget>
#include <QProgressBar>
#include <QTabWidget>
#include <QMutex>

#include "BlockContainer.h"
#include "BrowserWidget.h"
#include "LineEditSet.h"
#include "NoScrollComboBox.h"
#include "ProjectData.h"
#include "ImageScriptProcessor.h"

class ParallelProcessingWindow : public QWidget {
    
public:
    
    ParallelProcessingWindow(QWidget* parent);
    
private:
    
    QWidget* setupInputContainer();
    QWidget* setupInputFolderContainer();
    QWidget* setupScriptsContainer();
    QWidget* setupStatusContinaer();
    
    void changeSelectionCount(int count);
    
    void executeProcesses(bool execute=true);
    void executeImage(ImageScriptProcessor* processor);
    
    void addStatusToTable(int processId, const QString& image, const QString& text, bool error=false);

    QStringList selectedScriptPaths();
    void resetSelectedScriptsContainer(QStringList availScripts);
    
    QList<ImageScriptProcessor*> processors_;
    QMap<ImageScriptProcessor*, int> processorId_;
    static QMutex mutex_;

    //Widgets
    QLabel* statusLabel_;
    QTableWidget* statusEntryTable_;
    QListWidget* selectedScriptsCont;
    QTabWidget* availaleScriptsBox;
    QWidget* inputContiner_;
    QProgressBar* progressBar_; 
    QPushButton* importButton_;
    
    QStringList imagesToBeProcessed_;
    QStringList scriptsToBeExecuted_;
    bool currentlyExecuting_ = false;
    int processorsFinished_=0;

};

#endif /* PARALLELPROCESSINGWINDOW_H */

