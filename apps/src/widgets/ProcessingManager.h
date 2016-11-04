#ifndef PROCESSINGMANAGER_H
#define PROCESSINGMANAGER_H

#include <QWidget>
#include <QMap>
#include <QList>
#include <QTreeView>
#include <QLabel>
#include <QTableWidget>
#include <QPushButton>
#include <QSpinBox>
#include <QMutex>

#include "ProcessingModel.h"
#include "ImageScriptProcessor.h"

class ProcessingManager : public QWidget {
    
    Q_OBJECT
    
public:

    ProcessingManager(QWidget* parent = 0);
    
private:
    
    QWidget* setupQueueContainer();
    QWidget* setupStatusContainer();
    
    void executeProcesses(bool execute=true);
    void executeImage(ImageScriptProcessor* processor);
    
    void setQueueCount(int count);
    void addStatusToTable(int processId, const QString& image, const QString& text, bool error=false);
    void setupProcessors(int numProcessors);
    
    QList<ImageScriptProcessor*> processors_;
    QMap<ImageScriptProcessor*, int> processorId_;
    static QMutex mutex_;
    bool currentlyExecuting_ = false;
    int processorsFinished_=0;
    
    QSpinBox* processesBox_;
    QLabel* queueLabel_;
    QLabel* processingLabel_;
    QTableWidget* statusEntryTable_;
    QPushButton* executeButton_;
    QPushButton* clearButton_;
    
    QTreeView* queueView_;
    ProcessingModel* queueModel_;
};

#endif /* PROCESSINGMANAGER_H */

