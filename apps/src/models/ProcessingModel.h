#ifndef PROCESSINGMODEL_H
#define PROCESSINGMODEL_H

#include <QStandardItemModel>
#include <QStandardItem>
#include <QMutex>

#include "ProjectImage.h"
#include "ProjectData.h"
#include "ParameterElementData.h"
#include "ScriptData.h"

class ProcessingModel : public QStandardItemModel {
    
    Q_OBJECT
    
public:
    ProcessingModel(QObject* parent = 0);
    
    void addProcesses(QMap<ProjectImage*, QStringList> imageAndScripts);
    QMap<ProjectImage*, QStringList> nextInQueue();
    void clearAll();
    
signals:
    void rowCountChanged(int count);
    
private:
    static QMutex mutex_;
};

#endif /* PROCESSINGMODEL_H */

