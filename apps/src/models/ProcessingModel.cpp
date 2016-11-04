#include <QMutexLocker>

#include "ProcessingModel.h"

QMutex ProcessingModel::mutex_;

ProcessingModel::ProcessingModel(QObject* parent) :
QStandardItemModel(parent) {

}

void ProcessingModel::addProcesses(QMap<ProjectImage*, QStringList> imageAndScripts, bool prioritize) {
    for (ProjectImage* image : imageAndScripts.keys()) {
        if (image && !imageAndScripts[image].isEmpty()) {
            QStandardItem* imageItem = new QStandardItem(image->toString());
            imageItem->setToolTip(image->workingPath());

            if(prioritize) insertRow(0, imageItem);
            else appendRow(imageItem);
            
            //Add all the scripts to be processed
            QStringList scripts = imageAndScripts[image];
            for (QString script : scripts) {
                ScriptData sData(script);
                QStandardItem* item = new QStandardItem(QString(sData.getProperty("title")).simplified());
                item->setToolTip(script);
                imageItem->appendRow(item);
            }
        }
    }
    emit rowCountChanged(rowCount());
}

QMap<ProjectImage*, QStringList> ProcessingModel::nextInQueue() {
    QMutexLocker locker(&ProcessingModel::mutex_);
    QMap<ProjectImage*, QStringList> nextImage;
    if (rowCount() == 0) return nextImage;

    QStandardItem* imageItem = item(0, 0);

    ProjectImage* image = 0;
    QStringList scripts;
    if (imageItem) {
        image = projectData.projectImage(imageItem->toolTip());

        while (imageItem->hasChildren()) {
            QStandardItem* child = imageItem->child(0);
            scripts.append(child->toolTip());
            imageItem->removeRow(0);
        }

        nextImage.insert(image, scripts);
    }

    removeRow(0);
    
    emit rowCountChanged(rowCount());
    return nextImage;
}

void ProcessingModel::clearAll() {
    clear();
    emit rowCountChanged(rowCount());
}