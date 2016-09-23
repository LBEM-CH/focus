#ifndef IMAGETHUMBNIALS_H
#define IMAGETHUMBNIALS_H

#include <QTreeView>
#include <QMouseEvent>
#include <QPalette>
#include <QStringList>
#include <QDebug>

#include "ProjectModel.h"
#include "ProjectData.h"
#include "ApplicationData.h"
#include "mrcImage.h"

class ImageThumbnails : public QTreeView {
    
    Q_OBJECT
    
public:
    ImageThumbnails(QWidget* parent=0);
    
    QString getPath(int colId);
    QStandardItemModel* getModel();
    int getSlectionCount();;
    
public slots:
    void updateThumbanils();
    
    void updateChecks(const QStringList& checkedImages);
    void saveChecks();
    
protected:
    void mousePressEvent(QMouseEvent* e) override;
    
private:
    QStringList columnPaths;
    QStandardItemModel* model;
    int selectionCount = 0;
};

#endif /* IMAGELIBRARY_H */

