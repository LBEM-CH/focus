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
    ImageThumbnails(QWidget* parent=0)
    :QTreeView(parent) {
        setHeaderHidden(true);
        setSortingEnabled(false);
        setIconSize(QSize(64, 64));
        setAttribute(Qt::WA_MacShowFocusRect, 0);
        
        
        QPalette pal = palette();
        pal.setColor(QPalette::Highlight, Qt::darkCyan);
        setPalette(pal);
        //setBackgroundRole(QPalette::Dark);

        QStringList imageList = projectData.imageList();
        QList<QStandardItem*> items;
        for(int i=0; i< imageList.size(); ++i) {
            QIcon icon;
            QString imageDir = imageList[i];
            if(QFileInfo(imageDir + "/final_map.mrc").exists()) {
                mrcImage image(imageDir + "/final_map.mrc", true);
                icon.addPixmap(image.getPixmap());
            } else {
                icon = ApplicationData::icon("default_preview");
            }
            
            QStandardItem* item = new QStandardItem();
            item->setIcon(icon);
            item->setToolTip(projectData.projectDir().relativeFilePath(imageDir));
            item->setCheckable(true);
            item->setEditable(false);
            columnPaths.append(imageDir);
            items.append(item);
        }
        
        QStandardItem* lastitem = new QStandardItem();
        lastitem->setEditable(false);
        lastitem->setSelectable(false);
        items << lastitem;
        
        model =  new QStandardItemModel();
        model->appendRow(items);
        setModel(model);
        
        for (int i = 0; i < model->columnCount()-1; i++) resizeColumnToContents(i);  
    }
    
    QString getPath(int colId) {
        if(colId >= columnPaths.size()) return "";
        return columnPaths[colId];
    }
    
    QStandardItemModel* getModel() {
        return model;
    }
    
    int getSlectionCount(){
        return selectionCount;
    };
    
public slots:
    void updateChecks(const QStringList& checkedImages) {
        int count = 0;
        for(int i=0; i<columnPaths.size(); ++i) {
            if(checkedImages.contains(columnPaths[i])) {
                model->item(0, i)->setCheckState(Qt::Checked);
                count++;
            }
            else model->item(0, i)->setCheckState(Qt::Unchecked);
        }
        if(count != selectionCount) {
            selectionCount = count;
            emit selectionCountChanged(selectionCount);
        }
    }
    
    void saveChecks() {
        QFile saveFile(projectData.selectionDirfile());
        if (!saveFile.open(QIODevice::WriteOnly | QIODevice::Text)) return;
        
        int count = 0;
        for(int i=0; i<columnPaths.size(); ++i) {
            if(model->item(0, i)->checkState() == Qt::Checked) {
                saveFile.write(QString(projectData.projectDir().relativeFilePath(columnPaths[i]) + '\n').toLatin1());
                count++;
            }
        }
        if(count != selectionCount) {
            selectionCount = count;
            emit selectionCountChanged(selectionCount);
        }
    }
    
signals:
    void selectionCountChanged(int count);
    
protected:
    void mousePressEvent(QMouseEvent* e) override {
        QTreeView::mousePressEvent(e);
        selectionModel()->setCurrentIndex( indexAt( e->pos() ), QItemSelectionModel::ClearAndSelect );
    }
    
private:
    QStringList columnPaths;
    QStandardItemModel* model;
    int selectionCount = 0;
};

#endif /* IMAGELIBRARY_H */

