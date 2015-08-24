/* 
 * File:   albumContainer.h
 * Author: biyanin
 *
 * Created on August 17, 2015, 12:10 PM
 */

#ifndef ALBUMCONTAINER_H
#define	ALBUMCONTAINER_H

#include <QWidget>
#include <QTreeView>
#include <QString>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QSortFilterProxyModel>
#include <QHeaderView>
#include <QGridLayout>
#include <QModelIndex>
#include <QModelIndexList>
#include <QGroupBox>
#include <QToolButton>

#include "blockContainer.h"
#include "projectModel.h"
#include "imagePreview.h"
#include "confData.h"
#include "resultsData.h"
#include "projectDelegate.h"

class albumContainer : public QWidget
{
    Q_OBJECT
           
    public:
        albumContainer(confData *data, resultsData* results, QWidget* parent=NULL);
        
        projectModel* getDirModel();
        QTreeView* getDirView();
    
    public slots:
        void showContents(bool show);
        
        void showSelected(bool enable);
        bool loadSelection(const QString &fileName = "");
        
        void reload();
        void updateModel();
        void maskResults();
        void columnActivated(int i);
        void copyImage();
        
        void extendSelection();
        void reduceSelection();
        
        void saveProjectState();
        void loadProjectState();
            
    private:
        void setupDirectoryContainer(confData*);
        void modifySelection(bool select = true);

        confData* data;
        
        QTreeView* dirView;
        projectModel* dirModel;
        QSortFilterProxyModel *sortModel;
        
        imagePreview* preview;
    
};


#endif	/* ALBUMCONTAINER_H */

