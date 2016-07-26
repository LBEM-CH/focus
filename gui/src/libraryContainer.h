/* 
 * File:   albumContainer.h
 * Author: biyanin
 *
 * Created on August 17, 2015, 12:10 PM
 */

#ifndef LIBRARYCONTAINER_H
#define	LIBRARYCONTAINER_H

#include <QWidget>
#include <QTreeView>
#include <QString>
#include <QStringList>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QInputDialog>
#include <QToolBar>
#include <QSortFilterProxyModel>
#include <QHeaderView>
#include <QGridLayout>
#include <QModelIndex>
#include <QModelIndexList>
#include <QGroupBox>
#include <QToolButton>
#include <QListWidget>

#include "blockContainer.h"
#include "projectModel.h"
#include "imagePreview.h"
#include "confData.h"
#include "resultsData.h"
#include "projectDelegate.h"

class libraryContainer : public QWidget
{
    Q_OBJECT
           
    public:
        libraryContainer(confData *data, resultsData* results, QWidget* parent=NULL);
        
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
        void addImageFolder();
        void addImageFolder(const QString& folder);
        void moveSelectiontoFolder();
        void moveSelectionToFolder(const QString& targetPath);
        void trashSelection();
        bool copyRecursively(const QString &srcFilePath, const QString &tgtFilePath);
        
        void saveProjectState();
        void loadProjectState();
        
        void setPreviewImages(const QString&);
        void loadDataContainer(const QString&);
        void autoSwitch(bool);
        void updatePreview();
        
        void resetSelectionState();
                   
    private:
        void setupDirectoryContainer(confData*);
        QToolBar* setupContextAndMenu();
        void modifySelection(bool select = true);
        

        confData* data;
        
        QTreeView* dirView;
        projectModel* dirModel;
        QSortFilterProxyModel *sortModel;
        
        QLabel* selectionState;
               
        imagePreview* mapPreview;
        imagePreview* refPreview;
        imagePreview* dualPreview;
        
        QListWidget* selectionWidget;
        
        QStackedWidget* previews;
        
        QTimer* previewTimer;
        
        QComboBox* viewControl;
    
};


#endif	/* ALBUMCONTAINER_H */

