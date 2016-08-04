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
#include <QTreeWidget>
#include <QFormLayout>

#include "blockContainer.h"
#include "projectModel.h"
#include "imagePreview.h"
#include "confData.h"
#include "resultsData.h"
#include "projectDelegate.h"
#include "libraryImageStatus.h"

class LibraryTab : public QWidget
{
    Q_OBJECT
           
    public:
        LibraryTab(confData *data, resultsData* results, QWidget* parent=NULL);
        
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
        
        void autoSelect();
        void extendSelection();
        void reduceSelection();
        void addImageFolder();
        void addImageFolder(const QString& folder);
        void renameImageFolder();
        void moveSelectiontoFolder();
        void moveSelectionToFolder(const QString& targetPath);
        void trashSelection();
        bool copyRecursively(const QString &srcFilePath, const QString &tgtFilePath);
        
        void saveProjectState();
        void loadProjectState();
        
        void saveDirectorySelection();
        void loadDirectorySelection();
        
        void setPreviewImages(const QString&);
        void loadDataContainer(const QString&);
        void autoSwitch(bool);
        void updatePreview();
        
        void resetSelectionState();
        
        void updateProjectName();
        
        void setImagesOpen(QStringList);
        bool imageOpen(const QString& imageDir);
                   
    private:
        void setupDirectoryContainer(confData*);
        QWidget* setupToolBar();
        QWidget* setupSelectionTab();
        
        void modifySelection(bool select = true);
        

        confData* data;
        
        QTreeView* dirView;
        projectModel* dirModel;
        QSortFilterProxyModel *sortModel;
        
        QLabel* selectionState;
        QLabel* projectNameLabel;
               
        imagePreview* mapPreview;
        imagePreview* refPreview;
        imagePreview* dualPreview;
        
        libraryImageStatus* imageDataWidget;
        
        QStackedWidget* previews;
        
        QTimer* previewTimer;
        
        QComboBox* viewControl;
        
        QLineEdit* minDegree;
        QLineEdit* maxDegree;
        QComboBox* parameterToUse;
        QComboBox* negPosOption;
        
        QStringList imagesOpen;
    
};


#endif	/* ALBUMCONTAINER_H */

