#ifndef LIBRARYCONTAINER_H
#define	LIBRARYCONTAINER_H

#include <QWidget>
#include <QTreeView>
#include <QString>
#include <QStringList>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QTimer>
#include <QComboBox>
#include <QStackedWidget>
#include <QSortFilterProxyModel>
#include <QLabel>
#include <QToolButton>
#include <QLineEdit>

#include "BlockContainer.h"
#include "ProjectModel.h"
#include "ImageViewer.h"
#include "ParameterConfiguration.h"
#include "ResultsData.h"
#include "ProjectDelegate.h"
#include "LibraryImageStatus.h"

class LibraryTab : public QWidget
{
    Q_OBJECT
           
    public:
        LibraryTab(ResultsData* results, QWidget* parent=NULL);
        
        ProjectModel* getDirModel();
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
        
        void import();
        
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
        
        void updateProjectName(const QString& name="");
        
        void setImagesOpen(QStringList);
        bool imageOpen(const QString& imageDir);
                   
    private:
        void setupDirectoryContainer();
        QWidget* setupToolBar();
        QWidget* setupSelectionTab();
        
        void modifySelection(bool select = true);
        
        QTreeView* dirView;
        ProjectModel* dirModel;
        QSortFilterProxyModel *sortModel;
        
        QLabel* selectionState;
        QLabel* projectNameLabel;
               
        ImageViewer* mapPreview;
        ImageViewer* refPreview;
        ImageViewer* dualPreview;
        QToolButton* showHeaderButton;
        
        LibraryImageStatus* imageDataWidget;
        
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

