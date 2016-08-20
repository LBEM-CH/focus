#ifndef IMAGECONTAINER_H
#define	IMAGECONTAINER_H

#include <QWidget>
#include <QTabWidget>
#include <QMap>
#include <QString>
#include <QStringList>

#include "confData.h"
#include "image_window.h"

class ImageTab : public QWidget
{
    Q_OBJECT
           
    public:
        ImageTab(confData* mainData, QWidget* parent=NULL);
        
        QStringList getImagesOpen();
        
    public slots:
        void showImageWindow(const QString&);
        void closeImageWindow(int index);
        void setTabProcessing(const QString&);
        void setTabNormal(const QString&);
        void saveConfigs();
        bool configModified();
        
    signals:
        void imagesOpenChanged(QStringList);
        
    private:
        
        QLabel* noImageLabel;
        QTabWidget* windowTabWidget;
        confData* mainData;
        
        //Image Windows
        QMap<QString, ImageWindow*> imagesInitializedToTabs_;
        QStringList imagesShown_;
};

#endif