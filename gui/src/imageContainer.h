#ifndef IMAGECONTAINER_H
#define	IMAGECONTAINER_H

#include <QWidget>
#include <QTabWidget>
#include <QMap>
#include <QString>
#include <QStringList>

#include "confData.h"
#include "imageWindow.h"

class imageContainer : public QWidget
{
    Q_OBJECT
           
    public:
        imageContainer(confData* mainData, QWidget* parent=NULL);
        
    public slots:
        void showImageWindow(const QString&);
        void closeImageWindow(int index);
        
    private:
        
        QLabel* noImageLabel;
        QTabWidget* windowTabWidget;
        confData* mainData;
        
        //Image Windows
        QMap<QString, imageWindow*> imagesInitializedToTabs_;
        QStringList imagesShown_;
};

#endif