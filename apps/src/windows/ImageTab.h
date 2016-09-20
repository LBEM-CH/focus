#ifndef IMAGECONTAINER_H
#define IMAGECONTAINER_H

#include <QWidget>
#include <QTabWidget>
#include <QMap>
#include <QString>
#include <QStringList>

#include "ParameterConfiguration.h"
#include "ExecutionWindow.h"

class ImageTab : public QWidget {
    Q_OBJECT

public:
    ImageTab(QWidget* parent = NULL);

    QStringList getImagesOpen();

public slots:
    void showImageWindow(const QString&);
    void closeImageWindow(int index);
    void setTabProcessing(const QString&);
    void setTabNormal(const QString&);
    void saveConfigs();
    bool configModified();

private:

    QLabel* noImageLabel;
    QTabWidget* windowTabWidget;

    //Image Windows
    QMap<QString, ExecutionWindow*> imagesInitializedToTabs_;
    QStringList imagesShown_;
};

#endif