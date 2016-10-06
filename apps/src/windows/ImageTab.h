#ifndef IMAGECONTAINER_H
#define IMAGECONTAINER_H

#include <QWidget>
#include <QTabWidget>
#include <QMap>
#include <QString>
#include <QStringList>

#include "ParameterConfiguration.h"
#include "ExecutionWindow.h"
#include "ParallelProcessingWindow.h"

class ImageTab : public QWidget {
    Q_OBJECT

public:
    ImageTab(QWidget* parent = NULL);

public slots:
    void showImageWindow(const QString&);
    void closeImageWindow(int index);
    void setTabProcessing(const QString&);
    void setTabNormal(const QString&);
    void saveConfigs();
    bool configModified();

private:
    
    ParallelProcessingWindow* automatorWindow_;

    QTabWidget* windowTabWidget;

    //Image Windows
    QMap<QString, ExecutionWindow*> imagesInitializedToTabs_;
    QStringList tabIdToWorkingDir_;
};

#endif