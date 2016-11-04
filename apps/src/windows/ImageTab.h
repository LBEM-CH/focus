#ifndef IMAGECONTAINER_H
#define IMAGECONTAINER_H

#include <QWidget>
#include <QTabWidget>
#include <QMap>

#include "ParameterConfiguration.h"
#include "ExecutionWindow.h"
#include "ProcessingManager.h"

class ImageTab : public QWidget {
    Q_OBJECT

public:
    ImageTab(QWidget* parent = NULL);

public slots:
    void showImageWindow(ProjectImage* image);
    void closeImageWindow(int index);
    void setTabProcessing(ProjectImage* image);
    void setTabNormal(ProjectImage* image);
    void saveConfigs();
    bool configModified();
    void focusOnProcessingWindow();

private:
    
    ProcessingManager* automatorWindow_;

    QTabWidget* windowTabWidget;

    //Image Windows
    QMap<ProjectImage*, ExecutionWindow*> imagesInitializedToTabs_;
    QList<ProjectImage*> tabIdToWorkingDir_;
};

#endif