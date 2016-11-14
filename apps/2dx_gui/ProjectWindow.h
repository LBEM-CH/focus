#ifndef PROJECTWINDOW_H
#define PROJECTWINDOW_H

#include <QWidget>
#include <QLabel>
#include <QLayout>
#include <QGridLayout>
#include <QVBoxLayout>
#include <QFont>
#include <QFrame>
#include <QMap>

#include "ExecutionWindow.h"
#include "ApplicationData.h"
#include "ProjectData.h"
#include "ImageConfigChanger.h"
#include "GraphicalButton.h"

class ProjectWindow : public QWidget {

    Q_OBJECT

public:
    ProjectWindow(QWidget* parent);;

public slots:

    void setProjectTitle(const QString& title);
    void setProjectPath(const QString& path);
    void setImagesCount(int count);

private:

    QHBoxLayout* setupTitleContainer();
    QGridLayout* setupImageActions();
    QWidget* setupParametersWidget();

    QLabel* projectNameLabel_;
    QLabel* projectPathLabel_;
    QLabel* imagesCountLabel_;
    
    QList<ProjectImage*> imagesList_;

};


#endif /* PROJECTWINDOW_H */

