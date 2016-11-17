#ifndef SPOTSELECTTOOL_H
#define SPOTSELECTTOOL_H

#include <QApplication>
#include <QDesktopWidget>
#include <QWidget>
#include <QGridLayout>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QString>
#include <QLabel>
#include <QPoint>
#include <math.h>

#include "ParameterConfiguration.h"
#include "mrcImage.h"
#include "FullScreenImage.h"

class SpotSelectTool : public QWidget {
    Q_OBJECT
    
public:
    SpotSelectTool(const QString& workDir, FullScreenImage *image, mrcImage *sourceImage, const QPoint &dimensions, QWidget *parent = NULL);
    void updateIndices(const QPoint &pos);
    void updateIndices(float imageScale);
    QPointF getImageCoordinates();

public slots:
    void updateReferenceOrigin();

signals:
    void phaseChanged(float);

private:
    float inv[2][2];
    int screenWidth, screenHeight;

    QPoint currentPos;

    QLabel *i, *j, *mouseX, *mouseY, *refOriginX, *refOriginY;
    QLabel *resolution, *value, *phase;

    mrcHeader *imageHeader;
    mrcImage *imageData;
    FullScreenImage *image;
    
    QString workingDir;
};

#endif

