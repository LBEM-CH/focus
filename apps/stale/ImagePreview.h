#ifndef IMAGEPREVIEW_H
#define IMAGEPREVIEW_H

#include <QApplication>
#include <QDesktopWidget>
#include <QWidget>
#include <QStackedWidget>
#include <QGridLayout>
#include <QLinearGradient>
#include <QPixmap>
#include <QLabel>
#include <QDialog>
#include <QProgressBar>
#include <QProcess>
#include <QMainWindow>
#include <QTime>
#include <QCheckBox>

#include "ParameterConfiguration.h"
#include "MrcHeaderDisplay.h"
#include "ImageNavigator.h"
#include "mrcImage.h"

class ImagePreview : public QFrame {
    
    Q_OBJECT

public:
    ImagePreview(const QString& workingDir, QString result, bool showInfo, QWidget *parent = NULL);

public slots:
    void setImage(const QString &imageName);
    void clearNavigator();
    void shade();
    void toggleInfo();
    void showImageHeader(bool show);
    void progressDialog();
    void launchNavigator();

signals:
    void setProgress(int value);
    void load();

protected:
    void mouseDoubleClickEvent(QMouseEvent *event);

private:
    QString workingDir;
    QString result;

    QStackedWidget *preview;
    QLabel *imageLabel;
    MrcHeaderDisplay *headerWidget;
    mrcImage *image;
    mrcImage *navImage;

    ImageNavigator *nav;

    int minWidth;
    bool showInfo;

    void resetInfo();
    void resetImage(bool ignore_size = false);
    void clearImage();
};

#endif
