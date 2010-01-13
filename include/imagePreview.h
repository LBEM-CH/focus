/*
 *  imagePreview.h
 *  2DX
 *
 *  Created by Bryant Gipson on 3/1/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

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
#include <confData.h>
#include <mrcHeaderDisplay.h>
#include "imageNavigator.h"
#include <imageViewer.h>
#include "mrcImage.h"
#include "viewContainer.h"

class imagePreview : public QFrame
{
  Q_OBJECT

  public slots:
  void setImage(const QString &imageName);
  void clearNavigator();
  void shade();
  void toggleInfo();
  void progressDialog();
  void launchNavigator();
  void enableNewViewer(bool enable);

  signals:
  void setProgress(int value);
  void load();

  private:
  confData *conf;
  QString result;

  QStackedWidget *preview;
  QLabel *imageLabel;
  mrcHeaderDisplay *headerWidget;
  mrcImage *image;
  mrcImage *navImage;
  QMainWindow *navWindow;
  viewContainer *parentContainer;

  imageNavigator *nav;

  QMenuBar *mainMenuBar;

  bool useOldViewer;

  int minWidth;
  bool showInfo;

  void resetInfo();
  void resetImage();
  void clearImage();

  public:
  imagePreview(confData *data, QString result, bool showInfo, QMenuBar *menuBar, QWidget *parent = NULL);

  protected:
  void mouseDoubleClickEvent(QMouseEvent *event);
};

#endif
