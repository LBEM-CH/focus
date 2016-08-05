/*
 *  selectionFFT.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 8/29/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef SELECTIONFFT_H
#define SELECTIONFFT_H

#include <QLabel>
#include <QGridLayout>
#include <QPixmap>

class selectionFFT : public QWidget
{
  Q_OBJECT

  public slots:
  void increaseZoom();
  void decreaseZoom();
  void zoomStandard();

  private:
  QLabel *display;
  QPixmap image;

  float intensity;
  float scale;

  void calculateFFT();

  public:
  selectionFFT(QWidget *parent = NULL);
  void fft(QWidget *widget, const QRect &view);
  void setBrightness(float brightness);
  void setZoom(float zoom);
};

#endif
