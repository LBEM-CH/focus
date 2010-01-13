/*
 *  mrcImage.h
 *  2DX
 *
 *  Created by Bryant Gipson on 3/6/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef MRCIMAGE_H
#define MRCIMAGE_H

#include <QThread>
#include <QFile>
#include <QImage>
#include <QPixmap>
#include <QMatrix>
#include <QList>
#include <QString>
#include <QTime>
#include <QDebug>
#include <fftw3.h>
#include "mrcHeader.h"
#include <threadedLoad.h>

class mrcImage : public QThread
{
  Q_OBJECT

  public:
  enum maxValueMethod { maximum_value, gauss_fit };

  public slots:
  void setViewPhase(bool value);

  signals:
  void showProgress();
  void setProgress(int value);

  private:

  QList<mrcHeader *> headers;
  QString type;

  bool empty;
  QFile imageFile;
  QString fileName;
  short cellSize;
  bool showPhase;

  int littleEndian;

  float imageMax, imageMin;

  QMatrix tMatrix;

  char *rawData;
  uchar *imageData;

  QImage *image;
  QPixmap pixmap;

  uint32_t minLength;

  bool loadImage(mrcHeader *header, QImage::Format format = QImage::Format_RGB32);
  bool loadData(mrcHeader *header);
  void determineCellSize(mrcHeader *header);
  void scaleData(mrcHeader *header, QImage::Format format = QImage::Format_RGB32);
  void rescale(mrcHeader *header, float min, float max, QImage::Format format = QImage::Format_RGB32);
  void formatImage(mrcHeader *header, QImage::Format format = QImage::Format_RGB32);
  float stdDev(mrcHeader *header);
  void run();
  float fastMagnitude(float a, float b);

  public:

  mrcImage(QString filename, bool thumbnail = false, QObject *parent = NULL);
  ~mrcImage();

  void rescale(float min, float max);

  bool isEmpty();

  void initialize();
  void clear();

  bool thumbnailPresent();
  bool generateThumbnail();

  const QString &getFileName();
  QImage *getImage();
  QPixmap &getPixmap();
  const QString &getType();
  mrcHeader *getHeader(int h=0);
  float max();
  float min();
  float value(const QPoint &pos);
  float phase(const QPoint &pos);

  void setMatrix(const QMatrix &matrix);
  const QMatrix &matrix();

  QPoint maxValue(const QPoint &pos, int distance, mrcImage::maxValueMethod method, float sigma = 1.0);
  bool validPosition(int x, int y);

};

#endif
