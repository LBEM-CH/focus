/*
 *  zoomWindow.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 4/25/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef ZOOMWINDOW_H
#define ZOOMWINDOW_H

#include <QWidget>
#include <QMouseEvent>
#include <QGridLayout>
#include <QLabel>
#include <QPixmap>
#include <QPoint>
#include <QPainter>
#include <QDebug>
#include <math.h>

class zoomWindow : public QWidget
{
  Q_OBJECT

  signals:
  void zoomClick(const QPoint &pos);
  void zoomDoubleClick(const QPoint &pos);
  void zoomMove(const QPoint &pos);

  public slots:
  void zoom(const QPoint &pos);
  void zoom();

  private:
  QWidget *widget;
  QPixmap image;
  QLabel *display;
  QWidget *p;

  int zoomX,zoomY;
  QPoint zoomPos;
  QPoint currentPosition(QMouseEvent *event);
  public:
  zoomWindow(QWidget *imageWidget, QWidget *parent = NULL);

  protected:
  void mousePressEvent(QMouseEvent *event);
  void mouseDoubleClickEvent(QMouseEvent *event);
  void mouseMoveEvent(QMouseEvent *event);
  void paintEvent(QPaintEvent *event);
  void resizeEvent(QResizeEvent *event);
};

#endif
