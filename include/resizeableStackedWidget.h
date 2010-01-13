/*
 *  resizableStackedWidget.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 3/8/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef RESIZEABLESTACKEDWIDGET_H
#define RESIZEABLESTACKEDWIDGET_H

#include <QWidget>
#include <QVBoxLayout>

class resizeableStackedWidget : public QWidget
{
  Q_OBJECT

  private:
  int currentWidget;

  QList<QWidget *> widgets;
  QVBoxLayout *layout;

  public:

  resizeableStackedWidget(QWidget *parent = NULL);
  int addWidget(QWidget *widget);
  void setCurrentIndex(int index);
  QWidget *widget(int index);



};
#endif

