/*
 *  ctfTool.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/18/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef CTFTOOL_H
#define CTFTOOL_H

#include <QWidget>
#include <QGridLayout>
#include <QLabel>
#include <QDoubleSpinBox>
#include <QPushButton>
#include "confData.h"
#include "fullScreenImage.h"
#include "math.h"


class ctfTool : public QDialog
{
  Q_OBJECT

  public slots:
  void valueChanged();
  void save();
  void load();
  void hideThonRings();

  signals:
  void defocusChanged(float defocusX, float defocusY, float astigmatism);

  protected:
  bool eventFilter(QObject *target, QEvent *event);
  
  private:
  confData *data;
  fullScreenImage *image;
  QLabel *defocusX, *defocusY;
  QDoubleSpinBox *defocus, *defocusDifference, *astigmatism;

  QPushButton *revertButton, *saveButton;


  public:
  ctfTool(confData *conf, fullScreenImage *sourceImage, QWidget *parent = NULL);

};

#endif
