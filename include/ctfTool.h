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
#include "math.h"

class ctfTool : public QWidget
{
  Q_OBJECT

  public slots:
  void valueChanged();
  void save();
  void load();

  signals:
  void defocusChanged(float defocusX, float defocusY, float astigmatism);

  private:
  confData *data;
  QLabel defocusX, defocusY;
  QDoubleSpinBox defocus, defocusDifference, astigmatism;

  QPushButton *revertButton, *saveButton;

  public:
  ctfTool(confData *conf, QWidget *parent = NULL);

};

#endif
