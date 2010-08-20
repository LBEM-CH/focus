/*
 *  displayParametersTool.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/18/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef DISPLAYPARAMETERSTOOL_H
#define DISPLAYPARAMETERSTOOL_H

#include <QWidget>
#include <QGridLayout>
#include <QLabel>
#include <QSpinBox>
#include <QDoubleSpinBox>
#include <QComboBox>
#include <QCheckBox>
#include <QSignalMapper>
#include <QAction>
#include <confData.h>
#include <mrcImage.h>

class displayParametersTool : public QWidget
{
  Q_OBJECT

  public slots:
/* Overloaded Slots */

/* Slots */
  void changeLatticeSize(int value);
  void changeLatticeCircleLine(int thickness);
  void changeLatticeOrders(int value);
  void changeSpotSize(int value);
  void changeRefinementSize(int value);
  void changeViewFitState(int state);
  void changeSearchMethod(int method);
  void flush();
  void complete(QWidget *widget);
  void complete();

  void setDefaults();

  signals:
  void latticeSizeChanged(int size);
  void latticeCircleLineChanged(int thickness);
  void latticeOrdersChanged(int orders);
  void spotSizeChanged(int size);
  void refinementSizeChanged(int size);

  void searchRangeChanged(int range);
  void sigmaChanged(double sigma);
  void searchMethodChanged(mrcImage::maxValueMethod method);

  void viewFitChanged(bool view);

  private:
  QSpinBox latticeSize, latticeOrders, spotSize, latticeRefineSize, latticeCircleLine;
  QSpinBox searchRange;
  QDoubleSpinBox sigma;

  QCheckBox viewFit;
  
  QSignalMapper *signalMapper;

  QComboBox maxValueMethodChooser;

  public:
  displayParametersTool(QWidget *parent = NULL);

  protected:
  void showEvent(QShowEvent *event);
  void focusInEvent(QFocusEvent *event);

};

#endif
