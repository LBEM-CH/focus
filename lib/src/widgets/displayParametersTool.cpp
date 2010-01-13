/*
 *  displayParametersTool.cpp
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/18/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "displayParametersTool.h"
#include <iostream>
using namespace std;

displayParametersTool::displayParametersTool(QWidget *parent)
                                            :QWidget(parent,Qt::Tool)
{
  QGridLayout *layout = new QGridLayout(this);

  signalMapper = new QSignalMapper(this);

  maxValueMethodChooser.addItem("Maximum Value");
  maxValueMethodChooser.addItem("Gauss Fit");

  setDefaults();

  connect(&latticeSize,SIGNAL(valueChanged(int)),this,SLOT(changeLatticeSize(int)));
  connect(&latticeOrders,SIGNAL(valueChanged(int)),this,SLOT(changeLatticeOrders(int)));
  connect(&spotSize,SIGNAL(valueChanged(int)),this,SLOT(changeSpotSize(int)));
  connect(&latticeRefineSize,SIGNAL(valueChanged(int)),this,SLOT(changeRefinementSize(int)));
  connect(&searchRange,SIGNAL(valueChanged(int)),this,SIGNAL(searchRangeChanged(int)));
  connect(&sigma,SIGNAL(valueChanged(double)),this,SIGNAL(sigmaChanged(double)));
  connect(&viewFit,SIGNAL(stateChanged(int)),this,SLOT(changeViewFitState(int)));
  connect(&maxValueMethodChooser,SIGNAL(currentIndexChanged(int)),this,SLOT(changeSearchMethod(int)));

  connect(&latticeSize,SIGNAL(editingFinished()),signalMapper,SLOT(map()));
  signalMapper->setMapping(&latticeSize,&latticeSize);
  connect(&latticeOrders,SIGNAL(editingFinished()),signalMapper,SLOT(map()));
  signalMapper->setMapping(&latticeOrders,&latticeOrders);
  connect(&spotSize,SIGNAL(editingFinished()),signalMapper,SLOT(map()));
  signalMapper->setMapping(&spotSize,&spotSize);
  connect(&latticeRefineSize,SIGNAL(editingFinished()),signalMapper,SLOT(map()));
  signalMapper->setMapping(&latticeRefineSize,&latticeRefineSize);
  connect(&searchRange,SIGNAL(editingFinished()),signalMapper,SLOT(map()));
  signalMapper->setMapping(&searchRange,&searchRange);
  connect(&sigma,SIGNAL(editingFinished()),signalMapper,SLOT(map()));
  signalMapper->setMapping(&sigma,&sigma);

  connect(signalMapper,SIGNAL(mapped(QWidget*)),SLOT(complete(QWidget*)));

  layout->addWidget(new QLabel("<b>Display Parameters</b>"),0,0,1,1);
  layout->addWidget(new QLabel("Lattice Orders:"),1,0,1,1);
  layout->addWidget(&latticeOrders,1,1,1,1);
  layout->addWidget(new QLabel("Lattice Circle Radius [px]:"),2,0,1,1);
  layout->addWidget(&latticeSize,2,1,1,1);
  layout->addWidget(new QLabel("Spot Circle Radius [px]:"),3,0,1,1);
  layout->addWidget(&spotSize,3,1,1,1);
  layout->addWidget(new QLabel("Lattice Refinement Circle Radius [px]:"),4,0,1,1);
  layout->addWidget(&latticeRefineSize,4,1,1,1);
  layout->addItem(new QSpacerItem(10,20),5,0,1,1);
  layout->addWidget(new QLabel("<b>Smart Mouse Parameters</b>"),6,0,1,1);
  layout->addWidget(new QLabel("Maximum Value Method:"),7,0,1,1);
  layout->addWidget(&maxValueMethodChooser,7,1,1,1);
  layout->addWidget(new QLabel("Max Value Search Range [px]:"),8,0,1,1);
  layout->addWidget(&searchRange,8,1,1,1);
  layout->addWidget(new QLabel("Max Value Sigma [px]:"),9,0,1,1);
  layout->addWidget(&sigma,9,1,1,1);
  layout->addWidget(new QLabel("View Fit:"),10,0,1,1);
  layout->addWidget(&viewFit,10,1,1,1);
  setLayout(layout);
}

void displayParametersTool::changeLatticeSize(int value)
{
  emit latticeSizeChanged(value*2);
}

void displayParametersTool::changeLatticeOrders(int value)
{
  emit latticeOrdersChanged(value*2);
}

void displayParametersTool::changeSpotSize(int value)
{
  emit spotSizeChanged(value*2);
}

void displayParametersTool::changeRefinementSize(int value)
{
  emit refinementSizeChanged(value*2);
}

void displayParametersTool::changeViewFitState(int state)
{
  if(state == Qt::Checked)
    emit viewFitChanged(true);
  else
    emit viewFitChanged(false);
}

void displayParametersTool::changeSearchMethod(int method)
{
  if(method==0) emit searchMethodChanged(mrcImage::maximum_value);
  else if(method==1) emit searchMethodChanged(mrcImage::gauss_fit);
}

void displayParametersTool::flush()
{
  emit latticeSizeChanged(latticeSize.value()*2);
  emit latticeOrdersChanged(latticeOrders.value()*2);
  emit spotSizeChanged(spotSize.value()*2);
  emit refinementSizeChanged(latticeRefineSize.value()*2);
  emit searchRangeChanged(searchRange.value());
  emit sigmaChanged(sigma.value());
  changeSearchMethod(maxValueMethodChooser.currentIndex());
}

void displayParametersTool::focusInEvent(QFocusEvent *)
{
  latticeSize.setFocus(Qt::PopupFocusReason);
  latticeSize.selectAll();
}

void displayParametersTool::complete(QWidget *widget)
{
  if(widget!=NULL && widget->hasFocus())
  {
    hide();
  }
}

void displayParametersTool::complete()
{
  complete(focusWidget());
}

void displayParametersTool::showEvent(QShowEvent *event)
{
  QWidget::showEvent(event);
  setFocus(Qt::PopupFocusReason);
}

void displayParametersTool::setDefaults()
{
  latticeSize.setValue(10);
  latticeSize.setSingleStep(1);
  latticeOrders.setValue(20);
  latticeOrders.setSingleStep(1);
  latticeOrders.setMinimum(1);
  spotSize.setValue(10);
  spotSize.setSingleStep(1);
  latticeRefineSize.setValue(5);
  latticeRefineSize.setSingleStep(1);
  searchRange.setValue(25);
  searchRange.setSingleStep(1);
  sigma.setValue(1.5);
  sigma.setSingleStep(0.5);
  viewFit.setCheckState(Qt::Unchecked);
}


