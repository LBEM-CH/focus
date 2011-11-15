/*
 *  ctfTool.cpp
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/18/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "ctfTool.h"
#include "float.h"
#include <iostream>
using namespace std;

ctfTool::ctfTool(confData *conf, fullScreenImage *sourceImage, QWidget *parent)
                :QDialog(parent)
{
  data = conf;
  image = sourceImage;
  
  defocus = new QDoubleSpinBox(this);
  defocus->setRange(-DBL_MAX,DBL_MAX);
  defocus->setSingleStep(20.0);
  defocus->installEventFilter(this);
  defocusDifference = new QDoubleSpinBox(this);
  defocusDifference->setRange(-DBL_MAX,DBL_MAX);
  defocusDifference->setSingleStep(20.0);
  defocusDifference->installEventFilter(this);
  astigmatism = new QDoubleSpinBox(this);
  astigmatism->setRange(0.0,360.0);
  astigmatism->setWrapping(true);
  astigmatism->installEventFilter(this);
  defocusX = new QLabel(this);
  defocusY = new QLabel(this);

  load();

  connect(defocus,SIGNAL(valueChanged(double)),this,SLOT(valueChanged()));
  connect(defocusDifference,SIGNAL(valueChanged(double)),this,SLOT(valueChanged()));
  connect(astigmatism,SIGNAL(valueChanged(double)),this,SLOT(valueChanged()));

  saveButton = new QPushButton("Accept");
  saveButton->setDefault(true);
  revertButton = new QPushButton("Reload");

  connect(saveButton,SIGNAL(clicked()),this,SLOT(save()));
  connect(revertButton,SIGNAL(clicked()),this,SLOT(load()));

  QGridLayout *layout = new QGridLayout(this);
  layout->addWidget(new QLabel("Defocus x, y:"),0,0,1,1);
  layout->addWidget(defocusX,0,1,1,1); layout->setAlignment(defocusX,Qt::AlignCenter);
  layout->addWidget(defocusY,0,2,1,1); layout->setAlignment(defocusY,Qt::AlignCenter);

  layout->addWidget(new QLabel("Defocus [A]: "),1,0,1,1);
  layout->addWidget(defocus,1,1,1,2);

  layout->addWidget(new QLabel("Astigmatism [A]: "),2,0,1,1);
  layout->addWidget(defocusDifference,2,1,1,2);

  layout->addWidget(new QLabel("Astigmatic Angle [deg]: "),3,0,1,1);
  layout->addWidget(astigmatism,3,1,1,2);
  layout->addWidget(revertButton,4,0,1,1);
  layout->addWidget(saveButton,4,1,1,2);

  setLayout(layout);

  valueChanged();
}

void ctfTool::valueChanged()
{
  double x = defocus->value()+0.5*defocusDifference->value();
  double y = defocus->value()-0.5*defocusDifference->value();
  defocusX->setText(QString::number(x));
  defocusY->setText(QString::number(y));
  emit defocusChanged(x,y,astigmatism->value());
}

void ctfTool::load()
{
  double x,y;
  QStringList cell = data->get("defocus","value").split(',');
  if(cell.size()==3)
  {
    x=cell[0].toDouble();
    y=cell[1].toDouble();
    astigmatism->setValue(fmod(cell[2].toFloat(),360.0));
  }
  else
  {
    x = 5000.0;
    y = 5000.0;
  }
  defocus->setValue((x + y)/2.0);
  defocusDifference->setValue(x-y);
  defocusX->setText(QString::number(x));
  defocusY->setText(QString::number(y));
}

void ctfTool::save()
{
  double x = defocus->value()+0.5*defocusDifference->value();
  double y = defocus->value()-0.5*defocusDifference->value();
  QString defocus = QString::number(x) + ',' + QString::number(y) + ',' + QString::number(astigmatism->value());
  data->set("defocus",defocus);
  data->set("DEFOCUS_done","y");
  data->setModified(true);
}

void ctfTool::hideThonRings()
{
  image->toggleCTFView();
}

bool ctfTool::eventFilter(QObject *target, QEvent *event)
{
  if (target == defocus ||  target == defocusDifference || target == astigmatism)
  {
    if (event->type() == QEvent::KeyPress)
    {
      QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
      if (keyEvent->key() == Qt::Key_C) 
      {
        hideThonRings();
        return true;
      }
    }
  }
  return QDialog::eventFilter(target, event);
}

