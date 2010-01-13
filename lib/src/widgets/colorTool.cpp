/***************************************************************************
 *   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
 *   HStahlberg@ucdavis.edu                                                *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <colorTool.h>
#include <iostream>
using namespace std;

#define PI 3.14159265

colorTool::colorTool(fullScreenImage *sourceImage, mrcHeader *header, QWidget *parent)
                    :QDialog(parent)
{
  setModal(true);
  image = sourceImage;
  imageHeader = header;
  QGridLayout *layout = new QGridLayout(this);
  QLabel *brightness = new QLabel("Brightness:",this);
  QLabel *contrast = new QLabel("Contrast:",this);
  brightnessValue = new QLineEdit("0",this);
  contrastValue = new QLineEdit("0",this);
  brightnessValue->setMaximumWidth(8*4);
  contrastValue->setMaximumWidth(8*4);
  brightnessSlider = new QSlider(Qt::Horizontal,this);
  brightnessSlider->setRange(-100,100);
  brightnessSlider->setSingleStep(1);
  brightnessSlider->setValue(0);
  contrastSlider = new QSlider(Qt::Horizontal,this);
  contrastSlider->setRange(-100,100);
  contrastSlider->setSingleStep(1);
  contrastSlider->setValue(0);
  maxSpinBox = NULL;
  minSpinBox = NULL;

  invertImage = false;

  layout->addWidget(brightness,0,0,1,1);
  layout->addWidget(brightnessValue,0,1,1,1);
  layout->addWidget(brightnessSlider,1,0,1,2);
  layout->addWidget(contrast,2,0,1,1);
  layout->addWidget(contrastValue,2,1,1,1);
  layout->addWidget(contrastSlider,3,0,1,2);

  QAction *editFinished = new QAction(tr("Complete"),this);
  editFinished->setShortcuts(QList<QKeySequence>()<<tr("Enter")<<tr("Return"));
  connect(editFinished,SIGNAL(triggered()),this,SLOT(complete()));
  addAction(editFinished);

	maxSpinBox = new QDoubleSpinBox(this);
	maxSpinBox->setRange(-FLT_MAX,FLT_MAX);
	maxSpinBox->setMaximumWidth(8*20);
	maxSpinBox->setValue(sourceImage->imageMax());
	minSpinBox = new QDoubleSpinBox(this);
	minSpinBox->setRange(-FLT_MAX,FLT_MAX);
	minSpinBox->setMaximumWidth(8*20);
	minSpinBox->setValue(sourceImage->imageMin());

  QPushButton *rescaleButton = new QPushButton("Rescale",this);
	connect(rescaleButton,SIGNAL(clicked()),this,SLOT(rescale()));
  QPushButton *invertButton = new QPushButton("Invert",this);
  connect(invertButton,SIGNAL(clicked()),this,SLOT(invert()));

  layout->addWidget(new QLabel("Max:"),4,0,1,1);
  layout->addWidget(maxSpinBox,4,1,1,1);

	layout->addWidget(new QLabel("Min:"),5,0,1,1);
	layout->addWidget(minSpinBox,5,1,1,1);
	layout->addWidget(invertButton,6,0,1,1);
	layout->addWidget(rescaleButton,6,1,1,1);
	rescaledBrightness = true;
	rescaledContrast = true;

	connect(maxSpinBox,SIGNAL(editingFinished()),this,SLOT(completeMax()));
	connect(minSpinBox,SIGNAL(editingFinished()),this,SLOT(completeMin()));
	//  }
	//  else
	//  {
	//    layout->addWidget(invertButton,4,0,1,2);
	//  }

	if(header->mode() == 3 || header->mode() == 4)
	{
		showPhasesButton = new QPushButton("Show Phases");
		showPhasesButton->setCheckable(true);
		layout->addWidget(showPhasesButton, 7,0,1,2);
		connect(showPhasesButton,SIGNAL(toggled(bool)),this,SLOT(showPhase(bool)));
		connect(showPhasesButton,SIGNAL(toggled(bool)),this,SIGNAL(togglePhase(bool)));
	}

	connect(brightnessSlider,SIGNAL(valueChanged(int)),this,SLOT(setBrightness(int)));
	connect(contrastSlider,SIGNAL(valueChanged(int)),this,SLOT(setContrast(int)));

	setLayout(layout);
}

void colorTool::setBrightness(int brightness)
{
	brightnessValue->setText(QString().setNum(brightness));
	float max = image->imageMax(), min = image->imageMin();
	float offset = float(brightness)/200.0*(max-min);
	maxSpinBox->setValue(max-offset);
	minSpinBox->setValue(min-offset);
}

void colorTool::setContrast(int contrast)
{
	contrastValue->setText(QString().setNum(contrast));
	float max = image->imageMax(), min = image->imageMin();
	maxSpinBox->setValue(max-float(contrast)/200.0*(max-min));
	minSpinBox->setValue(min+float(contrast)/200.0*(max-min));
}

void colorTool::rescale()
{
	disconnect(brightnessSlider,SIGNAL(valueChanged(int)),this,SLOT(setBrightness(int)));
	disconnect(contrastSlider,SIGNAL(valueChanged(int)),this,SLOT(setContrast(int))); 

	contrastSlider->setValue(0); contrastValue->setText("0");
	brightnessSlider->setValue(0); brightnessValue->setText("0");
	image->rescale(minSpinBox->value(),maxSpinBox->value(),invertImage);

	connect(brightnessSlider,SIGNAL(valueChanged(int)),this,SLOT(setBrightness(int)));
	connect(contrastSlider,SIGNAL(valueChanged(int)),this,SLOT(setContrast(int))); 
}

void colorTool::complete()
{
	rescale();
	accept();
}

void colorTool::completeMax()
{
	if(maxSpinBox->hasFocus())
		complete();
}

void colorTool::completeMin()
{
	if(minSpinBox->hasFocus())
		complete();
}


void colorTool::focusInEvent(QFocusEvent *)
{
	if(maxSpinBox != NULL)
	{
		maxSpinBox->setFocus(Qt::PopupFocusReason);
		maxSpinBox->selectAll();
	}
}

void colorTool::invert()
{
	invertImage^=true;
	rescale();
	//if(imageHeader->mode()!=0) rescale();
	//  else
	//  {
	//    image->getImage()->invertPixels();
	//    *(image->getPixmap()) = QPixmap::fromImage(*(image->getImage()));
	//    image->update();
	//  }
	emit toggleInvert(invertImage);
	accept();
}

void colorTool::showEvent(QShowEvent *event)
{
	QWidget::showEvent(event);
	//  setFocus(Qt::PopupFocusReason);
}

void colorTool::showPhase(bool show)
{
	image->getSourceImage()->setViewPhase(show);
	*(image->getPixmap()) = QPixmap::fromImage(*((image->getSourceImage()->getImage())));
	rescale();
	if(show)
		showPhasesButton->setText("Hide Phases");
	else
		showPhasesButton->setText("Show Phases");
	accept();
}

