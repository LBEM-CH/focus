/*
 *  controlBar.cpp
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/8/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "controlBar.h"
#include <QPalette>
#include <QLinearGradient>
#include <iostream>
using namespace std;


controlBar::controlBar(confData *data, controlBar::type viewType, QWidget *parent)
                        :QWidget(parent)
{
  conf = data;
  if(viewType == footer)
    setFixedHeight(41);
  else
    setFixedHeight(93);
  setAutoFillBackground(true);
  QPalette pal(palette());
  QLinearGradient grad(QPoint(0,0),QPoint(0,height()));
  grad.setColorAt(1,QColor(113,113,114));
  grad.setColorAt(0,QColor(172,172,171));
  pal.setBrush(QPalette::Background,QBrush(grad));
  setPalette(pal);
  layout = new QHBoxLayout;

  if(viewType == actions)
  {	
    saveButton = new graphicalButton(conf->getIcon("gear"),this);
    saveButton->setToolTip("Save");
    saveButton->setCheckable(false);
    saveButton->setChecked(false);
  		
    executeButton = new graphicalButton(conf->getIcon("play"),this);
    executeButton->setCheckable(true);
    executeButton->setChecked(false);
    executeButton->setToolTip("Run current script");

    layout->addWidget(saveButton);
    layout->addWidget(executeButton);	

    connect(saveButton,SIGNAL(clicked()),this,SLOT(saveClicked()));
    connect(executeButton,SIGNAL(clicked()),this,SLOT(executeClicked()));
  }
  else if(viewType == viewActions)
  {
    graphicalButton *refreshButton = new graphicalButton(conf->getIcon("refreshButton"),this);
    refreshButton->setCheckable(false);
    refreshButton->setToolTip("Refresh results");
    connect(refreshButton,SIGNAL(clicked()),this,SIGNAL(refresh()));

    helpButton = new graphicalButton(conf->getIcon("helpButton"),this);
    helpButton->setCheckable(true);
    helpButton->setToolTip("Show/Hide script manual");
    connect(helpButton,SIGNAL(clicked()),this,SIGNAL(toggleManual()));

//    layout->addStretch(100);
    layout->addWidget(helpButton);
    layout->addWidget(refreshButton);
//    layout->addStretch(1);
  }
  else if(viewType == progress)
  {
    progressBar = new scriptProgress(this);
    progressBar->setMaximum(100);
    progressBar->setProgressType(scriptProgress::ticks);
    layout->addWidget(progressBar);
  }
  else if(viewType == header)
  {
    progressBar = new scriptProgress(this);
    progressBar->setProgressType(scriptProgress::none);
    layout->addWidget(progressBar);
  }
  else if(viewType == footer)
  {
    layout->setAlignment(Qt::AlignLeft);
	
    graphicalButton *previewControlButton = new graphicalButton(conf->getIcon("previewControl"),this);
    previewControlButton->setCheckable(true);
    previewControlButton->setToolTip("Show/Hide image preview");

    graphicalButton *infoControlButton = new graphicalButton(conf->getIcon("infoButton"),this);
    infoControlButton->setCheckable(true);
    infoControlButton->setChecked(true);
    infoControlButton->setToolTip("Show/Hide image header information");

    graphicalButton *manualButton = new graphicalButton(conf->getIcon("manualButton"),this);
    manualButton->setCheckable(false);
    manualButton->setToolTip("View online help");

    layout->addWidget(infoControlButton);
    layout->addWidget(previewControlButton);
    layout->addStretch(50);
    layout->addStretch(60);
    layout->addWidget(manualButton);
    layout->addStretch(5);

    layout->setAlignment(manualButton,Qt::AlignRight);

    connect(previewControlButton,SIGNAL(clicked()),this,SIGNAL(hideWidget()));
    connect(infoControlButton,SIGNAL(clicked()),this,SIGNAL(toggleInfo()));
    connect(manualButton,SIGNAL(clicked()),this,SIGNAL(viewHelp()));
  }
  setLayout(layout);
}

controlBar::~controlBar()
{

}

void controlBar::saveClicked()
{
  if(saveButton->isCheckable())
  {
    emit save();
//    saveButton->setCheckable(false);
  }
}

void controlBar::saveAvailable(bool available)
{
  if(available)
  {
    saveButton->setCheckable(true);
    saveButton->setChecked(true);
  }
  else
  {
    saveButton->setChecked(false);
    saveButton->setCheckable(false);
  }
}

void controlBar::executeClicked()
{
  executeButton->setToolTip("Stop current script");
  emit execute();
}


void controlBar::setProgress(int value)
{
  progressBar->setValue(value);
}

void controlBar::setText(QString text)
{
  progressBar->setText(text);
}

void controlBar::scriptFinished()
{
  executeButton->setChecked(false);
  executeButton->setToolTip("Run current script");
  update();
}

void controlBar::setManual(bool show)
{
  helpButton->setChecked(show);
}
