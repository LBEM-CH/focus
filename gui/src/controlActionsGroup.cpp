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

#include "controlActionsGroup.h"
#include <QPalette>
#include <QLinearGradient>
#include <iostream>
using namespace std;


controlActionsGroup::controlActionsGroup(confData *data, controlActionsGroup::type viewType, QWidget *parent)
                        :QWidget(parent)
{
  conf = data;
  if(viewType == footer)
    //setFixedHeight(50);
    setMinimumHeight(41);
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
    saveButton = new graphicalButton(conf->getIcon("save"),this);
    saveButton->setToolTip("Save");
    saveButton->setCheckable(false);
    saveButton->setChecked(false);
  		
    executeButton = new graphicalButton(conf->getIcon("play"),this);
    executeButton->setCheckable(true);
    executeButton->setChecked(false);
    executeButton->setToolTip("Run current script");
    connect(executeButton,SIGNAL(toggled(bool)),this,SIGNAL(execute(bool)));

    layout->addWidget(saveButton);
    layout->addWidget(executeButton);	

    connect(saveButton,SIGNAL(clicked()),this,SLOT(saveClicked()));
    connect(executeButton,SIGNAL(clicked()),this,SLOT(executeClicked()));
  }
  else if(viewType == viewActions)
  {
    graphicalButton *refreshButton = new graphicalButton(conf->getIcon("refresh"),this);
    refreshButton->setCheckable(false);
    refreshButton->setToolTip("Refresh results");
    connect(refreshButton,SIGNAL(clicked()),this,SIGNAL(refresh()));

    helpButton = new graphicalButton(conf->getIcon("help"),this);
    helpButton->setCheckable(true);
    helpButton->setToolTip("Show/Hide script manual");
    connect(helpButton,SIGNAL(toggled(bool)),this,SIGNAL(toggleManual(bool)));

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

    graphicalButton *infoControlButton = new graphicalButton(conf->getIcon("info"),this);
    infoControlButton->setCheckable(true);
    infoControlButton->setChecked(true);
    infoControlButton->setToolTip("Show/Hide image header information");

    progressStamps *processingProgress = new progressStamps(data, this);

    graphicalButton *manualButton = new graphicalButton(conf->getIcon("manual"),this);
    manualButton->setCheckable(false);
    manualButton->setToolTip("View online help");

    graphicalButton *bugReport = new graphicalButton(conf->getIcon("bug"),this);
    bugReport->setToolTip("Report Issue/Bug");

    layout->addWidget(infoControlButton);
    layout->addWidget(previewControlButton);
    layout->addStretch(50);
    layout->addWidget(processingProgress);
    layout->addStretch(60);
    layout->addWidget(manualButton);
    layout->addWidget(bugReport);
    layout->addStretch(1);

    layout->setAlignment(manualButton,Qt::AlignRight);

    connect(previewControlButton,SIGNAL(clicked()),this,SIGNAL(hideWidget()));
    connect(infoControlButton,SIGNAL(clicked()),this,SIGNAL(toggleInfo()));
    connect(manualButton,SIGNAL(clicked()),this,SIGNAL(viewHelp()));
    connect(bugReport,SIGNAL(clicked()),this,SIGNAL(reportBug()));
  }
  setLayout(layout);
}

controlActionsGroup::~controlActionsGroup()
{

}

void controlActionsGroup::saveClicked()
{
  if(saveButton->isCheckable())
  {
    emit save();
//    saveButton->setCheckable(false);
  }
}

void controlActionsGroup::saveAvailable(bool available)
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

void controlActionsGroup::executeClicked()
{
  executeButton->setToolTip("Stop current script");
}


void controlActionsGroup::setProgress(int value)
{
  progressBar->setValue(value);
}

void controlActionsGroup::incrementProgress(int inc)
{
  progressBar->incrementValue(inc);
}

void controlActionsGroup::setText(QString text)
{
  progressBar->setText(text);
  update();
}

void controlActionsGroup::scriptFinished()
{
  executeButton->setChecked(false);
  executeButton->setToolTip("Run current script");
  update();
}

void controlActionsGroup::setManual(bool show)
{
  helpButton->setChecked(show);
}
