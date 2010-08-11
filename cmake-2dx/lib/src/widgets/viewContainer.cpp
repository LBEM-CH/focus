/**************************************************************************
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

#include <QApplication>
#include <QDesktopWidget>
#include "viewContainer.h"
#include <iostream>
using namespace std;

viewContainer::titleBar::titleBar(QString title, viewContainer::type viewType, QWidget *parent, viewContainer::styleType gradientStyle)
				 :QWidget(parent)
{
  headerWidgets = 0;
  setFocusPolicy(Qt::NoFocus);
  layout = new QGridLayout(this);
//  layout->setAlignment(Qt::AlignTop);
  QPalette pal(palette());
  pal.setColor(QPalette::Light,QColor(70,70,70,100));
  pal.setColor(QPalette::Dark,QColor(255,255,255,100));
  if(gradientStyle == viewContainer::grey) pal.setColor(QPalette::WindowText,QColor(0,0,0));
  if(gradientStyle == viewContainer::black) pal.setColor(QPalette::WindowText,QColor(255,255,255));
  setPalette(pal);
  layout->setMargin(0);
  layout->setSpacing(0);
  setAutoFillBackground(true);
  setFixedHeight(19);
  QPalette titlePal(palette());
  QLinearGradient grad(QPoint(0,0),QPoint(0,height()));
  if(gradientStyle == viewContainer::grey)
  {
    grad.setColorAt(1,QColor(123,122,122));
    grad.setColorAt(0,QColor(227,227,227));
  }
  else if(gradientStyle == viewContainer::black)
	{
		/*    grad.setColorAt(0.0,QColor(69,69,69));
					grad.setColorAt(0.333,QColor(45,45,45));
					grad.setColorAt(0.55,QColor(0,0,0));
					grad.setColorAt(1.0,QColor(33,33,33));
    */
		grad.setColorAt(0.0,QColor(169*123.0/113.0,169*123.0/113.0,169*123.0/113.0));
		grad.setColorAt(0.333,QColor(145*123.0/113.0,145*123.0/113.0,145*123.0/113.0));
		grad.setColorAt(0.55,QColor(110*227.0/172.0,110*227.0/172.0,110*227.0/172.0));
		grad.setColorAt(1.0,QColor(133*227.0/172.0,133*227.0/172.0,133*227.0/172.0));
	}

	titlePal.setBrush(QPalette::Background,QBrush(grad));
	setPalette(titlePal);

	if(viewType == control)
		for(int i=0;i<3;i++)
		{
			QFrame *vBar = new QFrame(this);
			vBar->setFixedHeight(12);
			vBar->setFrameStyle(QFrame::VLine | QFrame::Sunken);
			layout->addWidget(vBar,0,2+i,1,1);
		}
	layout->addItem(new QSpacerItem(3,3),0,6,1,1);

	QFont font("Times", 12, QFont::Light);
	//if(gradientStyle == viewContainer::grey) font = QFont
	//if(gradientStyle == viewContainer::black) font = QFont("Times", 12, QFont::Bold);

	titleText = new QLabel(title,this);
	titleText->setFont(font);
	titleText->setAutoFillBackground(false);
	titleText->setAlignment(Qt::AlignCenter);
	//titleText->setPalette(pal);
	layout->addWidget(titleText,0,1,1,1);
	layout->setAlignment(titleText,Qt::AlignCenter);

	setLayout(layout);
}

void viewContainer::titleBar::setTitle(const QString &title)
{
	titleText->setText(title);
}

void viewContainer::titleBar::setWidget(QWidget *widget, Qt::Alignment align)
{
	if(align & Qt::AlignLeft)
	{
		layout->addWidget(widget,0,0,1,1);
		headerWidgets++;
		if(headerWidgets==1)
		{
			QSpacerItem *spacer = new QSpacerItem(widget->width(),widget->height(),QSizePolicy::Maximum);
			layout->addItem(spacer,0,2,1,1);
		}
	}
	else if(align & Qt::AlignRight)
	{
		if(headerWidgets==0)
		{
			QSpacerItem *spacer = new QSpacerItem(widget->width(),widget->height(),QSizePolicy::Maximum);
			layout->addItem(spacer,0,0,1,1);
		}

		layout->addWidget(widget,0,2,1,1);
		headerWidgets++;
	}

	for(int i=0;i<3;i++)
		layout->setColumnStretch(i,1);
}

void viewContainer::initialize(QString titleText, viewContainer::type viewType, viewContainer::styleType gradientType)
{
	int minWidth = int(QApplication::desktop()->width()/5.00);
	if(minWidth>235) minWidth = 235;
	setAutoFillBackground(true);

	QPalette pal(palette());
	//  pal.setColor(QPalette::Dark,QColor(255,255,255));
	if(viewType == control)
	{
		pal.setColor(QPalette::Window,QColor(226,234,244));
		//setFixedWidth(minWidth);
	}
	else if(viewType == image)
	{
		pal.setColor(QPalette::Window,Qt::white);
		//setFixedWidth(minWidth);
	}
	else if (viewType == data)
		pal.setColor(QPalette::Window,Qt::white);

	setPalette(pal);
	setFrameStyle(QFrame::Panel | QFrame::Raised);
	setLineWidth(1);

	layout = new QVBoxLayout(this);
	layout->setAlignment(Qt::AlignTop);
	layout->setMargin(0);
	layout->setSpacing(0);
	//  layout->setColumnStretch(0,2);

	title = new titleBar(titleText,viewType,this,gradientType);
	layout->addWidget(title);

	if(viewType == control || viewType == data || viewType == image)
	{
		QFrame *hl = new QFrame(this);
		hl->setFrameStyle(QFrame::HLine | QFrame::Plain);
		hl->setFixedHeight(1);
		layout->addWidget(hl);
	}
	setLayout(layout);

	containerSplitter = NULL;
	maximizedWindow = -1;
}

	viewContainer::viewContainer(QString title, viewContainer::type viewType, QWidget *parent, viewContainer::styleType gradientStyle)
:QFrame(parent)
{
	initialize(title,viewType,gradientStyle);
}

	viewContainer::viewContainer(QString title, QWidget *parent, viewContainer::styleType gradientStyle)
:QFrame(parent)
{
	initialize(title,control,gradientStyle);
}

void viewContainer::setHeaderWidget(QWidget *widget, Qt::Alignment align)
{
	title->setWidget(widget,align);
}

void viewContainer::addWidget(QWidget *widget, bool addToSplitter)
{
	if(widget->parent()==NULL) widget->setParent(this);
	if(addToSplitter)
		addSplitterWidget(widget);
	else
		layout->addWidget(widget);
	//setMinimumWidth(widget->width());
}

void viewContainer::addSplitterWidget(QWidget *widget)
{
	if(containerSplitter == NULL)
	{
		containerSplitter = new QSplitter(Qt::Vertical, this);
		layout->addWidget(containerSplitter);
	}

	containerSplitter->addWidget(widget);
}

void viewContainer::addScrollWidget(QWidget *widget, bool addToSplitter)
{
	QScrollArea *window = new QScrollArea(this);
	window->setWidgetResizable(true);
	window->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
	window->setWidget(widget);
	//Fixme horizontalScrollBar() width + widget width produces incorrect results on linux.
	//setMinimumWidth(widget->width()+30);
	//  setBaseSize(QSize(widget->width()+30,235)); //window->horizontalScrollBar()->width());
	if(addToSplitter)
		addSplitterWidget(window);
	else
		layout->addWidget(window);
}

void viewContainer::mouseDoubleClickEvent(QMouseEvent *event)
{
	QWidget::mouseDoubleClickEvent(event);
	emit doubleClicked();
}

void viewContainer::shade()
{
	if(isHidden())
		show();
	else
		hide();
}

void viewContainer::setText(const QString &text)
{
	title->setTitle(text);
}

void viewContainer::showAll()
{
	for(int i=0;i<containerSplitter->sizes().size();i++)
		containerSplitter->widget(i)->show();
}

void viewContainer::maximizeWindow(int index)
{
	if(index == -1) {maximizedWindow = index;}
	if(index!=-1 && maximizedWindow == -1) saveSplitterState();
	if(containerSplitter == NULL) return;
	QList<int> sizes = containerSplitter->sizes();
	int max = 0;

	for(int i=0;i<sizes.size();i++)
		max+=sizes[i];

	for(int i=0;i<sizes.size();i++)
		if(index==-1 || i == index) sizes[i] = 1;
		else sizes[i] = 0;//max - sizes.size() + 1;

		containerSplitter->setSizes(sizes);
		maximizedWindow = index;
}

void viewContainer::saveSplitterState(int state)
{
	splitterState[state] = containerSplitter->saveState();
}

void viewContainer::restoreSplitterState(int state)
{
	containerSplitter->restoreState(splitterState[state]);
}

void viewContainer::setHeaderToolTip(const QString &string)
{
	title->setToolTip(string);
}

void viewContainer::resizeSplitter()
{
	for(int i=0;i<containerSplitter->count();i++)
	{
		//    if(containerSplitter->widget(i)->isHidden())
		//      containerSplitter->setStretchFactor(i,0);
		//    else
		containerSplitter->setStretchFactor(i,10);
	}
}


