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

#include "scriptProgress.h"

scriptProgress::scriptProgress(QWidget *parent)
                              :QProgressBar(parent)
{
  setFixedWidth(560);
  setFixedHeight(48);
  progressType = aqua;
  setValue(0);
}

scriptProgress::~scriptProgress()
{
}

void scriptProgress::incrementValue(int inc)
{
	if(value()+inc<=maximum())
		setValue(value()+inc);
  else
   setValue(maximum());
}

void scriptProgress::paintEvent(QPaintEvent *event)
{
  QWidget::paintEvent(event);

  QPainter imageBase(this);
  QLinearGradient grad(QPoint(0,0),QPoint(0,height()));
  grad.setColorAt(1,QColor(214,219,191));
  grad.setColorAt(0.5,QColor(233,237,211));
  grad.setColorAt(0,QColor(254,255,232));
  imageBase.setPen(QColor(0,0,0,0));
  imageBase.setBrush(grad);

  imageBase.drawRect(0,0,width(),height());

  int borderAlpha = 150;

  imageBase.setPen(QPen(QColor(200,200,200,borderAlpha),3, Qt::SolidLine, Qt::RoundCap));
  imageBase.drawLine(0,0,0,height());

  imageBase.setPen(QPen(QColor(200,200,200,borderAlpha),3, Qt::SolidLine, Qt::RoundCap));
  imageBase.drawLine(width(),0,width(),height());

  imageBase.setPen(QPen(QColor(70,70,70,borderAlpha),3, Qt::SolidLine, Qt::RoundCap));
  imageBase.drawLine(0,0,width(),0);

  imageBase.setPen(QPen(QColor(255,255,255,borderAlpha),3, Qt::SolidLine, Qt::RoundCap));
  imageBase.drawLine(0,height(),width(),height());


  if(progressType != none)
  {
    imageBase.setPen(QColor(0,0,0,255));
    imageBase.setFont(QFont("Times",18,QFont::Bold));
    imageBase.drawText(QRect(0,0,width(),3*height()/4), Qt::AlignHCenter | Qt::AlignVCenter, title);
  }

	int max = (345 * value())/maximum();
  QLinearGradient progressBarGrad(QPoint((1024-345)/2,3*height()/4-5),QPoint((width()-345)/2,3*height()/4+5));
  if(progressType == aqua)
  {
    progressBarGrad.setColorAt(1,QColor(184,250,255));
    progressBarGrad.setColorAt(0.5,QColor(106,170,235));
    progressBarGrad.setColorAt(0,QColor(208,224,244));
    imageBase.setPen(QColor(0,0,0,0));
    imageBase.setBrush(progressBarGrad);
    imageBase.drawRect((width()-345)/2,0+3*height()/4-5,345*value()/maximum(),10);
  }
  else if(progressType == overlay)
  {
    progressBarGrad.setColorAt(1,QColor(149,153,133));
    progressBarGrad.setColorAt(.99,QColor(69,72,62));
    progressBarGrad.setColorAt(0.5,QColor(161,164,144));
    progressBarGrad.setColorAt(0,QColor(215,219,193));;
    imageBase.setPen(QColor(0,0,0,0));
    imageBase.setBrush(progressBarGrad);
    imageBase.drawRect((width()-345)/2,0+3*height()/4-5,345*value()/maximum(),10);
  }
  else if(progressType == silver)
  {
    progressBarGrad.setColorAt(1,QColor(42,45,43));
    progressBarGrad.setColorAt(0.5,QColor(161,164,165));
    progressBarGrad.setColorAt(0,QColor(255,255,255));;
    imageBase.setPen(QColor(0,0,0,0));
    imageBase.setBrush(progressBarGrad);
    imageBase.drawRect((width()-345)/2,0+3*height()/4-5,345*value()/maximum(),10);
  }
  else if(progressType == ticks)
  {
    imageBase.setPen(QColor(163,167,140,255));
    for(int i=0;i<max;i+=2)
      imageBase.drawLine(QPoint((width()-345)/2+i,3*height()/4-5),QPoint((width()-345)/2+i,3*height()/4+5));
  }

	imageBase.setBrush(Qt::black);
	QPoint list[4];
	list[0]=QPoint((width()-345)/2+max-5, 3*height()/4);
	list[1]=QPoint((width()-345)/2+max, 3*height()/4-5);
	list[2]=QPoint((width()-345)/2+max+5, 3*height()/4);
	list[3]=QPoint((width()-345)/2+max, 3*height()/4+5);
	imageBase.drawConvexPolygon(list,4);

	if(progressType != none)
	{
		imageBase.setBrush(QColor(0,0,0,0));
		imageBase.setPen(QColor(0,0,0,255));
		imageBase.drawRect(0+(width()-345)/2,0+3*height()/4-5,345,10);
	}
	else
	{
		imageBase.setBrush(QColor(0,0,0,255));
		imageBase.setPen(QColor(0,0,0));
		imageBase.setFont(QFont("Times",18,QFont::Bold,true));
		imageBase.drawText(QRect(0,0,width(),3*height()/4), Qt::AlignHCenter | Qt::AlignVCenter, "2dx_image");
		//    imageBase.setFont(QFont("Times",14,QFont::Bold,true));
		//    imageBase.drawText(QRect(0,height()/2,width(),height()/2), Qt::AlignHCenter | Qt::AlignVCenter, "Version 2.0.1 (October, 2006)");
	}
}

void scriptProgress::setText(QString text)
{
	title = text;
	update();
}

void scriptProgress::setProgressType(scriptProgress::barType type)
{
	progressType = type;
}


