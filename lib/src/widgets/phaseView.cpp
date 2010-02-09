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

#include <phaseView.h>

#ifndef M_PI
#define M_PI 3.1415926
#endif

phaseView::phaseView(QWidget *parent)
          :QWidget(parent,Qt::Tool)
{
  QVBoxLayout *layout = new QVBoxLayout(this);
  setLayout(layout);
  layout->setMargin(0);
  layout->setSpacing(0);

  colorWheel = new QGraphicsView(this);
  layout->addWidget(colorWheel);

  QGraphicsScene *scene = new QGraphicsScene;
  colorWheel->setScene(scene);
  //scene->setSceneRect(0,0,170,170);
  gradient = new QConicalGradient(50,50,0);
  iGradient = new QConicalGradient(50,50,180);

  QColor color;
  float a, la;
  QGraphicsTextItem *item;
  for(int i=0;i<=256;i++)
  {
    la = (float)i/256.0;
    a = la*2.0*M_PI;
    if(i%64 == 0 && i!=256)
    {
      item = new QGraphicsTextItem;
      item->setZValue(0.1);
      item->setHtml(QString::number(la*360.0));
      item->setPos(50.0 * (1.0 + 1.3*cos(a)) - item->boundingRect().width()/2.0, 50.0 * (1.0 - 1.3*sin(a)) - item->boundingRect().height()/2.0);
      scene->addItem(item);
     }
     color.setHsvF(la,1.0,1.0);
     gradient->setColorAt(1.0-la, color);
     iGradient->setColorAt(1.0-la, color);
   }

   colorWheelEllipse = new QGraphicsEllipseItem(QRectF(0,0,100,100));
   colorWheelEllipse->setPen(QPen(QColor(255,255,255,1)));
   colorWheelEllipse->setBrush(*gradient);
   scene->addItem(colorWheelEllipse);

   phaseIndicator = new QGraphicsLineItem;
   phaseIndicator->setPen(QPen(Qt::black));
   phaseIndicator->setZValue(1.0);
   scene->addItem(phaseIndicator);

   colorWheel->setFrameStyle(QFrame::NoFrame);
   //colorWheel->setBackgroundBrush(Qt::white);
   colorWheel->setFixedHeight(170);
   colorWheel->setFixedWidth(170);
   setFixedSize(QSize(170,170));
}

void phaseView::setPhase(float theta)
{
  phaseIndicator->setLine(50.0,50.0,50.0*(1.0+cos(theta)),50.0*(1.0 - sin(theta)));
}

void phaseView::show(bool view)
{
  if(view) showNormal();
  else hide();
}

void phaseView::invert(bool value)
{
  if(value)
    colorWheelEllipse->setBrush(*iGradient);
  else
    colorWheelEllipse->setBrush(*gradient);
  colorWheel->update();
}

