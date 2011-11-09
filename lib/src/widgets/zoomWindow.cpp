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

#include "zoomWindow.h"
#include <iostream>
using namespace std;

zoomWindow::zoomWindow(QWidget *imageWidget, QWidget *parent)
                      :QWidget(parent, Qt::Tool)
{
  widget = imageWidget;
  p=parent;
  QPalette pal(palette());
  pal.setBrush(QPalette::Background,QColor(126,126,127));
  setPalette(pal);
  setMouseTracking(true);
  zoomPos = QPoint(0,0);
/*  QGridLayout *layout = new QGridLayout(this);
  layout->setMargin(0);
  layout->setSpacing(0);
  setLayout(layout);
*/
}

void zoomWindow::zoom(const QPoint &pos)
{
  zoomPos = pos;
  zoomX = 256/4;
  zoomY = 256/4;

  int x = pos.x()-zoomX/2, y=pos.y()-zoomY/2;
  
  image = QPixmap::grabWidget(widget,x,y,zoomX,zoomY);
  repaint();
}

void zoomWindow::zoom()
{
  zoom(zoomPos);
}

void zoomWindow::mousePressEvent(QMouseEvent *event)
{
  if(event->button()==Qt::LeftButton)
  {
    zoomX = 256/4;
    zoomY = 256/4;
    float x = (float)event->x()/(float)(width())*float(zoomX), y = (float)event->y()/(float)height()*zoomY;
    if(x<zoomX/2.0) x-=1.0;
    if(y<zoomY/2.0) y-=1.0;

    QPoint offset((int)(x-zoomX/2.0),(int)(y - zoomY/2.0));
    emit zoomClick(zoomPos+offset);
    zoom(zoomPos);
  }
  if(event->button()==Qt::MidButton)
  {
    hide();
  }
}

void zoomWindow::mouseDoubleClickEvent(QMouseEvent *event)
{
  if(event->button()==Qt::LeftButton)
  {
    zoomX = 256/4;
    zoomY = 256/4;
    float x = (float)event->x()/(float)(width())*float(zoomX), y = (float)event->y()/(float)height()*zoomY;
    if(x<zoomX/2.0) x-=1.0;
    if(y<zoomY/2.0) y-=1.0;

    QPoint offset((int)(x-zoomX/2.0),(int)(y - zoomY/2.0));
    emit zoomDoubleClick(zoomPos+offset);
 //   zoom(zoomPos);
  }
  if(event->button()==Qt::MidButton)
  {
    hide();
  }
}

void zoomWindow::mouseMoveEvent(QMouseEvent *event)
{
  emit zoomMove(currentPosition(event));
}

QPoint zoomWindow::currentPosition(QMouseEvent *event)
{
  zoomX = 256/4;
  zoomY = 256/4;
  float x = (float)event->x()/(float)(width())*float(zoomX), y = (float)event->y()/(float)height()*zoomY;
  if(x<zoomX/2.0) x-=1.0;
  if(y<zoomY/2.0) y-=1.0;

  QPoint offset((int)(x-zoomX/2.0),(int)(y - zoomY/2.0));  	
	
  return offset+zoomPos;
}

void zoomWindow::paintEvent(QPaintEvent *event)
{
  if(!image.isNull())
  {
    QWidget::paintEvent(event);
    QPainter p(this);
    p.drawPixmap(0,0,image.scaled(rect().size(),Qt::IgnoreAspectRatio,Qt::FastTransformation));
  }
}

void zoomWindow::resizeEvent(QResizeEvent *event)
{
  resize(event->size().width(),event->size().width());
  QWidget::resizeEvent(event);
}

