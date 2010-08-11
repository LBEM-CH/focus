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

#include <mrcGraphicsItem.h>

mrcGraphicsItem::mrcGraphicsItem(largeMRC *imageFile, int x, int y, int *t, QGraphicsItem *parent)
:QThread(NULL), QGraphicsPixmapItem(parent)
{
	image = imageFile;
	pX = x;
	pY = y;
  optimalThreads = idealThreadCount();
  if(optimalThreads<=0) optimalThreads = 1;
  threadCount = t;
  timer = -1;
  setTerminationEnabled();
	setPixmap(QPixmap(image->pixmapWidth(x,y),image->pixmapHeight(x,y)));
	//connect(this,SIGNAL(load()),this,SLOT(loadPixmap()));

	if(image->isFFT())
	{
		if(pX>=0) setOffset(pX,pY-(float)(image->height())/2.0);
		else setOffset(pX-(float)image->pixmapWidth(pX,pY)+1,(float)image->height()+pY-image->pixmapHeight(pX,pY)+1-(float)(image->height())/2.0);
	}
	else
		setOffset(pX,pY);
	pixmapLoaded = false;
}

mrcGraphicsItem::~mrcGraphicsItem()
{
	//qDebug()<<"mrcGraphicsItem "<<pX<<" "<<pY<<"  released.";
  if(isRunning()) wait();
}

void mrcGraphicsItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget* widget)
{
	if(!pixmapLoaded)
  {
    if((*threadCount)<optimalThreads) run();
    else if(timer == -1) timer=startTimer(5);  
  }
  else
  {
    painter->setRenderHint(QPainter::Antialiasing,false);
		QGraphicsPixmapItem::paint(painter,option,widget);
  }
}

void mrcGraphicsItem::run()
{
	pixmapLoaded = true;
  mutex.lock();
  (*threadCount)++;
  mutex.unlock();

	loadPixmap();

  mutex.lock();
  (*threadCount)--;
  mutex.unlock();
}

void mrcGraphicsItem::timerEvent(QTimerEvent *event)
{
  //qDebug()<<"wait...";
  if(*threadCount<0) {qDebug()<<"Something crazy has happened..."; return;}
	if(*threadCount<optimalThreads) {killTimer(timer); start();}
}

void mrcGraphicsItem::loadPixmap()
{
  mutex.lock();
  setPixmap(image->pixmap(pX,pY));
  mutex.unlock();
  update(boundingRect()); 
  //emit updated(sceneBoundingRect());
  if(scene()!=NULL && scene()->views().size()>0)
    scene()->views().first()->updateScene(QList<QRectF>()<<sceneBoundingRect());
}

