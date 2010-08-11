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

#include "navigator.h"
#include <iostream>
using namespace std;

navigator::navigator(confData *conf, const QString &imageName, QWidget *parent)
          :QGraphicsView(parent)
{
  setWindowFlags(Qt::Window);
  setAttribute(Qt::WA_DeleteOnClose,true);

  data = conf;
  image = new mrcImage(imageName,false,this);
  if(image->isEmpty()) close();

  scene = new QGraphicsScene(this);
  setCacheMode(QGraphicsView::CacheBackground);
  setDragMode(QGraphicsView::ScrollHandDrag);
  setTransformationAnchor(QGraphicsView::AnchorViewCenter);
  scene->setSceneRect(-image->getPixmap().width()/2.0,-image->getPixmap().height()/2.0,image->getPixmap().width(),image->getPixmap().height());
  scene->setBackgroundBrush(Qt::black);
  QGraphicsPixmapItem *b = scene->addPixmap(image->getPixmap());
  b->setZValue(-1.0);
  b->setPos(scene->sceneRect().topLeft());


  initializeActions();
  initializeOverlay();

  setScene(scene);
  showFullScreen();
}

void navigator::initializeActions()
{
  QAction *closeAction = new QAction("Close",this);
  closeAction->setShortcut(tr("Esc"));
  addAction(closeAction);
  connect(closeAction,SIGNAL(triggered()),this,SLOT(close()));

  QAction *centerAction = new QAction("Center",this);
  centerAction->setShortcut(tr("Space"));
  addAction(centerAction);
  connect(centerAction,SIGNAL(triggered()),this,SLOT(center()));

  QAction *toggleLatticeAction = new QAction("View Lattice",this);
  toggleLatticeAction->setShortcut(tr("L"));
  toggleLatticeAction->setCheckable(true);
  addAction(toggleLatticeAction);
  connect(toggleLatticeAction,SIGNAL(toggled(bool)),this,SLOT(toggleLattice(bool)));

}

void navigator::initializeLattice()
{
  QPointF l[2] = { data->get("lattice")->toQPointF(),  data->get("lattice")->toQPointF(1) };
  cerr<<l[0].x()<<", "<<l[0].y()<<", "<<l[1].x()<<", "<<l[1].y()<<endl;
  primaryLattice = new QGraphicsItemGroup;
  QGraphicsEllipseItem *ellipse;

  for(int i = -20; i <= 20; i++)
    for(int j = -20; j <= 20; j++)
    {
      QPointF p = QPointF(i*l[0].x()+j*l[1].x(),-i*l[0].y()-j*l[1].y());
      ellipse = scene->addEllipse(QRectF(p.x()-19.0/2.0,p.y()-19.0/2.0,19.0,19.0),QPen(Qt::green));
      primaryLattice->addToGroup(ellipse);
    }
}

void navigator::initializeOverlay()
{
  initializeLattice();
  updateSceneRect(sceneRect());
}

void navigator::center()
{
}

void navigator::toggleItem(QGraphicsItem *item, bool visible)
{
  item->setVisible(visible);
}

void navigator::toggleLattice(bool visible)
{
  toggleItem(primaryLattice,visible);
}
