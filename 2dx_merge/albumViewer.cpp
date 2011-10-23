/**************************************************************************
*   Copyright (C) 2006 by the Stahlberg laboratory                        *
*   Henning.Stahlberg@unibas.ch                                           *
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

#include "albumViewer.h"
#include <QDebug>

#ifdef PI
#undef PI
#endif
#define PI 3.141592653589793

albumViewer::albumViewer(QWidget *parent)
            :QWidget(parent,Qt::Window)
{
//  setAttribute(Qt::WA_DeleteOnClose);
  data = NULL;
  image = NULL;
  pixmap = NULL;
  phaseOrigin = QPointF(0,0);
  resize(400,400);
  show();
}

void albumViewer::setConf(confData *conf)
{
  data = conf;
  loadConfParameters();
}

void albumViewer::setConf(const QString &confName)
{
  if(data != NULL) delete data;
  data = new confData(confName);
  data->syncWithUpper();
  loadConfParameters();
///  qDebug()<<confName;
}

void albumViewer::setImage(const QString &fileName)
{
  imageName = fileName;
  if(image!=NULL) delete image;
  image = new mrcImage(fileName);
  imageHeader = image->getHeader();
  pixmap = &image->getPixmap();
  loadCellParameters();
  update();
}

void albumViewer::guessConfFromImage()
{
  
}

void albumViewer::paintEvent(QPaintEvent */*event*/)
{
  if(pixmap==NULL) return;
  QPainter *painter = new QPainter(this);

  //  shearScale = float(pixmap->height())*tan((cellC-90.0)*PI/180.0)/float(pixmap->width());
  //  if(cellA!=0.0 && cellB!=0.0 && nx!=0.0 && ny!=0.0) pixelSize = (cellB*nx)/(cellA*ny);
  //  else pixelSize = 1.0;
  shearScale = -1.0/tan((cellC)*PI/180.0);
  if(cellA!=0.0 && cellB!=0.0 && nx!=0.0 && ny!=0.0) pixelSize = cellB/cellA*1.0/sqrt(fabs(1+shearScale*shearScale));
  else pixelSize = 1.0;

  QPointF pOrigin = QPointF(phaseOrigin.x()/360.0*pixmap->width(), phaseOrigin.y()/360.0*pixmap->height());
  
  painter->setViewTransformEnabled(true);
  painter->translate(QPointF(float(width())/2.0,float(height())/2.0));
  if(cellC!=0.0 && cellC!=90.0) painter->shear(shearScale,0.0);
  // if(pixelSize!=1.0 && cellA/nx!=cellB/ny) painter->scale(1.0,pixelSize);
  if(pixelSize!=1.0) painter->scale(1.0,pixelSize);
  
  painter->setClipRect(QRect(-pixmap->width()/2.0,-pixmap->height()/2.0,pixmap->width(),pixmap->height()));  
  painter->drawPixmap(QPointF(-pixmap->width()/2.0,-pixmap->height()/2.0),*pixmap);
  painter->setPen(Qt::green);
  painter->drawPoint(pOrigin);
  
  painter->resetTransform();
  painter->translate(QPointF(float(width())/2.0,float(height())/2.0));  

  double r=400;
  float thetaD = data->get("taxa","value").toFloat();
  float tangl  = data->get("tangl","value").toFloat();
  float realang = data->get("realang","value").toFloat();
  float recipang = 180.0 - realang;
  bool revhk = data->get("revhk")->toBool();
  bool rot90 = data->get("rot90")->toBool();
  bool rot180 = data->get("rot180")->toBool();
  bool sgnxch = data->get("sgnxch")->toBool();
  bool revhnd = data->get("revhnd")->toBool();
  bool revxsgn = data->get("revxsgn")->toBool();

  while(thetaD>90.0) thetaD -= 180.0;
  while(thetaD<=-90.0) thetaD += 180.0;

  if(revhk) thetaD = recipang-thetaD;
  if(rot90) thetaD -= recipang;
  if(sgnxch) thetaD = -thetaD;
  if(revxsgn) thetaD = -thetaD;

  while(thetaD>90.0) thetaD -= 180.0;
  while(thetaD<=-90.0) thetaD += 180.0;

  // if(invertAngle) thetaD*=-1;
  // float theta = -(thetaD)/180.0*PI;
  
  // TAXA is defined as the angle between the Tilt Axis (in the final map) and the A Axis (the horizontal X-axis in the final map).
  // TAXA is measuring counterclock wise. A value of 45 degrees for TAXA would mean a line from 10:30am to 4:40 pm.
  if(abs(tangl)>=1.0)
  { painter->rotate(thetaD);
    QPen pen = QPen(Qt::black);
    pen.setColor(QColor(100,204,210));
    pen.setWidth(2);
    painter->setPen(pen);  
    painter->drawLine(QPointF(-r,-1.0),QPointF(r,-1.0));
  }
  delete painter;  
}

void albumViewer::loadCellParameters()
{
  if(data==NULL) return;

  nx = (float)((int*)(imageHeader->rawData()))[0];
  ny = (float)((int*)(imageHeader->rawData()))[1];
  cellA = ((float*)(imageHeader->rawData()))[10];
  cellB = ((float*)(imageHeader->rawData()))[11];
  cellC = ((float*)(imageHeader->rawData()))[15];
  mode = imageHeader->mode();
}

void albumViewer::loadConfParameters()
{
  phaseOrigin = QPointF (data->get("phaori","value").section(',',0,0).toFloat(),data->get("phaori","value").section(',',-1,-1).toFloat());
}

void albumViewer::mousePressEvent(QMouseEvent */*event*/)
{
  
}

