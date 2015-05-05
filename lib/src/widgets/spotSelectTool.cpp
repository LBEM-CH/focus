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


#include "spotSelectTool.h"
#include <iostream>
using namespace std;

#define PI 3.14159265

spotSelectTool::spotSelectTool(confData *conf, fullScreenImage *fsImage, mrcImage *originalImage, const QPoint &, QWidget *parent)
                              :QWidget(parent,Qt::Tool)
{
  data = conf;
  image = fsImage;
  imageData = originalImage;
  imageHeader = imageData->getHeader();
  screenWidth = imageHeader->nx();
  screenHeight = imageHeader->ny();
  QStringList cell=data->get("lattice","value").split(',');

  QFont highlight(font());
  highlight.setBold(true);
   

  float lattice[2][2];

  lattice[0][0]=cell[0].toFloat();
  lattice[0][1]=cell[1].toFloat();

  lattice[1][0]=cell[2].toFloat();
  lattice[1][1]=cell[3].toFloat();

  float det = lattice[0][0]*lattice[1][1]-lattice[0][1]*lattice[1][0];

  if(det!=0)
  {
    inv[0][0] = lattice[1][1]/det;
    inv[0][1] = lattice[0][0]/det;
    inv[1][0] = -lattice[0][1]/det;
    inv[1][1] = -lattice[1][0]/det;
  }

  QVBoxLayout *globalLayout = new QVBoxLayout(this);
  setLayout(globalLayout);

  QHBoxLayout *layoutLabel = new QHBoxLayout;
  QLabel *fileName = new QLabel(QFileInfo(originalImage->getFileName()).fileName());
  fileName->setFont(highlight);
  layoutLabel->addWidget(fileName);
  QPalette pal(palette());
  pal.setColor(QPalette::Foreground, Qt::blue);
  fileName->setPalette(pal);


  QGridLayout *layout = new QGridLayout;
  layout->setSpacing(4);
  layout->setMargin(2);


  globalLayout->addLayout(layoutLabel);
  globalLayout->addLayout(layout);

  QLabel *index = new QLabel("Miller Index: ");
  i = new QLabel("--");
  j = new QLabel("--");
  mouseX = new QLabel("--");
  mouseY = new QLabel("--");
  resolution = new QLabel("--");
  value = new QLabel("--");
  phase = new QLabel("--");
  int k = 0;
  if(imageHeader->isFFT())
  {
    layout->addWidget(index,k,0,1,1);
    layout->addWidget(i,k,1,1,1);
    layout->addWidget(j,k,2,1,1);
    k++;
  }

  layout->addWidget(new QLabel("Mouse Coordinates: "),k,0,1,1);
  layout->addWidget(mouseX,k,1,1,1);
  layout->addWidget(mouseY,k,2,1,1);
  k++;

  if(imageHeader->isFFT())
  {
    layout->addWidget(new QLabel("Resolution [A]: "),k,0,1,1);
    layout->addWidget(resolution,k,1,1,1);
    k++;

    layout->addWidget(new QLabel("Amplitude: "),k,0,1,1);
    layout->addWidget(value,k,1,1,1);
    k++;

    layout->addWidget(new QLabel("Phase [deg]: "),k,0,1,1);
    layout->addWidget(phase,k,1,1,1);
    k++;
  }
  else
  {
    layout->addWidget(new QLabel("Pixel Value: "),k,0,1,1);
    layout->addWidget(value,k,1,1,1);
    k++;
  }

  if(!imageHeader->isFFT())
  {
    QStringList cell=data->get("refori","value").split(',');
    refOriginX = new QLabel(cell[0],this);
    refOriginY = new QLabel(cell[1],this);
    layout->addWidget(new QLabel("Reference Origin: "),k,0,1,1);
    layout->addWidget(refOriginX,k,1,1,1);
    layout->addWidget(refOriginY,k,2,1,1);
    k++;
  }

}

void pMatrix(QMatrix m)
{
  cout<<m.m11()<<" "<<m.m12()<<endl;
  cout<<m.m21()<<" "<<m.m22()<<endl;
  cout<<"---"<<endl;
  cout<<m.det()<<endl;
  cout<<endl;
  cout<<" ########################## "<<endl;
}

void spotSelectTool::updateIndices(const QPoint &pos)
{
  currentPos = pos;
  int mX = pos.x();
  int mY = pos.y();
  float xScale = imageHeader->mx()/imageHeader->cellA();
  float yScale = imageHeader->my()/imageHeader->cellB();

  if(imageHeader->isFFT())
  {
    QStringList cell=data->get("lattice","value").split(',');
    float lattice[2][2];

    lattice[0][0]=cell[0].toFloat();
    lattice[1][0]=cell[1].toFloat();

    lattice[0][1]=cell[2].toFloat();
    lattice[1][1]=cell[3].toFloat();

    float det = lattice[0][0]*lattice[1][1]-lattice[0][1]*lattice[1][0];

    if(det!=0)
    {
      inv[0][0] = lattice[1][1]/det;
      inv[1][1] = lattice[0][0]/det;
      inv[0][1] = -lattice[0][1]/det;
      inv[1][0] = -lattice[1][0]/det;
    }

    float x = ((float)(pos.x()) * inv[0][0] + (float)(pos.y()) * inv[0][1]);
    float y = ((float)(pos.x()) * inv[1][0] + (float)(pos.y()) * inv[1][1]);
    if(x<0.0) x-=0.5; else x+=0.5;
    if(y<0.0) y-=0.5; else y+=0.5;

    i->setText(QString::number(int(x)));
    j->setText(QString::number(int(y)));
    resolution->setText(QString::number(((imageHeader->ny())*data->get("stepdigitizer","value").toFloat()*1e4/data->get("magnification","value").toFloat())/(sqrt(float(xScale*xScale*mX*mX+yScale*yScale*mY*mY)))));
    mouseX->setText(QString::number(mX));
    mouseY->setText(QString::number(mY));
    QPoint p = pos;
    float phaseValue;
    if(p.x()<0)
    {
      p*=-1;
      phaseValue = 2.0*PI - imageData->phase(QPoint(p.x(),p.y()+screenHeight/2));
    }
    else
      phaseValue = imageData->phase(QPoint(p.x(),p.y()+screenHeight/2));

    value->setText(QString::number(imageData->value(QPoint(p.x(),p.y()+screenHeight/2))));
    phase->setText(QString::number(phaseValue/(2.0*PI)*360.0));
    emit phaseChanged(phaseValue);
  }
  else
  {
    float xPos = pos.x() + screenWidth/2;
    float yPos = (pos.y() + (screenHeight)/2);
    float scale = image->imageScale();
		float cellC = ((float*)(imageHeader->rawData()))[15];
    int mode = imageHeader->mode();

    QMatrix m = imageData->matrix();
    // qDebug()<<m.m11()<<" "<<m.m22()<<endl;

    if(mode < 3)
    {
      m = m.inverted();
      QPointF tP = QPointF(pos.x()*m.m11() - pos.y()*m.m21(), -(pos.x()*m.m12() - pos.y()*m.m22() ))*scale + QPointF(screenWidth/2, screenHeight/2);
      xPos = (int)tP.x(); yPos = (int)tP.y();
    }

    if(xPos<0) xPos = 0; if(xPos>screenWidth - 1) xPos = screenWidth - 1;
    if(yPos<0) yPos = 0; if(yPos>screenHeight - 1) yPos = screenHeight - 1;

    value->setText(QString::number(imageData->value(QPoint(xPos,yPos))));

    if(cellC!=0 && cellC!=90 && mode < 3)
		{
//			xPos = (xPos/(float)image->width()*2-1)*360;
//			yPos = (yPos/(float)image->height()*2-1)*360;
		}


    mouseX->setText(QString::number(xPos+1));
    mouseY->setText(QString::number(yPos+1));
  }
}

void spotSelectTool::updateReferenceOrigin()
{
  QStringList cell=data->get("refori","value").split(',');
  refOriginX->setText(cell[0]);
  refOriginY->setText(cell[1]);
}

void spotSelectTool::updateIndices(float imageScale)
{
  updateIndices(currentPos/imageScale);
}

QPointF spotSelectTool::getImageCoordinates()
{
  return QPointF(mouseX->text().toFloat(),mouseY->text().toFloat());
}

