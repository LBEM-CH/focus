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

#include "SelectionFFT.h"
#include <QTime>
#include <fftw3.h>
#include <math.h>
#include <float.h>
#include <iostream>
using namespace std;

SelectionFFT::SelectionFFT(QWidget *parent)
             :QWidget(parent,Qt::Tool)
{
  QGridLayout *layout = new QGridLayout(this);
  layout->setMargin(0);
  layout->setSpacing(0);
  setLayout(layout);

  display = new QLabel(this);
  display->setAlignment(Qt::AlignCenter);
  layout->addWidget(display);

  setFixedSize(300,300);

  intensity = 5.0;
  scale = 1.0;
}

void SelectionFFT::fft(QWidget *widget, const QRect &view)
{
  QRect window = view;
  if(QString(qVersion()).startsWith("4.3"))
  {
    float x = round(view.topLeft().x()/2.0), y = round(view.topLeft().y()/2.0);
    window.moveTopLeft(QPoint(x,y));
  }
  image = widget->grab(window);
//  display->setPixmap(image);
//  setGeometry(view);
  setFixedSize(view.size());
  repaint();
  calculateFFT();
}

void SelectionFFT::calculateFFT()
{
  if(image.isNull()) return;
#ifdef DEBUG
  QTime timer;
  timer.start();
  int startTime = timer.elapsed();
#endif

  QImage tempImage = image.toImage();
#ifdef DEBUG
  cout<<"Image Conversion: "<<timer.elapsed()-startTime<<endl;
#endif
  int w = image.width(), h = image.height();
  float max = 0.0, min = FLT_MAX;
  fftwf_complex *fft = new fftwf_complex[w*h];

  for(int i=0;i<w*h;i++)
  {
    fft[i][0] = float(QColor(((uint*)tempImage.bits())[i]).blue());//*powf(-1,i%w+i/w);//QColor(tempImage.pixel(i%w,i/w)).blue()*powf(-1,i%w+i/w);
    if((i%w+i/w)%2) fft[i][0]*=-1;
    fft[i][1] = 0.0;
  }

#ifdef DEBUG
  cout<<"Load time: "<<timer.elapsed()-startTime<<endl;
  int startFFT = timer.elapsed();
#endif
  fftwf_plan p = fftwf_plan_dft_2d(w, h, (fftwf_complex*)fft, (fftwf_complex*)fft, FFTW_FORWARD, FFTW_ESTIMATE);
  fftwf_execute(p);
  fftwf_destroy_plan(p);
#ifdef DEBUGsetColorCount
  int finishFFT = timer.elapsed();
  cout<<"FFT Time: "<<finishFFT-startFFT<<endl;
  int startLoad = timer.elapsed();
#endif
  float *imageFinal = new float[w*h];
  float norm=sqrt(float(w*h));
  for(int i=0;i<w*h;i++)
  {
    imageFinal[i] = (sqrt(fft[i][0]*fft[i][0]+fft[i][1]*fft[i][1]))/(norm);
    if(imageFinal[i]>max) max = imageFinal[i];
    if(imageFinal[i]<min) min = imageFinal[i];
  }

  uchar *normalizedFinal = new uchar[w*h];
  float c;
  for(int i=0;i<w*h;i++)
  {
    c = (imageFinal[i]-min)/((max-min)/float(pow(2,(int)intensity+3)))*255.0;//(100.0+(intensity-5.0)/8.0*199.0))*255.0;
    if(c>255.0) c = 255.0;
    if(c<0.0) c = 0.0;
    normalizedFinal[i] = uchar(c);
  }

  tempImage = QImage(normalizedFinal,w,h,QImage::Format_Indexed8);
  tempImage.setColorCount(256);
  for(int i=0;i<256;i++)
    tempImage.setColor(i,QColor(i,i,i).rgb());

  if(scale == 1.0)
    display->setPixmap(QPixmap::fromImage(tempImage));
  else
    display->setPixmap(QPixmap::fromImage(tempImage).scaledToHeight(int(scale*tempImage.height())));

  delete[] fft; delete[] imageFinal; delete[] normalizedFinal;
#ifdef DEBUG
  cout<<"Cleanup Time: "<<timer.elapsed()-startLoad<<endl;
  cout<<"Miliseconds :"<<timer.elapsed()<<endl;
#endif
}

void SelectionFFT::setBrightness(float brightness)
{
  intensity = brightness;
  calculateFFT();
}

void SelectionFFT::setZoom(float zoom)
{
  scale = zoom;
  calculateFFT();
}

void SelectionFFT::increaseZoom()
{
  scale*=1.2;
  calculateFFT();
}

void SelectionFFT::decreaseZoom()
{
  scale/=1.2;
  calculateFFT();
}

void SelectionFFT::zoomStandard()
{
  scale = 1.0;
  calculateFFT();
}
