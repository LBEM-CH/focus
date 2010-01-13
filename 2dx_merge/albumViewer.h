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

#ifndef IMAGEVIEWER_H
#define IMAGEVIEWER_H

#include <QWidget>
#include <QPainter>
#include <mrcImage.h>
#include <confData.h>
#include <math.h>

class albumViewer : public QWidget
{
  Q_OBJECT

  public slots:
  	void setImage(const QString &fileName);
    void setConf(const QString &confName);
    void loadCellParameters();
    void loadConfParameters();

  private:
    confData *data;
	  mrcImage *image;
	  QPixmap *pixmap;
	  QString imageName;
	  QString basePath;

    /* Cell Parameters */
    float cellA, cellB, cellC, nx, ny;

    /* Display Parameters */
    float pixelSize, shearScale;
    int mode;

    mrcHeader *imageHeader;

    /* Interface */
    QPointF phaseOrigin;

  public:
    albumViewer(QWidget *parent = NULL);
  	void setConf(confData *conf);
	  void guessConfFromImage();
	
  protected:
    void paintEvent(QPaintEvent *event);
    void mousePressEvent(QMouseEvent *event);
	
};

#endif

