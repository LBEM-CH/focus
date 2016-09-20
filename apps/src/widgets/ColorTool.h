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

#ifndef COLORTOOL_H
#define COLORTOOL_H

#include <QDialog>
#include <QLabel>
#include <QAction>
#include <QLineEdit>
#include <QSlider>
#include <QDoubleSpinBox>
#include <QGridLayout>
#include <QImage>
#include <QPixmap>
#include <QPushButton>
#include <float.h>

#include "FullScreenImage.h"
#include "mrcHeader.h"

class ColorTool : public QDialog {
    Q_OBJECT

public:
    ColorTool(FullScreenImage *sourceImage, mrcHeader *header, QWidget *parent = NULL);

public slots:
    void setBrightness(int brightness);
    void setContrast(int contrast);
    void complete();
    void completeMax();
    void completeMin();
    void rescale();
    void invert();
    void showPhase(bool show);

    //  void setPhase(float theta);

signals:
    void togglePhase(bool show);
    void toggleInvert(bool invert);

protected:
    void showEvent(QShowEvent *event);
    void focusInEvent(QFocusEvent *event);

private:
    bool rescaledBrightness, rescaledContrast;
    FullScreenImage *image;
    mrcHeader *imageHeader;
    QLineEdit *brightnessValue;
    QLineEdit *contrastValue;
    QSlider *brightnessSlider;
    QSlider *contrastSlider;
    QDoubleSpinBox *minSpinBox, *maxSpinBox;

    QPushButton *showPhasesButton;

    bool invertImage;
};

#endif

