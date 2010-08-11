/*
 *  graphicalButton.cpp
 *  2DX
 *
 *  Created by Bryant Gipson on 3/14/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "graphicalButton.h"
#include <iostream>
using namespace std;

void graphicalButton::initialize()
{
  if(ico!=NULL && !ico->isNull())
  {
    setFixedSize(ico->actualSize(size()*2));
    connect(this,SIGNAL(toggled(bool)),this,SLOT(changeState(bool)));
  }
  else
    cerr<<"Invalid icon."<<endl;
}

graphicalButton::graphicalButton(QWidget *parent)
                :QAbstractButton(parent)
{
  ico = NULL;
  initialize();
}

graphicalButton::graphicalButton(QIcon *icon, QWidget *parent)
                :QAbstractButton(parent)
{
  ico = icon;
  initialize();
}

void graphicalButton::paintEvent(QPaintEvent *event)
{
  QWidget::paintEvent(event);
  QPainter imageBase(this);
  QSize iSize = size();
  if(ico!=NULL)
  {
    if(!isChecked())
      if(!isDown())
        imageBase.drawPixmap(0,0,ico->pixmap(iSize));
      else
        imageBase.drawPixmap(0,0,ico->pixmap(iSize,QIcon::Active));
    else
      if(!isDown())
        imageBase.drawPixmap(0,0,ico->pixmap(iSize,QIcon::Normal,QIcon::On));
      else
        imageBase.drawPixmap(0,0,ico->pixmap(iSize,QIcon::Active,QIcon::On));
  }
}

void graphicalButton::checkStateSet()
{

}

void graphicalButton::changeState(bool checked)
{
  if(checked) emit stateChanged(Qt::Checked);
  else emit stateChanged(Qt::Unchecked);
}

void graphicalButton::setCheckable(bool checkable)
{
  QAbstractButton::setCheckable(checkable);
}

