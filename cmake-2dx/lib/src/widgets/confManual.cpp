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

#include "confManual.h"
#include <QApplication>
#include <iostream>
using namespace std;

int min(int a, int b)
{
  if(a<b) return b;
  else return a;
}

confManual::confManual(confData *conf, confData *localConf, QWidget *parent)
           :QWidget(parent)
{
  data = localConf;
  globalData = conf;

  QVBoxLayout *layout = new QVBoxLayout(this);
  layout->setMargin(0);
  layout->setSpacing(0);
  layout->setAlignment(Qt::AlignTop);

  QLabel *title = new QLabel("Manual",this);
  title->setAutoFillBackground(true);
  QPalette titlePal(palette());
  QLinearGradient grad(QPoint(0,0),QPoint(0,title->height()));
  grad.setColorAt(1,QColor(207,92,31));
  grad.setColorAt(0.5,QColor(220,126,33));
  grad.setColorAt(0,QColor(229,153,88));;

  titlePal.setBrush(QPalette::Background,grad);
  titlePal.setColor(QPalette::WindowText,QColor(247,245,250));

  title->setPalette(titlePal);
  title->setFixedHeight(20);
  title->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
  QFont titleFont(QApplication::font());
  titleFont.setPointSize(titleFont.pointSize()-1);
  titleFont.setWeight(QFont::Bold);
  title->setFont(titleFont);
  layout->addWidget(title);

  text = new textBrowser(globalData,this);
  text->setReadOnly(true);
  QPalette pal(text->palette());
  pal.setColor(QPalette::Base,QColor(255,255,220));
  text->setPalette(pal);

  for(int i=0;i<data->manual().size();i++)
    text->insertHtml(data->manual()[i] + "<p>");
  text->moveCursor(QTextCursor::Start);
  layout->addWidget(text);

  setMinimumHeight(150);

  setLayout(layout);
}

