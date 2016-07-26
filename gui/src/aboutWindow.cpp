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

#include "aboutWindow.h"
#include <iostream>
using namespace std;

aboutWindow::aboutWindow(confData *conf, QWidget *parent, bool is_merge)
             :QWidget(parent,Qt::Window)
{

  QAction *hideWindow = new QAction(tr("Close Window"),this);
  hideWindow->setShortcut(tr("Ctrl+W"));
  addAction(hideWindow);
  connect(hideWindow,SIGNAL(triggered()),this,SLOT(hide()));

  if (is_merge)
  {
    setWindowTitle("About 2dx_merge");
  }
  else
  {
	setWindowTitle("About 2dx_image");  
  }

  data = conf;

  setFixedSize(QSize(568,410));

  QGridLayout *layout = new QGridLayout(this);
  setLayout(layout);


  if(data->getImage("appImage")!=NULL)
  {
    QLabel *appImage = new QLabel();
    appImage->setPixmap(QPixmap::fromImage(*(data->getImage("appImage"))));
    appImage->setAlignment(Qt::AlignTop);
    layout->addWidget(appImage,0,0,2,1);
  }
  else
    cerr<<"appImage not found"<<endl;

/*  aboutTitle = new QLabel("2dx_image");
  aboutTitle->setFont(QFont("Times",18,QFont::Bold));
  layout->addWidget(aboutTitle,0,1,1,1);
*/
  QPalette pal(palette());
  pal.setBrush(QPalette::Base,Qt::transparent);

  aboutText = new textBrowser(data,this);
  aboutText->setFont(QFont("Times",12));
  aboutText->viewport()->setAutoFillBackground(true);
  aboutText->viewport()->setPalette(pal);
  aboutText->setLocalSource(data->getDir("application") + "../about.htm");
  aboutText->setFrameShape(QFrame::NoFrame);

  layout->addWidget(aboutText,1,1,1,1);

}
