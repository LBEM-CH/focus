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


#include "textBrowser.h"
#include <iostream>
using namespace std;

textBrowser::textBrowser(confData *conf, QWidget *parent)
            :QTextBrowser(parent)
{
  data = conf;
  connect(this,SIGNAL(anchorClicked(const QUrl &)),this,SLOT(linkClicked(const QUrl &)));
}

void textBrowser::setSource(const QUrl&)
{

}

void textBrowser::setLocalSource(const QUrl &source)
{
  QTextBrowser::setSource(source);
}

void textBrowser::linkClicked(const QUrl &link)
{
  QProcess::startDetached(data->getApp("webBrowser") + " " +  link.toString());
}
