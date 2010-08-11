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

#include <albumDelegate.h>
#include <iostream>
using namespace std;

albumDelegate::albumDelegate(QObject *parent)
             :QItemDelegate(parent)
{
}

void albumDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
  QString file = index.data(Qt::UserRole+5).toString() + "/final_map.mrc";
      if(QFileInfo(file).exists())
      {
        mrcImage thumb(file,true);
        QPixmap map = thumb.getPixmap();
        QRect r(option.rect.topLeft()-QPoint(0,100),QSize(100,100));
        drawDecoration(painter,option, r, thumb.getPixmap().scaled(100,100));
        drawDisplay(painter,option,option.rect,index.data(Qt::DisplayRole).toString());
        
      }
}

