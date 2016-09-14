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

#include "ApplicationData.h"
#include "ProjectDelegate.h"

ProjectDelegate::ProjectDelegate(QObject *parent)
: QItemDelegate(parent) {
    checkIcon = ApplicationData::icon("lock");
}

void ProjectDelegate::drawCheck(QPainter *painter, const QStyleOptionViewItem &/*option*/, const QRect &rect, Qt::CheckState state) const {
    if (state == Qt::Checked || state == Qt::PartiallyChecked)
        checkIcon.paint(painter, rect, Qt::AlignCenter, QIcon::Normal, QIcon::On);
    else
        checkIcon.paint(painter, rect, Qt::AlignCenter, QIcon::Normal, QIcon::Off);
}

