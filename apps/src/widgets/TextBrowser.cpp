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
#include <QDesktopServices>

#include "TextBrowser.h"

TextBrowser::TextBrowser(QWidget *parent)
: QTextBrowser(parent) {
    connect(this, SIGNAL(anchorClicked(const QUrl &)), this, SLOT(linkClicked(const QUrl &)));
}

void TextBrowser::setSource(const QUrl&) {}

void TextBrowser::setLocalSource(const QUrl &source) {
    QTextBrowser::setSource(source);
}

void TextBrowser::linkClicked(const QUrl &link) {
    QDesktopServices::openUrl(link);
}
