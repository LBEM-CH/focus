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
#include <QFileInfo>
#include <QDateTime>

#include "FileInfoDisplay.h"

FileInfoDisplay::FileInfoDisplay(QWidget *parent)
: QWidget(parent) {
    setAutoFillBackground(true);
    QPalette pal(palette());
    pal.setColor(QPalette::Background, QColor(255, 255, 255));
    setPalette(pal);
    QGridLayout *layout = new QGridLayout(this);
    layout->setMargin(1);
    layout->setSpacing(0);
    layout->setAlignment(Qt::AlignTop | Qt::AlignJustify);
    layout->setColumnStretch(0, 2);
    layout->setColumnStretch(1, 1);
    layout->setColumnStretch(2, 1);
    layout->setColumnStretch(3, 1);

    for (int i = 0; i < 4; i++) {
        labels << new QLabel();
        if (i > 0) {
            labels[i]->setFont(font());
            labels[i]->setWordWrap(true);
            labels[i]->setAlignment(Qt::AlignCenter | Qt::AlignRight);
        }
    }

    bool color = true;
    
    QFont font = labels[0]->font();
    font.setBold(true);
    font.setPointSize(16);
    labels[0]->setFont(font);
    labels[0]->setAlignment(Qt::AlignCenter);
    layout->addWidget(labels[0], 0, 0, 1, 2);
    
    layout->addWidget(labels[1], 1, 0, 1, 2);
    setColors(1, 1, color);
    color = color ^ true;
    
    layout->addWidget(titleLabel("Created:", color), 2, 0, 1, 1);
    layout->addWidget(labels[2], 2, 1, 1, 1);
    setColors(2, 2, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Size:", color), 3, 0, 1, 1);
    layout->addWidget(labels[3], 3, 1, 1, 1);
    setColors(3, 3, color);
    color = color ^ true;

    layout->setRowStretch(0, 1);
    for (int i = 1; i < 4; i++)
        layout->setRowStretch(i, 0);

    setLayout(layout);
}

void FileInfoDisplay::setColors(int start, int end, bool color) {
    QPalette pal(palette());

    if (color) pal.setColor(QPalette::Background, QColor(237, 243, 254));
    else pal.setColor(QPalette::Background, QColor(255, 255, 255));

    for (int i = start; i <= end; i++) {
        labels[i]->setAutoFillBackground(true);
        labels[i]->setPalette(pal);
    }
}

QLabel *FileInfoDisplay::titleLabel(const QString &text, bool color) {
    QPalette pal(palette());
    if (color) pal.setColor(QPalette::Background, QColor(237, 243, 254));
    else pal.setColor(QPalette::Background, QColor(255, 255, 255));

    QLabel *l = new QLabel(text);
    l->setAutoFillBackground(true);
    l->setPalette(pal);
    l->setFont(font());
    l->setMaximumWidth(width());
    return l;
}

void FileInfoDisplay::setFile(const QString &result) {
    QFileInfo inf(result);
    QString resultTitle = inf.fileName();

    labels[0]->setText(inf.suffix().toUpper() + " File");
    labels[1]->setText(resultTitle);
    labels[2]->setText(inf.created().toString("dd.MM.yyyy hh:mm"));
    labels[3]->setText(QString::number(inf.size()) + " bytes");
}

