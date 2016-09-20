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

#include "MouseAssignTool.h"

MouseAssignTool::MouseAssignTool(QWidget *parent) : QWidget(parent, Qt::Tool) {
    QList<QLabel *> labels;
    QGridLayout *layout = new QGridLayout(this);
    layout->setAlignment(Qt::AlignCenter);
    labels << new QLabel("Mouse: ") << new QLabel("Left") << new QLabel("Middle") << new QLabel("Right") << new QLabel("Trackpad (Portables): ") << new QLabel("Click")
            << new QLabel("Alt + Click") << new QLabel("Ctrl + Click") << new QLabel("Function: ");

    for (int i = 0; i < labels.size(); i++) {
        layout->addWidget(labels[i], i / 4, i % 4, 1, 1);
        if (i % 4) labels[i]->setAlignment(Qt::AlignCenter);
    }

    QLabel *button;
    for (int i = 0; i < 3; i++) {
        button = new QLabel("--");
        buttons << button;
        layout->addWidget(button, 2, i + 1, 1, 1);
        button->setAlignment(Qt::AlignCenter);
    }

    setLayout(layout);
}

void MouseAssignTool::assignButton(int button, const QString &text) {
    if (buttons[button] != NULL)
        buttons[button]->setText(text);
}
