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

#include <MrcHeaderDisplay.h>

MrcHeaderDisplay::MrcHeaderDisplay(QWidget *parent)
: QWidget(parent) {
    setAutoFillBackground(true);
    QPalette pal(palette());
    pal.setColor(QPalette::Background, QColor(255, 255, 255));
    setPalette(pal);
    setFont(QFont("Courier", 10));
    QGridLayout *layout = new QGridLayout(this);
    layout->setMargin(1);
    layout->setSpacing(0);
    layout->setAlignment(Qt::AlignTop | Qt::AlignJustify);
    layout->setColumnStretch(0, 2);
    layout->setColumnStretch(1, 1);
    layout->setColumnStretch(2, 1);
    layout->setColumnStretch(3, 1);

    for (int i = 0; i < 30; i++) {
        labels << new QLabel();
        if (i > 0) {
            labels[i]->setFont(font());
            labels[i]->setAlignment(Qt::AlignCenter | Qt::AlignRight);
        }
    }

    bool color = true;

    QPalette titlePal(palette());
    QLinearGradient grad(QPoint(0, 0), QPoint(0, 20));
    grad.setColorAt(1, QColor(31, 92, 207));
    grad.setColorAt(0.5, QColor(33, 126, 220));
    grad.setColorAt(0, QColor(88, 153, 229));
    titlePal.setBrush(QPalette::Background, grad);
    titlePal.setColor(QPalette::WindowText, QColor(247, 245, 250));
    labels[0]->setAutoFillBackground(true);
    labels[0]->setPalette(titlePal);
    labels[0]->setFont(QFont("Courier", 11));
    labels[0]->setAlignment(Qt::AlignCenter);

    layout->addWidget(labels[0], 0, 0, 1, 4);
    layout->addWidget(titleLabel("NX, NY, NZ:", color), 1, 0, 1, 1);
    layout->addWidget(labels[1], 1, 1, 1, 1);
    layout->addWidget(labels[2], 1, 2, 1, 1);
    layout->addWidget(labels[3], 1, 3, 1, 1);
    setColors(1, 3, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Mode:", color), 2, 0, 1, 1);
    layout->addWidget(labels[4], 2, 1, 1, 3);
    setColors(4, 4, color);
    color = color ^ true;


    layout->addWidget(titleLabel("Origin x,y,z:", color), 3, 0, 1, 1);
    layout->addWidget(labels[5], 3, 1, 1, 1);
    layout->addWidget(labels[6], 3, 2, 1, 1);
    layout->addWidget(labels[7], 3, 3, 1, 1);
    setColors(5, 7, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Sample x,y,z:", color), 4, 0, 1, 1);
    layout->addWidget(labels[8], 4, 1, 1, 1);
    layout->addWidget(labels[9], 4, 2, 1, 1);
    layout->addWidget(labels[10], 4, 3, 1, 1);
    setColors(8, 10, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Cell Axes:", color), 5, 0, 1, 1);
    layout->addWidget(labels[11], 5, 1, 1, 1);
    layout->addWidget(labels[12], 5, 2, 1, 1);
    layout->addWidget(labels[13], 5, 3, 1, 1);
    setColors(11, 13, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Cell Angles:", color), 6, 0, 1, 1);
    layout->addWidget(labels[14], 6, 1, 1, 1);
    layout->addWidget(labels[15], 6, 2, 1, 1);
    layout->addWidget(labels[16], 6, 3, 1, 1);
    setColors(14, 16, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Basis Vectors:", color), 7, 0, 1, 1);
    layout->addWidget(labels[17], 7, 1, 1, 1);
    layout->addWidget(labels[18], 7, 2, 1, 1);
    layout->addWidget(labels[19], 7, 3, 1, 1);
    setColors(17, 19, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Min, Max, Mean:", color), 8, 0, 1, 1);
    layout->addWidget(labels[20], 8, 1, 1, 1);
    layout->addWidget(labels[21], 8, 2, 1, 1);
    layout->addWidget(labels[22], 8, 3, 1, 1);
    setColors(20, 22, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Space Group:", color), 9, 0, 1, 1);
    layout->addWidget(labels[23], 9, 1, 1, 3);
    setColors(23, 23, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Symmetry Bytes:", color), 10, 0, 1, 1);
    layout->addWidget(labels[24], 10, 1, 1, 3);
    setColors(24, 24, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Phase Origin:", color), 11, 0, 1, 1);
    layout->addWidget(labels[25], 11, 1, 1, 1);
    layout->addWidget(labels[26], 11, 2, 1, 1);
    layout->addWidget(labels[27], 11, 3, 1, 1);
    setColors(25, 27, color);
    color = color ^ true;

    layout->addWidget(titleLabel("RMS:", color), 12, 0, 1, 1);
    layout->addWidget(labels[28], 12, 1, 1, 3);
    setColors(28, 28, color);
    color = color ^ true;

    layout->addWidget(titleLabel("Labels:", color), 13, 0, 1, 1);
    layout->addWidget(labels[29], 13, 1, 1, 3);
    setColors(29, 29, color);

    for (int i = 0; i < 15; i++)
        layout->setRowStretch(i, 1);

    setLayout(layout);
}

void MrcHeaderDisplay::setColors(int start, int end, bool color) {
    QPalette pal(palette());

    if (color) pal.setColor(QPalette::Background, QColor(237, 243, 254));
    else pal.setColor(QPalette::Background, QColor(255, 255, 255));

    for (int i = start; i <= end; i++) {
        labels[i]->setAutoFillBackground(true);
        labels[i]->setPalette(pal);
    }
}

QLabel *MrcHeaderDisplay::titleLabel(const QString &text, bool color) {
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

void MrcHeaderDisplay::setHeader(const QString &result, mrcHeader header) {
    int *rawHeader = header.rawData();
    QString resultTitle = QFileInfo(result).fileName();

    QFontMetrics m(labels[0]->font());

    labels[0]->setText("<b><font color=\"#F7F5FA\">" + m.elidedText(resultTitle, Qt::ElideLeft, contentsRect().width()) + "</font></b>");
    if (rawHeader[3] > 2) labels[1]->setText(QString().setNum((rawHeader[0] - 1) << 1));
    else labels[1]->setText(QString().setNum(rawHeader[0]));
    for (int i = 1; i < 10; i++)
        labels[i + 1]->setText(QString().setNum(rawHeader[i]));

    for (int i = 10; i < 16; i++)
        labels[i + 1]->setText(QString().setNum(((float*) rawHeader)[i]));
    for (int i = 16; i < 19; i++) {
        if (rawHeader[i] == 1)
            labels[i + 1]->setText("x");
        else if (rawHeader[i] == 2)
            labels[i + 1]->setText("y");
        else
            labels[i + 1]->setText("z");
    }
    for (int i = 19; i < 22; i++)
        labels[i + 1]->setText(QString().setNum(((float*) rawHeader)[i], 'f', 1));
    for (int i = 22; i < 24; i++)
        labels[i + 1]->setText(QString().setNum(rawHeader[i]));
    for (int i = 24; i < 27; i++)
        labels[i + 1]->setText(QString().setNum(((float*) rawHeader)[i + 25], 'f', 1));

    labels[28]->setText(QString().setNum(((float*) rawHeader)[54]));
    labels[29]->setText(QString().setNum(rawHeader[55]));
}

