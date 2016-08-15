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

aboutWindow::aboutWindow(confData *conf, QWidget *parent)
: QWidget(parent, Qt::Window) {

    QAction *hideWindow = new QAction(tr("Close Window"), this);
    hideWindow->setShortcut(tr("Ctrl+W"));
    addAction(hideWindow);
    connect(hideWindow, SIGNAL(triggered()), this, SLOT(hide()));

    setWindowTitle("About 2DX");

    data = conf;

    setFixedSize(QSize(568, 410));

    QVBoxLayout *layout = new QVBoxLayout(this);
    
    QLabel* title = new QLabel("2DX Graphical User Interface");
    QFont font = title->font();
    font.setBold(true);
    font.setPointSize(18);
    title->setFont(font);
    
    QLabel* subTitle = new QLabel("2D Electron Crystallography Image Processing Suite");
    
    QLabel* version = new QLabel("Version: " + data->version_number() + " (revision: " + data->version_revision() + ")");
    QPalette pal = version->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    version->setPalette(pal);
    
    QVBoxLayout* headerLayout = new QVBoxLayout;
    headerLayout->addWidget(title);
    headerLayout->addWidget(subTitle);
    headerLayout->addWidget(version);
    headerLayout->addStretch(1);
    
    QHBoxLayout* topLayout = new QHBoxLayout;
    topLayout->addLayout(headerLayout);
    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    topLayout->addWidget(spacer);

    if (data->getImage("appImage") != NULL) {
        QLabel *appImage = new QLabel();
        appImage->setPixmap(QPixmap::fromImage(*(data->getImage("appImage"))).scaledToHeight(100));
        appImage->setAlignment(Qt::AlignTop);
        topLayout->addWidget(appImage);
    } else {
        cerr << "appImage not found" << endl;
    }
    
    layout->addLayout(topLayout, 0);
    
    
    QFrame* hLine = new QFrame(this);
    hLine->setFrameStyle(QFrame::HLine | QFrame::Sunken);
    layout->addWidget(hLine);

    aboutText = new textBrowser(data, this);
    //aboutText->setFont(QFont("Times", 12));
    aboutText->viewport()->setAutoFillBackground(true);
    aboutText->setLocalSource(data->getDir("application") + "../resources/about.htm");
    aboutText->setFrameShape(QFrame::NoFrame);
    
    QPalette palBrowser(palette());
    palBrowser.setBrush(QPalette::Base, Qt::transparent);
    aboutText->viewport()->setPalette(palBrowser);
    
    layout->addWidget(aboutText, 1);
    setLayout(layout);

}
