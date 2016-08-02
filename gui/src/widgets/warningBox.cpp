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

#include "warningBox.h"

warningBox::warningBox(resultsFile *file, QWidget *parent)
           :QFrame(parent)
{
  results = file;
  warning = 0;

  if(file!=NULL)
    connect(results,SIGNAL(loaded()),this,SLOT(load()));

  timer.setSingleShot(true);
  connect(&timer,SIGNAL(timeout()),this,SLOT(load()));

  setFrameStyle(QFrame::Box | QFrame::Raised);
  setLineWidth(1);

    QHBoxLayout *layout = new QHBoxLayout(this);
    layout->setMargin(1);
    layout->setSpacing(0);
    setLayout(layout);

    setAutoFillBackground(true);

    QPalette pal(palette());
    pal.setColor(QPalette::Window,Qt::white);
    setPalette(pal);

    labelFont = font();
    labelFont.setPointSize(labelFont.pointSize()-2);
    setFont(labelFont);

    setSizePolicy(QSizePolicy::Minimum,QSizePolicy::Minimum);

    label = new QLabel(this);
    label->setAlignment(Qt::AlignLeft);
    label->setAutoFillBackground(true);
//    pal.setColor(QPalette::WindowText,QColor(201,24,24));
    pal.setColor(QPalette::Window,QColor(255,255,220));
    label->setPalette(pal);
    label->setFont(labelFont);
    layout->addWidget(label);
    hide();
}

void warningBox::load()
{
  timer.stop();
  if(results == NULL) return;
  QStringList warnings = results->warningList();
  if(!warnings.empty())
  {
    if(warning>warnings.size()) warning = 0;
    label->setText(QString::number(warning+1) + "/" + QString::number(warnings.size()) + ": " + warnings[warning%warnings.size()]);
    warning = (warning+1)%warnings.size();
    if(warnings.size()>1) timer.start(5000);
    if(isHidden()) show();
    update();
  }
  else
    hide();
}

void warningBox::load(const QString &fileName)
{
  if(results != NULL) delete results;
  results = new resultsFile(fileName);
  load();
}
