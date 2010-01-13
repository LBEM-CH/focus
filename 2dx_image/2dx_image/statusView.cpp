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

#include "statusView.h"
#include <iostream>
using namespace std;

statusView::statusView(confData *status, QWidget *parent)
                      :QWidget(parent)
{
  int minWidth = int(QApplication::desktop()->width()/5.00);
  if(minWidth > 235) minWidth = 235;
  setMinimumWidth(minWidth);
  setFixedHeight(minWidth);
  statusData = status;
  QGridLayout *layout = initializeLayout(this);

  QLabel *item;

  QWidget *separator = new QWidget;
  separator->setFixedHeight(2);
  separator->setMinimumWidth(minWidth);
  separator->setAutoFillBackground(true);

  QLinearGradient grad(QPoint(0,0),QPoint(0,2));
  grad.setColorAt(1,QColor(113,113,114));
  grad.setColorAt(0,QColor(172,172,171));

  QPalette pal(palette());
  pal.setBrush(QPalette::Window,grad);
  pal.setBrush(QPalette::Base,grad);

  separator->setPalette(pal);

  QString qMagS2 = statusData->get("QVal","value"), qMagS1 = statusData->get("CalculatedMag","value");
  qMagS1.truncate(qMagS1.indexOf('.')+2);  qMagS2.truncate(qMagS2.indexOf('.')+2);
  qMag<<new QLabel(qMagS1.trimmed())<<new QLabel(qMagS2.trimmed());
  qMag[0]->setAlignment(Qt::AlignCenter | Qt::AlignVCenter);
  qMag[1]->setAlignment(Qt::AlignRight | Qt::AlignVCenter);


  QLabel *QValHeader = new QLabel( "QVal" );
  QValHeader->setAlignment(Qt::AlignCenter | Qt::AlignVCenter);

  layout->addWidget(container(QList<QLabel *>()<<container(QList<QLabel *>() << new QLabel("Th.Mag") << qMag[0])<<
					container(QList<QLabel *>() << QValHeader << qMag[1])),0,0,1,1);
  layout->addWidget(separator,1,0,1,1);

  qVal<< new QLabel << new QLabel << new QLabel;

  for(unsigned int j = 0; j < 2; j++)
    for(unsigned int i = 0; i < 9; i++)
    {
      if(j==0) IQ << new QLabel(statusData->get("U" + QString::number(j+1) + "_IQ" + QString::number(i+1),"value"));
	    else IQ2 << new QLabel(statusData->get("U" + QString::number(j+1) + "_IQ" + QString::number(i+1),"value"));
    }

  for(quint32 i = 0; i<9; i++)
    IQS<<new QLabel(statusData->get("US_IQ" + QString::number(i+1),"value"));

  QString qVal1S = "QVal1", qVal2S = "QVal2", qValSS = "QValS";
  QList<QLabel *> qVal_L;
  qVal_L<<new QLabel(qVal1S)<<new QLabel(qVal2S)<<new QLabel(qValSS);

  for(int i=0;i<3;i++)
  {
    qVal_L[i]->setAlignment(Qt::AlignCenter);
    qVal[i]->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  }


  layout->addWidget(container(QList<QLabel *>() << new QLabel("IQ-1") << qVal_L[0] << qVal[0]),3,0,1,1);
  layout->addWidget(container(IQ),4,0,1,1);

  layout->addWidget(container(QList<QLabel *>() << new QLabel("IQ-2") << qVal_L[1] << qVal[1]),6,0,1,1);
  layout->addWidget(container(IQ2),7,0,1,1);

  layout->addWidget(container(QList<QLabel *>() << new QLabel("IQ-S") << qVal_L[2] << qVal[2]),9,0,1,1);
  layout->addWidget(container(IQS),10,0,1,1);

  QStringList tiltTitles; tiltTitles<<"Defoc."<<"Latt."<<"SpSplit"<<"Merge";

  for(unsigned int i = 0; i < 4; i++)
  {
    headers << new QLabel(tiltTitles[i]);
    headers[i]->setAlignment(Qt::AlignCenter);
    headers[i]->setFont(font());
  }

  separator = new QWidget;
  separator->setAutoFillBackground(true);
  separator->setPalette(pal);
  separator->setFixedHeight(2);
  separator->setMinimumWidth(minWidth);

  layout->addWidget(separator,11,0,1,1);

  layout->setAlignment(Qt::AlignCenter);

  QWidget *containerWidget = new QWidget(this);
  QGridLayout *containerLayout = initializeLayout(containerWidget);
  containerLayout->setAlignment(Qt::AlignLeft);
  int rows = 5 + 1;
  int columns = headers.size() + 1;


  QStringList items; items<<"TLTAXIS"<<"TLTANG"<<"TLTAXA"<<"TAXA"<<"TANGL";
  QStringList headerItems; headerItems<<"DEFOCUS"<<"LATTICE"<<"TTREFINE"<<"MERGE";

  for(int i = 0; i < rows; i++)
    for(int j = 0; j < columns; j++)
		{
			if(i==0 && j>0) containerLayout->addWidget(headers[j-1],0,j,1,1);
			if(i>0 && j==0)
			{
				item = new QLabel(items[i-1]);
				item->setAlignment(Qt::AlignLeft);
				item->setFont(font());
        titles<<item;
				containerLayout->addWidget(item,i,0,1,1);
			}
			if(i>0 && j>0)
			{
        QString value = statusData->get(headerItems[j-1] + "_" + items[i-1],"value");
        value.truncate(value.indexOf('.')+2);
				item = new QLabel(value);
				item->setAlignment(Qt::AlignCenter);
				item->setFont(font());
				defocus<<item;
				containerLayout->addWidget(item,i,j,1,1);
			}
	  containerLayout->setColumnStretch(j,2);
	}
  containerWidget->setLayout(containerLayout);
  layout->addWidget(containerWidget,12,0,1,1);

  layout->setRowStretch(1,2);
//  layout->setRowStretch(2,2);
//  layout->setRowStretch(4,2);
  layout->setRowStretch(11,2);

  setLayout(layout);
  
  QString toolTipText = "<B>Status window</B><hr>";
  toolTipText += "Theoretical Magnification and Last Quality Value.<br><BR>";
  toolTipText += "Quality value and IQ values for the two unbending runs (IQ-1, IQ-2) and for the unbending with synthetic reference (IQ-S).<BR>";
  toolTipText += "The 9 numbers in one row are the numbers of IQ1, IQ2, IQ3, ..., IQ9 spots in the powerspectrum after unbending.<br><BR>";
  toolTipText += "Currently refined tilt-geometries (<b>bold</b> entries represent the currently used values.)";
  
  setToolTip(toolTipText);

  load();
  updateFontInfo();
}

QLabel *statusView::container(QList<QLabel *> widgets)
{
  QLabel *containerWidget = new QLabel();
  QGridLayout *layout = initializeLayout(containerWidget);
  for(int i=0;i<widgets.size();i++)
	{
    titles<<widgets[i];
	  widgets[i]->setFont(font());
    layout->addWidget(widgets[i],0,i,1,1);
	}
  containerWidget->setLayout(layout);
  return containerWidget;
}

QGridLayout *statusView::initializeLayout(QWidget *parent)
{
  QGridLayout *layout = new QGridLayout(parent);
  layout->setMargin(0);
  layout->setSpacing(0);
  return layout;
}

void statusView::load()
{
  QString qMagS2 = statusData->get("QVal","value"), qMagS1 = statusData->get("CalculatedMag","value");
  qMagS1.truncate(qMagS1.indexOf('.')+2);  qMagS2.truncate(qMagS2.indexOf('.')+2);

  qMag[0]->setText(qMagS1.trimmed());
  qMag[1]->setText(qMagS2.trimmed());	

  QString qVal1S = statusData->get("QVal1","value"), qVal2S = statusData->get("QVal2","value"), qValSS = statusData->get("QValS","value");
  qVal[0]->setText(qVal1S);
  qVal[1]->setText(qVal2S);
  qVal[2]->setText(qValSS);
	
  for(unsigned int j = 0; j < 2; j++)
    for(unsigned int i = 0; i < 9; i++)
    {
      if(j==0) IQ[i]->setText(statusData->get("U" + QString::number(j+1) + "_IQ" + QString::number(i+1),"value"));
	    else IQ2[i]->setText(statusData->get("U" + QString::number(j+1) + "_IQ" + QString::number(i+1),"value"));
    }

  for(quint32 i = 0; i<9; i++)
    IQS[i]->setText(statusData->get("US_IQ" + QString::number(i+1),"value"));

  int rows = 5;
  int columns = 4;

  QStringList items; items<<"TLTAXIS"<<"TLTANG"<<"TLTAXA"<<"TAXA"<<"TANGL";
  QStringList headerItems; headerItems<<"DEFOCUS"<<"LATTICE"<<"TTREFINE"<<"MERGE";

  int defocusMethod = 0;
  if(!statusData->get("DEFOCUS_ACTIVE","value").trimmed().isEmpty())
    defocusMethod=statusData->get("DEFOCUS_ACTIVE","value").toInt();
  
  QFont boldFont(font());
  boldFont.setBold(true);
  
  foreach(QLabel *l, headers)
    l->setFont(font());
  // defocusMethod
  // 0 = not yet defined.
  // 1 = Defocus
  // 2 = Lattice
  // 3 = SpSplit
  // 4 = Merge
  if(defocusMethod>0 && defocusMethod<headers.size()) headers[defocusMethod-1]->setFont(boldFont);
  
  for(int i = 0; i < rows; i++)
    for(int j = 0; j < columns; j++)
		{
      QString value = statusData->get(headerItems[j] + "_" + items[i],"value");
      value.truncate(value.indexOf('.')+2);
		  defocus[i*columns + j]->setText(value);
      if(j == (defocusMethod-1)) defocus[i*columns+j]->setFont(boldFont);
      else defocus[i*columns+j]->setFont(font());
		}
}

void statusView::updateFontInfo()
{
  setFont(QFont("Courier",QApplication::font().pointSize()-1));
  for(int i=0;i<qMag.size();i++)
    qMag[i]->setFont(font());
  for(int i=0;i<qVal.size();i++)
    qVal[i]->setFont(font());
  for(int i=0;i<IQ.size();i++)
    IQ[i]->setFont(font());
  for(int i=0;i<IQ2.size();i++)
    IQ2[i]->setFont(font());
  for(int i=0;i<defocus.size();i++)
    defocus[i]->setFont(font());
  for(int i=0;i<headers.size();i++)
    headers[i]->setFont(font());
  for(int i=0;i<titles.size();i++)
    titles[i]->setFont(font());
}
