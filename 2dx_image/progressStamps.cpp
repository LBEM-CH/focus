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

#include <iostream>
#include "progressStamps.h"
using namespace std;

progressStamps::progressStamps(confData *conf, QWidget *parent)
               :QWidget(parent)
{
  setFocusPolicy(Qt::NoFocus);
  setFixedHeight(41);
  QHBoxLayout *layout = new QHBoxLayout(this);
  layout->setAlignment(Qt::AlignCenter);
  data = conf;
  stampNames << "fft" << "defocus" << "lattice" << "spots" << "unbend" << "ctf" << "mask" << "phori" << "merge" << "reprocess";
  variableNames << "FFT_done"<< "DEFOCUS_done"<< "LATTICE_done"<< "SPOTS_done"<< "UNBENDING_done"<< "CTF_done" << "MASKING_done"<< "PHASEORI_done"<< "MERGING_done"<< "REPROCESSING_done";
 // QString resourceDir = data->getDir("icons");
  QString resourceDir =  conf->getDir("config") + "resource/";
  for(int i=0;i<stampNames.size();i++)
  {
    //cout << "adding the stamp " << stampNames[i].toStdString() << endl;
    stampImages[stampNames[i]] = QPair<QPixmap,QPixmap>(QPixmap(resourceDir + stampNames[i] + "-IU.png"),QPixmap(resourceDir + stampNames[i] + "-ID.png"));
    stamps[stampNames[i]] = new QLabel(this);
    stamps[stampNames[i]]->setPixmap(stampImages[stampNames[i]].first);
    //stamps[stampNames[i]]->setText(stampNames[i]);
    layout->addWidget(stamps[stampNames[i]]);
  }
  setLayout(layout);
  connect(data,SIGNAL(dataModified(bool)),this,SLOT(load(bool)));
  load(true);
}

void progressStamps::load(bool /* modified */)
{
  bool values[variableNames.size()];
  for(int i=0;i<variableNames.size();i++)
    values[i] = data->get(variableNames[i],"value").trimmed().toLower()=="y";

  for(int i=0;i<stampNames.size();i++)
  {
    if(valueValid(i,values))
      stamps[stampNames[i]]->setPixmap(stampImages[stampNames[i]].second);
    else
      stamps[stampNames[i]]->setPixmap(stampImages[stampNames[i]].first);
  }
  update();
}

bool progressStamps::valueValid(int index, bool *values)
{
  return values[index];
  /*
  if(index == 0) return values[0];
  if(index == 1) return values[1];
  if(index == 2) return values[2];
  if(index == 3) return values[3] && values[2];
  if(index == 4) return values[4] && values[3] && values[2];
  if(index>=5) return values[index];
  return false;
*/
}
