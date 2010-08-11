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

#ifndef CONFSECTIONHEADER_H

#include <QFrame>
#include <QHBoxLayout>
#include <QLinearGradient>
#include <QLabel>
#include <QHash>
#include <confData.h>
#include <confInput.h>
#include <graphicalButton.h>

class confSectionHeader : public QFrame
{
  Q_OBJECT

  public slots:
  void setChildrenHidden(bool hidden);
  void clearChildren();
  void addChild(confInput *input);
  void collapse(bool value);
  void showAllChildren(bool value=true);
  void childShown();

  signals:
  void hideChildren(bool value);

  private:
  confData *data;
  confSection *section;
  QList<confInput*> childInputs;
  QHash<confInput*,bool> visible;
  
  bool childrenHiddenValue;

  graphicalButton *collapseButton;
 
  public:
  confSectionHeader(confData *data, confSection *section, QWidget *parent = NULL);
  bool childrenHidden();
};

#endif
