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

#ifndef controlActionsGroup_h
#define controlActionsGroup_h

#include <QPushButton>
#include <QBoxLayout>
#include "scriptProgress.h"
#include "confData.h"
#include "graphicalButton.h"
#include "progressStamps.h"

class controlActionsGroup : public QWidget
{
  Q_OBJECT

  public slots:
  void setProgress(int value);
  void incrementProgress(int inc);
  void setText(QString text);
  void scriptFinished();
  void saveClicked();
  void saveAvailable(bool available);
  void executeClicked();
  void setManual(bool show);

  signals:
  void hideWidget();
  void toggleInfo();
  void toggleManual(bool);
  void save();
  void execute(bool);
  void refresh();
  void viewHelp();
  void reportBug();

  public:
  enum type { actions, viewActions, progress, header, footer, none };

  private:

  QHBoxLayout *layout;
  confData *conf;

  graphicalButton *saveButton;
  graphicalButton *executeButton;
  graphicalButton *helpButton;
  scriptProgress *progressBar;


  public:
  controlActionsGroup(confData *data, controlActionsGroup::type, QWidget *parent = NULL);
  ~controlActionsGroup();
};
#endif
