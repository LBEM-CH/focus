/*
 *  userLevelGroup.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/9/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef LEVELGROUP_H
#define LEVELGROUP_H

#include <QButtonGroup>
#include <QGridLayout>
#include <QStringList>
#include "confData.h"
#include "graphicalButton.h"

class levelGroup : public QWidget
{
  Q_OBJECT

  public slots:
  void setTitle(int v);
  void setLevel(int level);

  signals:
  void levelChanged(int v);
  void titleChanged(const QString &title);

  private:
  confData *data;
  QButtonGroup *buttonGroup;
  QList<graphicalButton*> buttons;
  QStringList title;
  QStringList toolTip;
  QStringList icon;

  public:
  levelGroup(confData *conf, int buttons, const QStringList &titles, const QStringList &toolTips, const QStringList &icons = QStringList()<<"gbAqua"<<"gbGreen"<<"gbRed", QWidget *parent = NULL);
  int level();
  void setTitleNames(const QStringList &titles);
};

#endif

