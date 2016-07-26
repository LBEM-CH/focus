/*
 *  userLevelGroup.cpp
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/9/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "levelGroup.h"

#include <iostream>
using namespace std;

levelGroup::levelGroup(confData *conf, int buttonCount, const QStringList &titles, const QStringList &toolTips, const QStringList &icons, QWidget *parent)
                              :QWidget(parent)
{
  data = conf;
  title = titles;
  toolTip = toolTips;
  icon = icons;
  QGridLayout *buttonContainerLayout = new QGridLayout(this);
  buttonContainerLayout->setMargin(0);
  buttonContainerLayout->setSpacing(0);
  buttonContainerLayout->setAlignment(Qt::AlignLeft);
  buttonGroup = new QButtonGroup(this);

  for(int i=0; i<buttonCount; i++)
    buttons<<new graphicalButton(data->getIcon(icon[i]),this);


  for(int i = 0; i<buttonCount; i++)
    buttons[i]->setToolTip(toolTip[i]);//"Low Verbosity Output");

  for(int i=0;i<buttons.size();i++)
  {
    buttons[i]->setCheckable(true);
    buttonGroup->addButton(buttons[i]);
    buttonGroup->setId(buttons[i],i);
    buttonContainerLayout->addWidget(buttons[i],0,i,1,1);
  }
  buttons[0]->setChecked(true);
  buttonGroup->setExclusive(true);

  setMaximumWidth(buttons[0]->width()*buttonCount);
  setLayout(buttonContainerLayout);

  connect(buttonGroup,SIGNAL(buttonClicked(int)),this,SIGNAL(levelChanged(int)));
  connect(buttonGroup,SIGNAL(buttonClicked(int)),this,SLOT(setTitle(int)));
}

int levelGroup::level()
{
  return buttonGroup->checkedId();
}

void levelGroup::setTitle(int v)
{
  emit titleChanged(title[v]);//"Logfile - Low Verbosity");
}

void levelGroup::setTitleNames(const QStringList &titles)
{
  title = titles;
}

void levelGroup::setLevel(int level)
{
  buttons[level]->setChecked(true);
  emit setTitle(level);
}
