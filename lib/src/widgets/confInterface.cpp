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

#include <QLinearGradient>
#include <QApplication>
#include "confInterface.h"
#include <iostream>
using namespace std;

confInterface::confInterface(confData *conf, QString concerns, QWidget *parent)
                            :QWidget(parent)
{
  data=conf;
  alt = QColor(227,233,244);
  alt_is_wrong = QColor(210,210,103);
  normal_is_wrong = QColor(250,250,123);
  advancedView = false;

  userLevel = 0;
  isWrong = 0;

  //Actions
  saveAction = new QAction(tr("Save"),this);
  loadAction = new QAction(tr("Load"),this);
  executeAction = new QAction(tr("Execute"),this);

  addAction(saveAction);
  addAction(loadAction);
  addAction(executeAction);

  connect(saveAction,SIGNAL(triggered()),this,SLOT(save()));
  connect(loadAction,SIGNAL(triggered()),this,SLOT(load()));
  connect(executeAction,SIGNAL(triggered()),this,SLOT(execute()));
  //connect(this,SIGNAL(fontInfoUpdated()),this,SLOT(updateFontInfo()));

  QVBoxLayout *generalLayout = new QVBoxLayout(this);
  generalLayout->setMargin(0);
  generalLayout->setSpacing(0);
  generalLayout->setAlignment(Qt::AlignTop);

  confSectionHeader *sectionHeader;

  for(unsigned int i=0;i<data->size();i++)
  {
    if(concerns.isEmpty() || (*data)[i]->concerns(concerns))
    {
      sectionHeader = new confSectionHeader(data,(*data)[i]);
      titles<<sectionHeader;
      generalLayout->addWidget(sectionHeader);

      char color=1;
      confElement *element;
      for(unsigned int j=0;j<(*data)[i]->size();j++)
      {
        element = (*(*data)[i])[j];
        QPalette pal(palette());
    	if(color)
        {
          pal.setColor(QPalette::Window,alt);
        }
        else
        {
          pal.setColor(QPalette::Window,Qt::white);
        }
        if((concerns.isEmpty() || element->concerns(concerns)) && !element->get("userLevel").contains("hidden",Qt::CaseInsensitive))
        {
          color=color^1;
	  confInput *input = new confInput(data,element,this);
          sectionHeader->addChild(input);
          lookup.insert(sectionHeader,input);
          if(input->userLevel()==0)
            emptyingSections<<sectionHeader;

          input->setPalette(pal);

          inputs.insert((*(*data)[i])[j]->get("valueLabel"), input);

   	  generalLayout->addWidget(input);
  	  connect(this,SIGNAL(saveAll()),input,SLOT(save()));
   	  connect(this,SIGNAL(loadAll()),input,SLOT(load()));
          connect(this,SIGNAL(fontInfoUpdated()),input,SLOT(updateFontInfo()));
        }
      }
      //connect(collapseButton,SIGNAL(toggled(bool)),this,SLOT(reselect(bool)));
    }
  }

  emptyingSections = QSet<QWidget*>::fromList(lookup.keys()) - emptyingSections;
  setUserLevel(0);
  setLayout(generalLayout);
}

void confInterface::openAllSections()
{
  foreach(confSectionHeader *h, titles)
  {
//    h->showAllChildren();
  }
}

void confInterface::save()
{
  emit saveAll();
  data->save();
}

void confInterface::load()
{
  emit loadAll();
}

void confInterface::execute()
{

}

void confInterface::updateFontInfo()
{
  for(int i=0;i<titles.size();i++)
  {
    confSectionHeader *title = titles[i];
    QFont titleFont(QApplication::font());
    titleFont.setPointSize(titleFont.pointSize()-1);
    titleFont.setWeight(QFont::Bold);
    title->setFont(titleFont);
  }
  update();
  emit fontInfoUpdated();
}


void confInterface::setUserLevel(int level)
{
  openAllSections();
  QPalette pal(palette());
  char color = 1;
  userLevel = level;

  if(level == 0)
  {
    QMapIterator<QWidget*,confInput*> i(lookup);
    while(i.hasNext())
    {
      i.next();
      if(color)
      {
          pal.setColor(QPalette::Window,alt);
      }
      else
      {
          pal.setColor(QPalette::Window,Qt::white);
      }

      if(i.value()->userLevel() == 1)
        i.value()->hide();
      else
      {
        i.value()->setPalette(pal);
        color^=1;
      }
    }

    foreach(QWidget *i, emptyingSections)
      i->hide();

  }
  else
  {
    QMapIterator<QWidget*,confInput*> i(lookup);
    while(i.hasNext())
    {
      i.next();
      if(color)
      {
          pal.setColor(QPalette::Window,alt);
      }
      else
      {
          pal.setColor(QPalette::Window,Qt::white);
      }

      if(i.key()->isHidden()) i.key()->show();
      if(i.value()->isHidden()) i.value()->show();
      i.value()->setPalette(pal);
      color^=1;
    }
  }
}

void confInterface::hideAll()
{
  openAllSections();
  confInput *input;
  foreach(input,inputs)
  {
    input->hide();
  }
}

void confInterface::select(const QStringList &selectionList, int)
{
  openAllSections();
  setUpdatesEnabled(false);
  hide();
  selectedInputs = selectionList;
  QString value;
  bool odd = false;
  bool showAtEnd = false;
  hideAll();
  QPalette pal(palette());

  QSet<QString> selectedSet;
  QStringList currentInputList = inputs.keys();
  QSet<QString> currentSet = QSet<QString>::fromList(currentInputList);

  foreach(value, selectedInputs)
  {
    if(!value.contains(QRegExp("\\/.*\\/")))
    {
      if(!value.contains(QRegExp("[\\*\\?]")))
        selectedSet<<value;
      else
        selectedSet.unite(QSet<QString>::fromList(currentInputList.filter(QRegExp('^' + value.replace('*',".*").replace("?",".") + '$'))));
    }
    else
      selectedSet.unite(QSet<QString>::fromList(currentInputList.filter(QRegExp(value.remove('/')))));
  }

  foreach(value, currentSet.intersect(selectedSet))
  {
    if (inputs[value]->userLevel() <= userLevel && inputs[value]->isHidden())
      inputs[value]->show();
    showAtEnd = true;
  }

  foreach(QWidget *section, lookup.keys())
  {
    section->hide();
    foreach(confInput *input, lookup.values(section))
    {
      if(!input->isHidden())
      {
        if(section->isHidden()) section->show();
        if(input->isWrong())
        {
          if(odd) pal.setColor(QPalette::Window,alt);
          else pal.setColor(QPalette::Window,Qt::white);
        }
        else
        {
          if(odd) pal.setColor(QPalette::Window,alt);
          else pal.setColor(QPalette::Window,Qt::white);
        }
        odd^=true;
        input->setPalette(pal);
        showAtEnd = true;
      }
    }
  }
  setUpdatesEnabled(true);
  if(showAtEnd) show();
  update();
}

void confInterface::setSelectionUserLevel(int level)
{
  userLevel = level;
  select(selectedInputs,userLevel);
}

void confInterface::reselect(bool value)
{
  if(!value)
    select(selectedInputs,userLevel);
}

