/*
 *  confDelegate.cpp
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/10/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream>
#include "confDelegate.h"
using namespace std;

confDelegate::confDelegate(confData *conf, QObject *parent)
             :QItemDelegate(parent)
{
  data = conf;
}

void confDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
  //confElement *element = (confElement*)(index.data(Qt::UserRole + 1).value<void*>());

  painter->setFont(option.font);

  QString value = index.data(Qt::DisplayRole).toString();
  QString type = index.data(Qt::UserRole).toString();

  painter->setBackground(index.data(Qt::BackgroundRole).value<QBrush>());

  if(index.data(Qt::UserRole).toString() == "label")
    painter->drawText(option.rect, value, Qt::AlignLeft | Qt::AlignVCenter);
  else if(type == "value")
  {
    painter->drawText(option.rect, value, Qt::AlignLeft | Qt::AlignVCenter);
  }
  else
    QItemDelegate::paint(painter,option,index);
}

void confDelegate::drawCheck(QPainter *painter, const QStyleOptionViewItem &option, const QRect &, Qt::CheckState state) const
{
  if(state == Qt::Checked)
    data->getIcon("lock")->paint(painter,option.rect,Qt::AlignCenter, QIcon::Normal, QIcon::On);
  else if(state == Qt::Unchecked)
    data->getIcon("lock")->paint(painter,option.rect,Qt::AlignCenter, QIcon::Normal, QIcon::Off);
}
