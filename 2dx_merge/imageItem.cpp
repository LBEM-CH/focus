/*
 *  imageItem.cpp
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 11/4/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "imageItem.h"

imageItem::imageItem()
          :QStandardItem()
{
}

imageItem::imageItem(const QString &text)
          :QStandardItem(text)
{
  
}

void imageItem::appendRow(const QList<imageItem*> &items)
{
  QList<QStandardItem*> itemList;
  foreach(imageItem* item,items)
    itemList<<(QStandardItem*)item;
  QStandardItem::appendRow(itemList);
}