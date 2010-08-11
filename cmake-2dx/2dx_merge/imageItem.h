/*
 *  imageItem.h
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 11/4/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef IMAGEITEM_H
#define IMAGEITEM_H

#include <QStandardItem>
#include <fileWatcher.h>
#include <confData.h>

class imageItem : public QStandardItem
{
  
public:
  imageItem();
  imageItem(const QString &text);
  void appendRow(const QList<imageItem*> &items);
};

#endif
