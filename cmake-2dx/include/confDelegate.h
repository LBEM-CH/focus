/*
 *  confDelegate.h
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/10/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef CONFDELEGATE_H
#define CONFDELEGATE_H

#include <QItemDelegate>
#include <QModelIndex>
#include <QPainter>
#include <QBrush>
#include <QApplication>
#include <confData.h>

class confDelegate : public QItemDelegate
{
  Q_OBJECT

  private:
  confData *data;

  public:
  confDelegate(confData *conf, QObject *parent = NULL);

//  QWidget *createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const;
  void paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const;
  void drawCheck(QPainter *painter, const QStyleOptionViewItem &option, const QRect &rect, Qt::CheckState state) const;
};

#endif
