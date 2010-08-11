/*
 *  confModel.h
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/10/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef CONFMODEL_H
#define CONFMODEL_H

#include <QStandardItemModel>
#include <QFile>
#include <QPointer>
#include <confData.h>
#include <QDebug>

class confModel : public QStandardItemModel
{
  Q_OBJECT

  private:
  confData *data;

  void initializeModel(confData *data);

  public:
  confModel(confData *conf, QObject *parent = NULL);
	bool setData( const QModelIndex & index, const QVariant & value, int role = Qt::EditRole);
};

#endif

