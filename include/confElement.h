/*
 *  confElement.h
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/22/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef _CONFELEMENT_H_
#define _CONFELEMENT_H_

#include <QHash>
#include <QStringList>
#include <QPoint>
#include <QVariant>
#include <QPair>


class confElement : public QObject
{
  Q_OBJECT

  signals:
  void dataChanged();

  private:
  QHash<QString,QString> properties;

  public:
  confElement(QObject *parent = NULL);

  bool set(QString label, QString value);
  QString get(QString label);

  bool concerns(QString concern);

  const QHash<QString,QString> &propertyList();

  QPoint toQPoint(int group = 0, bool *ok = NULL);
  QPointF toQPointF(int group = 0, bool *ok = NULL);
  bool toBool(bool *ok = NULL);
  float toFloat(bool *ok = NULL);
  QVariant data();
  QPair<float,float> range();

  confElement &operator=(const confElement &e);
  QString toString();
};

Q_DECLARE_METATYPE(confElement*);

#endif
