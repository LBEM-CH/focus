/*
 *  confElement.cpp
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/22/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "confElement.h"
#include <iostream>
using namespace std;

confElement::confElement(QObject *parent)
                        : QObject(parent)
{
};

bool confElement::set(QString label, QString value)
{
  bool changed = false;
  if(properties[label.toLower()] != value) changed = true;
  properties[label.toLower()]=value;
  if(changed)
  {
    emit dataChanged();
  }
  return true;
}

QString confElement::get(QString label)
{
  return properties[label.toLower()];
}

bool confElement::concerns(QString concern)
{
  QStringList cell = get("concerns").split(',');

  for(int i=0; i<cell.size(); i++)
    if(concern.trimmed().toLower() == cell[i].trimmed().toLower()) return true;
	
  return false;
}

const QHash<QString,QString> &confElement::propertyList()
{
  return properties;
}

QPoint confElement::toQPoint(int group, bool *ok)
{

  QStringList cell = get("value").split(',');
  int index = 2*group;
  if(cell.size()>=index+2)
  {
    bool oneOk, twoOk;
    QPoint p(cell[index].toInt(&oneOk),cell[index+1].toInt(&twoOk));
    if(ok!=NULL)
      *ok = oneOk && twoOk;
    return p;
  }
  else
  {
    if(ok!=NULL)
      *ok = false;
    return QPoint();
  }
}

QPointF confElement::toQPointF(int group, bool *ok)
{
  QStringList cell = get("value").split(',');
  int index = 2*(group);
  if(cell.size()>=index+2)
  {
    bool oneOk, twoOk;
    QPointF p(cell[index].toFloat(&oneOk),cell[index+1].toFloat(&twoOk));
    if(ok!=NULL)
      *ok = oneOk && twoOk;
    return p;
  }
  else
  {
    if(ok!=NULL)
      *ok = false;
    return QPointF();
  }
}

bool confElement::toBool(bool *ok)
{
QString value=get("value").trimmed().toLower();
  if(value == "y" || value == "yes" ) return true;
  else if(value == "n" || value == "no") return false;
  else if(ok!=NULL) *ok = false;
  return false;
}

float confElement::toFloat(bool *ok)
{
  return get("value").toFloat(ok);
}

QVariant confElement::data()
{
  return QVariant(get("value"));
}

confElement & confElement::operator=(const confElement &e)
{
  properties = e.properties;
  return *this;
}

QPair<float, float> confElement::range()
{
  QStringList type = get("type").split(';');
  float min=0.0, max=0.0;
  foreach(QString e, type)
  {
    if(e.contains("min",Qt::CaseInsensitive)) min = e.section('=',-1,-1).toFloat();
    if(e.contains("max",Qt::CaseInsensitive)) max = e.section('=',-1,-1).toFloat();
  }
  return QPair<float,float>(min,max);
}


