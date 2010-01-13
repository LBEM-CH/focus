/*
 *  confModel.cpp
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/10/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "confModel.h"

void confModel::initializeModel(confData *data)
{
  confSection *section;
  confElement *element;
  QStandardItem *sectionItem;
  QStandardItem *elementItem;

  for(quint32 i=0;i<data->size();i++)
  {
    section = (*data)[i];
    sectionItem = new QStandardItem(section->title());
    sectionItem->setCheckable(false);
    appendRow(sectionItem);
    for(quint32 j=0;j<section->size();j++)
    {
      element = (*section)[j];
      QList<QStandardItem*> propertyItems;
      elementItem = new QStandardItem(element->get("label"));
      elementItem->setData("label",Qt::UserRole);
      elementItem->setData(qVariantFromValue(element),Qt::UserRole + 1);
      elementItem->setCheckable(false);
      propertyItems<<elementItem;

      elementItem = new QStandardItem(element->get("valueLabel"));
      elementItem->setData("valueLabel",Qt::UserRole);
      elementItem->setData(qVariantFromValue(element),Qt::UserRole + 1);
      elementItem->setCheckable(false);
      propertyItems<<elementItem;

      elementItem = new QStandardItem;
      elementItem->setData("lock",Qt::UserRole);
      elementItem->setData(qVariantFromValue(element),Qt::UserRole + 1);
      elementItem->setCheckable(true);
      QString locked = element->get("locked").trimmed().toLower();
      if(locked == "yes") elementItem->setCheckState(Qt::Checked);
      else if(locked == "no") elementItem->setCheckState(Qt::Unchecked);
      else elementItem->setCheckable(false);
      propertyItems<<elementItem;

      foreach(elementItem,propertyItems)
        elementItem->setEditable(false);

      QString valueElement;
      foreach(valueElement, element->get("value").trimmed().split(','))
      {
        elementItem = new QStandardItem(valueElement);
        elementItem->setData("value",Qt::UserRole);
        elementItem->setData(qVariantFromValue(element),Qt::UserRole + 1);
				qDebug()<<element<<QVariant(element);
        elementItem->setCheckable(false);
				elementItem->setEditable(true);
        propertyItems<<elementItem;
      }

      appendRow(propertyItems);
    }
  }
}

confModel::confModel(confData *conf, QObject *parent)
               :QStandardItemModel(parent)
{
  data = conf;
  initializeModel(conf);
}

bool confModel::setData( const QModelIndex & index, const QVariant & value, int role)
{
  confElement *e = index.data(Qt::UserRole + 1).value<confElement*>();
  e->set("value",value.toString());
  return QStandardItemModel::setData(index,value,role);
}

