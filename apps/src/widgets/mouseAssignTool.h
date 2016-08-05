/*
 *  mouseAssignTool.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 8/18/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef MOUSEASSIGNTOOL_H
#define MOUSEASSIGNTOOL_H

#include <QLabel>
#include <QGridLayout>

class mouseAssignTool : public QWidget
{
  Q_OBJECT

  QList<QLabel *> buttons;

  public:
  mouseAssignTool(QWidget *parent = NULL);
  void assignButton(int button, const QString &text);

};

#endif
