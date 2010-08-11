/*
 *  graphicalButton.h
 *  2DX
 *
 *  Created by Bryant Gipson on 3/14/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef GRAPHICALBUTTON_H
#define GRAPHICALBUTTON_H

#include <QCheckBox>
#include <QPainter>
#include <QIcon>

class graphicalButton : public QAbstractButton
{
  Q_OBJECT

  public slots:
  void changeState(bool checked);
  void setCheckable(bool checkable);

  signals:
  void stateChanged(int state);

  private:
  QIcon *ico;
  void initialize();

  public:
  graphicalButton(QWidget *parent = NULL);
  graphicalButton(QIcon *icon, QWidget *parent = NULL);

  protected:
  virtual void paintEvent(QPaintEvent *event);
  virtual void checkStateSet();
//  virtual bool hitButton(const QPoint& pos);
};

#endif
