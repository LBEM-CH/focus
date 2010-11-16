/*
 *  confInput.h
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/22/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef CONFINPUT_H
#define CONFINPUT_H

#include <QWidget>
#include <QEvent>
#include <QWhatsThis>
#include <QWhatsThisClickedEvent>
#include <QGridLayout>
#include <QLabel>
#include <QList>
#include <QLineEdit>
#include <QButtonGroup>
#include <QRadioButton>
#include <QCheckBox>
#include <QComboBox>
#include <QSpacerItem>
#include <QPalette>
#include <QProcess>
#include <QIcon>
#include <QColor>
#include <float.h>
#include "confData.h"
#include "graphicalButton.h"
//#include "confValidator.h"

class confInput : public QWidget
{
  Q_OBJECT

  public slots:
  void save();
  void load();
  void setReadOnlyState(int state);
  void dataModified();
  void updateFontInfo();
  void updateStatus();
  void updateWhatsThis();
  void show();

  signals:
  void shown();

  private:
  confData *data;
  confElement *element;

  QGridLayout *layout;

  QString varUID;

  QFont labelFont;

  QString type;
  int user_level;
  bool is_wrong;

  QLabel *label;
  QList<QLineEdit *> lEdits;
  QComboBox *menu;

  QRadioButton *yes,*no;
  QLabel *yesLabel, *noLabel;

  graphicalButton *lockedBox;
  const QColor isWrongBGColor;
  
  public:
  confInput(confData *conf, confElement *e, QWidget *parent = NULL);
  int userLevel();
  bool isWrong();

  protected:
  void mousePressEvent(QMouseEvent *event);
  bool event(QEvent *event);
};

#endif
