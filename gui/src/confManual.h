/*
 *  confManual.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 8/25/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef CONFMANUAL_H
#define CONFMANUAL_H

#include <QLabel>
#include <QVBoxLayout>
#include <QProcess>
#include "confData.h"
#include "textBrowser.h"

class confManual : public QWidget
{
  Q_OBJECT

  public slots:

  private:
  confData *data;
  confData *globalData;

  textBrowser *text;

  public:
  confManual(confData *conf, confData *localConf, QWidget *parent = NULL);

};

#endif
