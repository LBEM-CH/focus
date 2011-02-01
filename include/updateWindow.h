/*
 *  updateWindow.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 9/7/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef UPDATEWINDOW_H
#define UPDATEWINDOW_H

#include <QWidget>
#include <QLabel>
#include <QPixmap>
#include <QGridLayout>
#include <QHttp>
#include <QDebug>
#include <QTextBrowser>
#include <QProcess>
#include <QPushButton>
#include <QMessageBox>
#include <QAction>
#include "confData.h"

class updateWindow : public QWidget
{
  Q_OBJECT

  public slots:
  void updateTextBox();
  void updateVersion();

  private:

  confData *data;
  QHttp *updateInf;
  QTextBrowser *updateText;
  QLabel *updateTitle, *versionInfo;
  QString installedVersion;


  public:
  updateWindow(confData *conf, QWidget *parent = NULL);

};

#endif

