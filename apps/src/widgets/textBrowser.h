/*
 *  textBrowser.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 8/25/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef TEXTBROWSER_H
#define TEXTBROWSER_H

#include <QTextBrowser>
#include <QProcess>
#include "confData.h"

class textBrowser : public QTextBrowser
{
  Q_OBJECT

  public slots:

  virtual void setSource(const QUrl &source);
  void setLocalSource(const QUrl &source);
  void linkClicked(const QUrl &link);

  private:
  confData *data;

  public:
  textBrowser(confData *conf, QWidget *parent = NULL);

};

#endif
