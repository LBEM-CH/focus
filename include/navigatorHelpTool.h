/*
 *  navigatorHelpTool.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 9/20/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */
#ifndef NAVIGATORHELPTOOL_H
#define NAVIGATORHELPTOOL_H

#include <QWidget>
#include <QGridLayout>
#include <QTextBrowser>

class navigatorHelpTool : public QWidget
{
  private:
  QTextBrowser *helpText;

  public:
  navigatorHelpTool(const QString &fileName, QWidget *parent = NULL);
};

#endif
