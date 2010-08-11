/*
 *  scriptParser.h
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/9/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef SCRIPTPARSER_H
#define SCRIPTPARSER_H

#include <QFile>
#include <QDir>
#include <QProcess>
#include <confData.h>
#include <parse_csh.h>

class scriptParser : public QObject
{
  Q_OBJECT

  private:
  QList<confData *> data;
  QString executionCall;

  public:
  scriptParser(confData *data, QObject *parent = NULL);
  scriptParser(const QList<confData *> &data, QObject *parent = NULL);
  int parse(const QString &source, const QString &destination);
  void execute(const QString &script, confData *data);
  void execute(const QString &source, const QString &working, confData *localData);
  const QString &executionString();

  static bool parseResults(confData  *conf, const QString &results);
};

#endif
