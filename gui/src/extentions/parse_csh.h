/*
 *  parse_csh.h
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/9/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef PARSE_CSH_H
#define PARSE_CSH_H

#include <QStringList>
#include <confData.h>

QString get(const QList<confData *> &data, const QString &value);
bool parse_csh(const QList<confData *> &data, QStringList &scriptData);
const QString &executionCall_csh(const QString &destination);

#endif
