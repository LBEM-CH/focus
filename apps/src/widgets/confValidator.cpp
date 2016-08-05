/*
 *  confValidator.cpp
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/30/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream>
#include "confValidator.h"
using namespace std;

void confValidator::fixup(QString &input) const
{
  if(input.toFloat()>top()) input = QString::number(top());
  else if(input.toFloat()<bottom()) input = QString::number(bottom());
  else input = QString::number(input.toFloat());
}

QDoubleValidator::State confValidator::validate(QString &input, int &) const
{
  bool ok;
  if(input.isEmpty()||(input=="-" && bottom()<=0.0)) return Intermediate;

  input.toFloat(&ok);
  if(ok)
  {
    if(input.toFloat()<=top()&&input.toFloat()>=bottom()) return Acceptable;
    if(input.toFloat()<bottom()) return Intermediate;
    else return Invalid;
  }
  else
  {
    return Invalid;
  }
}

