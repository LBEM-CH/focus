/*
 *  confSection.cpp
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/22/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "confSection.h"
#include <iostream>
using namespace std;

confSection::confSection(QString title, QObject *parent)
                        :QObject(parent)
{
  sectionTitle=title;
}

void confSection::append(confElement *e)
{
  elements<<e;
  e->setParent(this);
}

QString confSection::title()
{
  return sectionTitle;
}

unsigned int confSection::size()
{
  return elements.size();
}

void confSection::addConcern(QString concern)
{
  QStringList cell = concern.toLower().split(',');

  for(int i=0; i<cell.size(); i++)
    concernList[cell[i].trimmed().toLower()]++;
}

int confSection::concerns(QString concern)
{
  return concernList[concern.toLower()];
}

confSection & confSection::operator<<(confElement *e)
{
  append(e);
  return *this;
}

confElement* confSection::operator[](unsigned int i)
{
  return elements[i];
}

bool confSection::operator==(const confSection& section)
{
  return sectionTitle==section.sectionTitle;
}
