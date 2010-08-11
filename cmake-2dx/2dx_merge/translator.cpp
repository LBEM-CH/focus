/**************************************************************************
*   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
*   HStahlberg@ucdavis.edu                                                *
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
*   This program is distributed in the hope that it will be useful,       *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
*   GNU General Public License for more details.                          *
*                                                                         *
*   You should have received a copy of the GNU General Public License     *
*   along with this program; if not, write to the                         *
*   Free Software Foundation, Inc.,                                       *
*   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
***************************************************************************/

#include <iostream>
#include "translator.h"
using namespace std;

translator::translator(confData *conf, const QString &appConfig, const QString &translatorDir, QObject *parent)
           :QObject(parent)
{
  data = new confData(appConfig);
  config = conf;
  proc = new QProcess(this);
  proc->setWorkingDirectory(config->getDir("working") + "/" + "SCRATCH");
  QStringList env = QProcess::systemEnvironment();

  foreach(confElement *e, data->getLookupTable())
    env<<e->get("valueLabel") + "_2dx_app=" + e->get("value");

  proc->setEnvironment(env);

  getAvailableTranslators(translatorDir);

}

bool translator::getAvailableTranslators(const QString &translatorDir)
{
  QDir dir(translatorDir);
  if(!dir.exists()) return false;
  QString entry, ext;
  QStringList translatorList;
  foreach(entry,dir.entryList(QStringList()<<"*.tr",QDir::Files))
  {
    ext = entry;
    translators.insert(ext.remove(QRegExp("\\.tr$")).trimmed().toLower(),translatorDir + "/" + entry);
  }
  return true;
}

void translator::open(const QString &fileName)
{
  QString ext = QFileInfo(fileName).suffix().toLower();

  if(translators.contains(ext))
    proc->start(translators[ext],QStringList()<<fileName);
  else if(ext=="ps")
    proc->start(data->get("psViewer","value").trimmed() + " " + fileName);
  else if(ext=="mrc")
  {
    new imageNavigator(config,new mrcImage(fileName),NULL);
  }
  else
  {
    proc->start(data->get("scriptEditor","value").trimmed() + " " + fileName);
  }
}
