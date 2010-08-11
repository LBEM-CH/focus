/*
 *  scriptParser.cpp
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/9/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream>
#include <QDebug>
#include "scriptParser.h"
using namespace std;

scriptParser::scriptParser(confData *conf, QObject *parent)
             :QObject(parent)
{
  data << conf;
}

scriptParser::scriptParser(const QList<confData *> &conf, QObject *parent)
             :QObject(parent)
{
  data = conf;
}

int scriptParser::parse(const QString &source, const QString &destination)
{
  QFile s(source), d(destination);
//  cerr<<destination.toStdString()<<endl;
  if(!s.open(QIODevice::ReadOnly | QIODevice::Text)) {qDebug()<<"Template read failed."; return -1;}
  
  QStringList scriptData;
  QString line;

  while(!s.atEnd())
    scriptData<<s.readLine();
  s.close();
  
  if(scriptData[0].startsWith("#!/") && scriptData[0].contains("/csh")) 
  {
    parse_csh(data,scriptData);
    executionCall = executionCall_csh(destination);
  }
        
  if(!d.open(QIODevice::WriteOnly | QIODevice::Text)) {qDebug()<<"Script write failed for "<<destination; return -2;}    
    
  foreach(line,scriptData)
    d.write(line.toAscii());
    
  d.setPermissions(QFile::ExeOwner | QFile::ReadOwner | QFile::WriteOwner);
  d.close();

  return 0;
}

const QString &scriptParser::executionString()
{
  return executionCall;
}

bool scriptParser::parseResults(confData *conf, const QString &results)
{
//  currentResults = results;
  QFile data(results);
  if(!data.open(QIODevice::ReadOnly | QIODevice::Text)) return 0;

  QString line;
  while(!data.atEnd())
  {
    line = data.readLine();
		QRegExp lock("^\\s*#\\s(un)?lock\\s(.*)$",Qt::CaseInsensitive);
		QRegExp var("^\\s*set\\s*(\\S+)\\s*=\\s*\\\"{0,1}(.*)\\\"{0,1}");
    if(var.indexIn(line)!=-1)
    {
      QString variable = var.cap(1).trimmed();
      QString value = var.cap(2).trimmed().remove('"');
      if(!variable.isEmpty() && !value.isEmpty())
				conf->set(variable,value);
    }
		if(lock.indexIn(line)!=-1)
    {
      QString variable = lock.cap(2).remove('"').trimmed();
      QString value = "NO";
      bool setLock = !(lock.cap(1).toLower() == "un");
      if(setLock)      
        value = "YES";
      if(conf->get(variable)!=NULL && !conf->get(variable,"locked").trimmed().isEmpty())
				conf->get(variable)->set("locked",value);
			else
        qDebug()<<"No lockable variable "<<variable<<"!";
    }
 
  }
  data.close();
  return 1;
}

void scriptParser::execute(const QString &script, confData *localData)
{
  QProcess process;
  process.setWorkingDirectory(localData->getDir("working"));
  process.setStandardOutputFile(localData->getDir("working") + "/LOGS/" + script + ".log");
  process.start(executionCall, QIODevice::ReadOnly);
  process.waitForFinished(6*(60*60*1000));
  parseResults(localData,localData->getDir("working") + "/LOGS/" + script + ".results");
  localData->save();
}

void scriptParser::execute(const QString &source, const QString &working, confData *localData)
{
  
}

