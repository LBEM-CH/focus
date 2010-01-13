/*
 *  parse-csh.cpp
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/9/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream>
#include "parse_csh.h"
using namespace std;

QString get(const QList<confData *> &data, const QString &value)
{
  confData *local;
  foreach(local, data)
  {
    if(local->get(value) != NULL)
      return local->get(value,"value");
  }
  return "";
}

bool parse_csh(const QList<confData *> &data, QStringList &scriptData)
{
  int i=0;
  QString line;
  QStringList vSearch = data.first()->getValueSearch();
  vSearch << "sortorder" << "global" << "section" << "===" << "manual";
  
  while(!line.contains("$end_local_vars", Qt::CaseInsensitive) && i<scriptData.size())
  {
    line = scriptData[i].trimmed().toLower();
    line = line.remove('#').trimmed();
  
    if(line.startsWith("set "))
    {
      int k = scriptData[i].indexOf('=');
      if(k>0)
      {
        QStringList cell = scriptData[i].split('=');
        cell[0].remove(0,4);
        if(cell[0].trimmed()=="bin_2dx")
        {
          scriptData[i].replace(QRegExp("\"\\ *\""),'"' + data.last()->getDir("binDir") + '"');
        }
        else if(cell[0].trimmed()=="proc_2dx")
          scriptData[i].replace(QRegExp("\"\\ *\""),'"' + data.last()->getDir("procDir") + '"');
        else if(cell[0].trimmed()=="app_2dx_image")
          scriptData[i].replace(QRegExp("\"\\ *\""),'"' + data.last()->getApp("2dx_image") + '"');
        else if(cell[0].trimmed()=="app_2dx_merge")
          scriptData[i].replace(QRegExp("\"\\ *\""),'"' + data.last()->getApp("2dx_merge") + '"');
        else
        {
          QString v = get(data,cell[0].trimmed());
          scriptData[i].replace(QRegExp("\".*\""),'"' + v + '"');
					scriptData.insert(++i, QString("echo \"") + cell[0].trimmed() + " = " + v + "\"\n");
        }
		  
        if(get(data, cell[0].trimmed()).isEmpty() && cell[0].trimmed().toLower() != "bin_2dx" && cell[0].trimmed().toLower() != "proc_2dx" && cell[0].trimmed().toLower() != "app_2dx_image" && cell[0].trimmed().toLower() != "app_2dx_merge")
   	      cerr<<"Variable: "<<cell[0].trimmed().toStdString()<<" not found."<<endl;
      }      
    }
    
    for(int k=0;k<vSearch.size();k++)
      if(line.startsWith(vSearch[k].toLower())) 
      { 
        scriptData.removeAt(i);
        i--;
        break;
      }

    
    i++; 
  }
  
  if(i>0 && scriptData[i-1].toLower().contains("$end_local_vars")) scriptData.removeAt(i-1);
  
  while(!line.contains("$end_vars") && i<scriptData.size())
  {
    line = scriptData[i].trimmed().toLower();
	
    if(line.contains("set "))
    {
      int k = line.indexOf('=');
      if(k>0)
      {
        QStringList cell = line.split('=');
        cell[0].remove(0,4);
        if(cell[0].trimmed()=="bin_2dx")
        {
          scriptData[i].replace(QRegExp("\"\\ *\""),'"' + data.last()->getDir("binDir") + '"');
        }
        else if(cell[0].trimmed()=="proc_2dx")
          scriptData[i].replace(QRegExp("\"\\ *\""),'"' + data.last()->getDir("procDir") + '"');
        else if(cell[0].trimmed()=="app_2dx_image")
          scriptData[i].replace(QRegExp("\"\\ *\""),'"' + data.last()->getApp("2dx_image") + '"');
        else if(cell[0].trimmed()=="app_2dx_merge")
					scriptData[i].replace(QRegExp("\"\\ *\""),'"' + data.last()->getApp("2dx_merge") + '"');
				else
				{
					QString v = get(data,cell[0].trimmed());
					scriptData[i].replace(QRegExp("\".*\""),'"' + v + '"');
					scriptData.insert(++i, QString("echo \"") + cell[0].trimmed() + " = " + v + "\"\n");
				}  

				if(get(data, cell[0].trimmed()).isEmpty() && cell[0].trimmed().toLower() != "bin_2dx" && cell[0].trimmed().toLower() != "proc_2dx" && cell[0].trimmed().toLower() != "app_2dx_image" && cell[0].trimmed().toLower() != "app_2dx_merge")
					cerr<<"Variable: "<<cell[0].trimmed().toStdString()<<" not found."<<endl;
			}
		}
		i++;
	}

	if(i<scriptData.size())
		scriptData.removeAt(i-1);

	return true;
}

const QString &executionCall_csh(const QString &destination)
{
	return destination;
}
