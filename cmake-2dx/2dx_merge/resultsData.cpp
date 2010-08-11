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
#include "resultsData.h"
using namespace std;

resultsData::resultsData(confData *conf, const QString &fileName, const QString &defaultDir, QObject *parent)
            :QObject(parent)
{
  data = conf;
  mainDir = defaultDir;
  load(fileName);
  dryRun = false;
  watcher.setFile(fileName);
  connect(&watcher,SIGNAL(fileChanged(const QString &)),this,SLOT(load()));
}

bool resultsData::load(const QString &name)
{
  fileName = name;
  QFile file(fileName);
  results.clear();
  images.clear();
  if(!file.open(QIODevice::ReadOnly | QIODevice::Text))  {emit loaded(false); return false;}
  QString line, label, image, nickName;
  QString currentDirectory = mainDir;
  results[currentDirectory]["##CONFFILE##"] = "2dx_merge.cfg";
  int k = 0;
  while(!file.atEnd())
  {
    line = file.readLine();
    if(line.contains(QRegExp("^\\s*#")) && !line.contains(QRegExp("^\\s*#\\s*lock",Qt::CaseInsensitive)))
    {
      QRegExp pattern(".*#\\s*image(-important)?\\s*:\\s*\"?([^\"\\s]*)\"?\\s*(<.*>)?",Qt::CaseInsensitive);
      if(pattern.indexIn(line)!=-1)
      {               
        QString important;
        if(pattern.cap(1).toLower()=="-important") important = "<<@important>>";
        image = pattern.cap(2);
        nickName = pattern.cap(3);
        nickName.remove(QRegExp("^.*<|>.*$"));

        QFileInfo inf(currentDirectory + "/" + image);
        QString order = QString("%1").arg(k,6,10,QChar('0'));
        
        order.prepend("<<@");
        order.append(">>");
        if(inf.isDir())
        {
          QDir dir(inf.canonicalFilePath());
          foreach(QString file, dir.entryList(QDir::Files))
            images[inf.canonicalFilePath()][order + file] = important+file;
        }
        else
          images[currentDirectory][order + image] = important+nickName;
        k++;
      }
      // HEN>
      // else if(pattern.errorString().trimmed().toLower()!="no error occured") qDebug()<<pattern.errorString();
      // HEN<
    }
    else
    {
      //line.replace(QRegExp("#.*"),"");
      if(line.contains(QRegExp("<\\s*IMAGEDIR\\s*=", Qt::CaseInsensitive)))
      {
        currentDirectory = line.replace(QRegExp(".*<\\s*IMAGEDIR\\s*=\\s*\\\"?\\s*([^\"\\s]*)\\s*\\\"?\\s*>.*", Qt::CaseInsensitive),"\\1");
        results[currentDirectory]["##CONFFILE##"] = "2dx_image.cfg";
      }
      else if(line.contains(QRegExp("<\\s*/IMAGEDIR\\s*>", Qt::CaseInsensitive)))
      {
        currentDirectory = mainDir;
        results[currentDirectory]["##CONFFILE##"] = "2dx_merge.cfg";
      }
      else if(line.contains(QRegExp("set\\s*\\w*\\s*=", Qt::CaseInsensitive)))
      {
        QStringList data = line.replace(QRegExp("\\s*set\\s*(\\w*)\\s*=[\\s\"]*([^\\s\"]*)[\\s\"]*.*", Qt::CaseInsensitive), "\\1|@|\\2").split("|@|");
        if(data.size() == 2)
          results[currentDirectory][data[0]] = data[1];
      }
      else if(line.contains(QRegExp("^\\s*#\\s*lock",Qt::CaseInsensitive)))
      {

      }
    }
  }

//  printValues();

  file.close();
  emit loaded(true);
  return true;
}

bool resultsData::save()
{
	QString confFile;
	QMapIterator<QString, QMap<QString, QString> > it(results);

	while(it.hasNext())
	{
		it.next();
		if(QFileInfo(it.key()+"/"+it.value()["##CONFFILE##"]).exists())
		{
			if(it.key()!=mainDir)
			{
				confData local(it.key() + "/" + it.value()["##CONFFILE##"], data->getDir("config") + "/" + "2dx_master.cfg");
				local.syncWithUpper();
				if(!local.isEmpty())
				{
					QMapIterator<QString, QString> j(it.value());
					while(j.hasNext())
					{
						j.next();
						if(!j.key().contains(QRegExp("^##\\w*##$")))
						{
							if(!masked(it.key(),j.key()) && !dryRun)
								local.set(j.key().trimmed(),j.value().trimmed());
						}
					}
					local.save();
				}
			}
			else
			{
				QMapIterator<QString, QString> j(it.value());
				while(j.hasNext())
				{
					j.next();
					if(!j.key().contains(QRegExp("^##\\w*##$")))
					{
						if(!masked(it.key(),j.key()) && !dryRun)
							data->set(j.key().trimmed(),j.value().trimmed());
					}
				}
			} 
		}
		else
			cerr<<(it.key() + "/" + "2dx_image.cfg").toStdString()<<" does not exist."<<endl;
	}

	//  cerr<<"Saved"<<endl;
	emit saved(true);

	return true;
}

void resultsData::printValues()
{
	QString line;

	QMapIterator<QString, QMap<QString, QString> > it(results);
	while(it.hasNext())
	{
		it.next();
		cerr<<it.key().toStdString()<<endl;
		QMapIterator<QString, QString> j(it.value());
		while(j.hasNext())
		{
			j.next();
			cerr<<" "<<j.key().toStdString()<<" = "<<j.value().toStdString()<<endl;
		}
	}

	cerr<<endl<<"Images: "<<endl;

	QMapIterator<QString, QMap<QString, QString> > k(images);
	while(k.hasNext())
	{ 
		k.next();
  	QMapIterator<QString, QString> j(it.value());   
    while(j.hasNext())
    {
      j.next();
      cerr<<k.key().toStdString()<<endl;
      cerr<<" "<<j.value().toStdString()<<endl;      
    }
	}
}

bool resultsData::load()
{
	return load(fileName);
}

void resultsData::setMasked(const QString &directory, const QString &variable)
{
	maskedResults<<maskHash(directory,variable);
}

bool resultsData::masked(const QString &directory, const QString &variable)
{
	return maskedResults.contains(maskHash(directory,variable));
}

void resultsData::clearMasked()
{
	maskedResults.clear();
}

quint32 resultsData::maskHash(const QString &directory, const QString &variable)
{

	QString dir = directory + " " + variable;
	dir.remove('/');

	return qHash(dir);
}

bool resultsData::dryRunMode()
{
	return dryRun;
}

void resultsData::setDryRunMode(bool value)
{
	dryRun = value;
}

