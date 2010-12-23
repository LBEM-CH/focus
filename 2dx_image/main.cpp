/***************************************************************************
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

#include <QApplication>
#include <QtPlugin>
#include <QDesktopWidget>
#include <QDebug>
#include <iostream>
#include <confData.h>
#include "mainWindow.h"
#include "centralWindow.h"
#include <scriptParser.h>
using namespace std;

#ifdef _USE_STATIC_PLUGINS_
Q_IMPORT_PLUGIN(qjpeg)
Q_IMPORT_PLUGIN(qgif)
#endif

int main(int argc, char *argv[])
{

	if(argc == 3)
	{
		QCoreApplication app(argc, argv);
		QCoreApplication::setApplicationName("2dx_image");
		confData data;
		data.setDefaults(argv[1]);
		QDir standardScriptsDir(data.getDir("standardScriptsDir"));
		QDir customScriptsDir(data.getDir("customScriptsDir"));

		QMap<int, QString> scripts;
		QMap<int, QString> cScripts;

		//	cout<<argv[2]<<endl;

		QStringList searchScripts = QString(argv[2]).remove('"').trimmed().split(',');
		if(searchScripts.isEmpty()) searchScripts<<QString(argv[2]).remove('"').trimmed();

		QStringList sSearchScripts,cSearchScripts;
		for(int i=0;i<searchScripts.size();i++)
		{
			if(searchScripts[i].trimmed().toLower().startsWith('+'))
			{
				QString script =  searchScripts[i].trimmed()+".script";
				script.remove(0,1); 
				cSearchScripts << script;
			}
			else
				sSearchScripts <<  searchScripts[i].trimmed()+".script";
		}

		QMap<int, confData*> localConfList;
		QMap<int, confData*> localCConfList;
		int sortOrder;

		if(!sSearchScripts.isEmpty())
			foreach(QString entry, standardScriptsDir.entryList(sSearchScripts, QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted))
			{
				confData *local = new confData(data.getDir("standardScriptsDir") + "/" + entry);
				local->syncWithUpper();
				sortOrder = local->property("sortOrder").toInt();
				scripts.insert(sortOrder,entry);
				localConfList.insert(sortOrder,local);
			}

		QStringList entryList = customScriptsDir.entryList(cSearchScripts, QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted);
		QString entry;

		if(!cSearchScripts.isEmpty())
			for(int i=0;i<entryList.size();i++)
			{
				entry = entryList[i];
				confData *local = new confData(customScriptsDir.absolutePath() + "/" + entry);
				local->syncWithUpper();
				cScripts.insert(i,entry);
				localCConfList.insert(i,local);
			}

		QProcess process;
		int scriptCount = 0;

		QMapIterator<int, QString> it(scripts);
		while(it.hasNext())
		{
			cout<<"<<@progress: "<<int(scriptCount/float(scripts.size()+cScripts.size())*100.0)<<">>"<<endl;
			scriptCount++;
			it.next();
			scriptParser parser(QList<confData *>()<<localConfList[it.key()]<<&data);
			QString script = it.value();
			script.remove(QRegExp("\\.script$"));

			if(QFileInfo(standardScriptsDir.path() + "/" + script + ".script").exists())
			{
				cout<<"::  Executing: "<<script.toStdString()<<" in "<<data.getDir("working").toStdString()<<endl;
				parser.parse(standardScriptsDir.path() + "/" + script + ".script", data.getDir("working") + "/proc/" + script + ".com");
				parser.execute(script,&data);
			}
		}

		QMapIterator<int, QString> it2(cScripts);
		while(it2.hasNext())
		{
			cout<<"<<@progress: "<<int(scriptCount/float(scripts.size()+cScripts.size())*100.0)<<">>"<<endl;
			scriptCount++;
			it2.next();
			scriptParser parser(QList<confData *>()<<localCConfList[it2.key()]<<&data);
			QString script = it2.value();
			script.remove(QRegExp("\\.script$"));
			if(QFileInfo(customScriptsDir.path() + "/" + script + ".script").exists())
			{
				cout<<"Executing: "<<script.toStdString()<<" in "<<data.getDir("working").toStdString()<<endl;
				parser.parse(customScriptsDir.path() + "/" + script + ".script", data.getDir("working") + "/proc/" + script + ".com");
				parser.execute(script,&data);
			}
		}

		cout<<"<<@progress: 100>>"<<endl; 
	}
	else
	{
		QApplication app(argc, argv);
		QApplication::setApplicationName("2dx_image");

		QFont globalFont = QApplication::font();
		globalFont.setPointSize(12);
		QApplication::setFont(globalFont);

		mainWindow *mainWin;
		if(argc>1)
			mainWin = new mainWindow(argv[1]);
		else
			mainWin = new mainWindow(NULL);
		mainWin->show();
		mainWin->raise();
		mainWin->activateWindow();
		return app.exec();
	}
}
