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
#include <QDebug>
#include <QCommandLineParser>

#include <iostream>

#include "ParameterConfiguration.h"
#include "ScriptParser.h"
#include "ProjectData.h"
#include "ApplicationData.h"
#include "ScriptData.h"

QDir getProjectDir(const QString& workingDir) {
    QDir dir(workingDir);
    while(!QFileInfo(dir.canonicalPath() + "/merge/2dx_merge.cfg").exists() && !dir.isRoot()) {
        dir.cdUp();
    }
    
    if(QFileInfo(dir.canonicalPath() + "/merge/2dx_merge.cfg").exists()) return dir;
    else {
        qDebug() << "ERROR: Could not find the project directory!!";
        QCoreApplication::exit(1);
    }  
}

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    QCoreApplication::setApplicationName("2dx_image");
    QCoreApplication::setOrganizationName("C-CINA");
    QCoreApplication::setOrganizationDomain("c-cina.org");

    QCommandLineParser cliParser;
    cliParser.setApplicationDescription("2DX Software Suite: 2dx_image Command Line Version\n(If you were looking for GUI, try 2dx_gui instead.)");
    cliParser.addHelpOption();
    cliParser.addVersionOption();
    cliParser.addPositionalArgument("image_dir", "Path of the 2dx Image Working Dir to be opened.");
    cliParser.addPositionalArgument("scripts_to_run", "Comma separated list of scripts to run. Add '+' before the name if it is a custom script");
    cliParser.process(app);

    const QStringList args = cliParser.positionalArguments();
    if (args.size() == 2) {
        //Find the project dir
        QDir projectDir = getProjectDir(args[0]);
        projectData.initiailze(projectDir);

        QDir standardScriptsDir(ApplicationData::scriptsDir().canonicalPath() + "/image/standard");
        QDir customScriptsDir(ApplicationData::scriptsDir().canonicalPath() + "/image/custom");
        
        QMap<int, QString> scripts;
        QMap<int, QString> cScripts;

        QStringList searchScripts = QString(args[1]).remove('"').trimmed().split(',');
        if (searchScripts.isEmpty()) searchScripts << QString(args[1]).remove('"').trimmed();

        QStringList sSearchScripts, cSearchScripts;
        for (int i = 0; i < searchScripts.size(); i++) {
            if (searchScripts[i].trimmed().toLower().startsWith('+')) {
                QString script = searchScripts[i].trimmed() + ".script";
                script.remove(0, 1);
                if(!QFileInfo(customScriptsDir.canonicalPath() + "/" + script).exists()) {
                    qDebug() << "WARNING: Custom script: " << script << " does not exist in: \n\t" << customScriptsDir.canonicalPath();
                }
                cSearchScripts << script;
            } else {
                QString script = searchScripts[i].trimmed() + ".script";
                if(!QFileInfo(standardScriptsDir.canonicalPath() + "/" + script).exists()) {
                    qDebug() << "WARNING: Standard script: " << script << " does not exist in: \n\t" << standardScriptsDir.canonicalPath();
                }
                sSearchScripts << script;
            }
        }

        
        int sortOrder;

        if (!sSearchScripts.isEmpty()) {
            foreach(QString entry, standardScriptsDir.entryList(sSearchScripts, QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted)) {
                ScriptData local(standardScriptsDir.canonicalPath() + "/" + entry);
                sortOrder = local.getProperty("sortOrder").toInt();
                scripts.insert(sortOrder, entry);
            }
        }
        
        QStringList entryList = customScriptsDir.entryList(cSearchScripts, QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted);
        QString entry;

        if (!cSearchScripts.isEmpty()) {
            for (int i = 0; i < entryList.size(); i++) {
                entry = entryList[i];
                cScripts.insert(i, entry);
            }
        }

        int scriptCount = 0;

        QMapIterator<int, QString> it(scripts);
        while (it.hasNext()) {
            //cout << "<<@progress: " << int(scriptCount / float(scripts.size() + cScripts.size())*100.0) << ">>" << endl;
            scriptCount++;
            it.next();
            ScriptParser parser(args[0]);
            QString script = it.value();
            script.remove(QRegExp("\\.script$"));

            if (QFileInfo(standardScriptsDir.path() + "/" + script + ".script").exists()) {
                qDebug() << ":==== Executing in " << args[0] << " :  " << script;
                parser.parse(standardScriptsDir.path() + "/" + script + ".script", args[0] + "/proc/" + script + ".com");
                parser.execute(script);
            }
        }

        QMapIterator<int, QString> it2(cScripts);
        while (it2.hasNext()) {
            //cout << "<<@progress: " << int(scriptCount / float(scripts.size() + cScripts.size())*100.0) << ">>" << endl;
            scriptCount++;
            it2.next();
            ScriptParser parser(args[0]);
            QString script = it2.value();
            script.remove(QRegExp("\\.script$"));
            if (QFileInfo(customScriptsDir.path() + "/" + script + ".script").exists()) {
                qDebug() << ":==== Executing in " << args[0] << " : +" << script;
                parser.parse(customScriptsDir.path() + "/" + script + ".script", args[0] + "/proc/" + script + ".com");
                parser.execute(script);
            }
        }
    } else {
        std::cout << cliParser.helpText().toStdString() << std::endl;
        QCoreApplication::exit(1);
    }
    
    return 0;
}
