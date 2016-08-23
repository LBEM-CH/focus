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

#include "confData.h"
#include "scriptParser.h"

using namespace std;

confData* getConf(const QString &workingDirName) {

    QString appDir = QApplication::applicationDirPath();
    QString sep = "/../";
#ifdef Q_OS_MAC
  appDir+="/../../../";
#endif
    int tries = 0;
    while (!QFileInfo(appDir + sep + "resources/config/2dx_master.cfg").exists() && tries < 3) {
        cout << (appDir + sep + "resources/config/2dx_master.cfg").toStdString() << " does not exist!" << endl;
        sep += "../";
        tries++;
    }
    
    QDir workingDir(workingDirName);
    if (!workingDir.exists()) {
        qDebug() << (workingDirName + " does not exist!");
        exit(0);
    }
    workingDir.mkdir("proc");
    workingDir.mkdir("LOGS");
    
    QString referenceConf = appDir + sep + "resources/config/2dx_master.cfg";
    if (!QFileInfo(referenceConf).exists()) {
        qDebug() << (referenceConf + " does not exist!");
        exit(0);
    }
    
    QString mergeConfigLocation = workingDirName + "/../" + "2dx_master.cfg";
    if (!QFileInfo(mergeConfigLocation).exists()) {
        qDebug() << (mergeConfigLocation + " does not exist!");
        exit(0);
    }
    
    confData* mergeData = new confData(mergeConfigLocation, referenceConf);
    mergeData->updateConf(referenceConf);
        
    QString localConf = workingDir.absolutePath() + "/" + "2dx_image.cfg";   
    if (!QFileInfo(localConf).exists()) {
        qDebug() << (localConf + " does not exist!");
        exit(0);
    }

    confData* imageData = new confData(localConf, mergeData);
    if (QFileInfo(referenceConf).exists()) {
        imageData->updateConf(referenceConf);
    }
        
    QDir standardScriptsDir(appDir +  sep + "/scripts/image/standard/");
    QDir customScriptsDir(appDir + sep + "/scripts/image/custom/");

    if (!standardScriptsDir.exists()) {
        cout << standardScriptsDir.absolutePath().toStdString() << " does not exist!" << endl;
        exit(0);
    }
    if (!customScriptsDir.exists()) {
        cout << standardScriptsDir.absolutePath().toStdString() << " does not exist!" << endl;
        exit(0);
    }
    
    QString userPath = QDir::homePath() + "/.2dx";

    imageData->setDir("home_2dx", userPath);
    imageData->setDir("application", appDir + sep);
    imageData->setDir("working", workingDir);
    imageData->setDir("binDir", appDir + sep + "/kernel/mrc/bin");
    imageData->setDir("procDir", appDir + sep + "/scripts/proc");
    imageData->setDir("config", appDir + sep + "/resources/config");
    imageData->setDir("standardScriptsDir", standardScriptsDir);
    imageData->setDir("customScriptsDir", customScriptsDir);
    imageData->setApp("2dx_image", appDir + sep + "/bin/2dx_image");
    imageData->setApp("2dx_merge", appDir + sep + "/bin/2dx_merge");
    
    confData *cfg = new confData(userPath + "/2dx.cfg", imageData->getDir("config") + "/" + "2dx.cfg", true);
    if (cfg->isEmpty()) {
        cerr << "2dx.cfg not found." << endl;
        exit(0);
    }
    cfg->save();

    imageData->setAppConf(cfg);
    
    return imageData;
}

int main(int argc, char *argv[]) {
    QCoreApplication app(argc, argv);
    QCoreApplication::setApplicationName("2dx_image");
    QCoreApplication::setOrganizationName("C-CINA");
    QCoreApplication::setOrganizationDomain("c-cina.org");

    QCommandLineParser cliParser;
    cliParser.setApplicationDescription("2DX Software Suite: 2dx_image Command Line Version\n(If you were looking for GUI, try 2dx_gui instead.)");
    cliParser.addHelpOption();
    cliParser.addVersionOption();
    cliParser.addPositionalArgument("image_dir", "Path of the 2dx Image to be opened.");
    cliParser.addPositionalArgument("scripts_to_run", "Comma separated list of scripts to run. Add '+' before the name if it is a custom script");
    cliParser.process(app);

    const QStringList args = cliParser.positionalArguments();
    if (args.size() == 2) {       
        confData data = *(getConf(args[0]));
        QDir standardScriptsDir(data.getDir("standardScriptsDir"));
        QDir customScriptsDir(data.getDir("customScriptsDir"));
        
        std::cout << "Application Directory: " << data.getDir("application").toStdString() << "\n";

        QMap<int, QString> scripts;
        QMap<int, QString> cScripts;

        QStringList searchScripts = QString(args[1]).remove('"').trimmed().split(',');
        if (searchScripts.isEmpty()) searchScripts << QString(args[1]).remove('"').trimmed();

        QStringList sSearchScripts, cSearchScripts;
        for (int i = 0; i < searchScripts.size(); i++) {
            if (searchScripts[i].trimmed().toLower().startsWith('+')) {
                QString script = searchScripts[i].trimmed() + ".script";
                script.remove(0, 1);
                if(!QFileInfo(data.getDir("customScriptsDir") + "/" + script).exists()) {
                    std::cerr << "WARNING: Custom script: " << script.toStdString() << " does not exist in: \n\t" << data.getDir("customScriptsDir").toStdString() << "\n";
                }
                cSearchScripts << script;
            } else {
                QString script = searchScripts[i].trimmed() + ".script";
                if(!QFileInfo(data.getDir("standardScriptsDir") + "/" + script).exists()) {
                    std::cerr << "WARNING: Standard script: " << script.toStdString() << " does not exist in: \n\t" << data.getDir("standardScriptsDir").toStdString() << "\n";
                }
                sSearchScripts << script;
            }
        }

        QMap<int, confData*> localConfList;
        QMap<int, confData*> localCConfList;
        int sortOrder;

        if (!sSearchScripts.isEmpty()) {
            foreach(QString entry, standardScriptsDir.entryList(sSearchScripts, QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted)) {
                confData *local = new confData(data.getDir("standardScriptsDir") + "/" + entry);
                //local->syncWithUpper();
                sortOrder = local->property("sortOrder").toInt();
                scripts.insert(sortOrder, entry);
                localConfList.insert(sortOrder, local);
            }
        }
        
        QStringList entryList = customScriptsDir.entryList(cSearchScripts, QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted);
        QString entry;

        if (!cSearchScripts.isEmpty())
            for (int i = 0; i < entryList.size(); i++) {
                entry = entryList[i];
                confData *local = new confData(customScriptsDir.absolutePath() + "/" + entry);
                //local->syncWithUpper();
                cScripts.insert(i, entry);
                localCConfList.insert(i, local);
            }

        int scriptCount = 0;

        QMapIterator<int, QString> it(scripts);
        while (it.hasNext()) {
            //cout << "<<@progress: " << int(scriptCount / float(scripts.size() + cScripts.size())*100.0) << ">>" << endl;
            scriptCount++;
            it.next();
            scriptParser parser(QList<confData *>() << localConfList[it.key()] << &data);
            QString script = it.value();
            script.remove(QRegExp("\\.script$"));

            if (QFileInfo(standardScriptsDir.path() + "/" + script + ".script").exists()) {
                cout << ":==== Executing in " << data.getDir("working").toStdString() << " :  " << script.toStdString() << endl;
                parser.parse(standardScriptsDir.path() + "/" + script + ".script", data.getDir("working") + "/proc/" + script + ".com");
                parser.execute(script, &data);
            }
        }

        QMapIterator<int, QString> it2(cScripts);
        while (it2.hasNext()) {
            //cout << "<<@progress: " << int(scriptCount / float(scripts.size() + cScripts.size())*100.0) << ">>" << endl;
            scriptCount++;
            it2.next();
            scriptParser parser(QList<confData *>() << localCConfList[it2.key()] << &data);
            QString script = it2.value();
            script.remove(QRegExp("\\.script$"));
            if (QFileInfo(customScriptsDir.path() + "/" + script + ".script").exists()) {
                cout << ":==== Executing in " << data.getDir("working").toStdString() << " : +" << script.toStdString() << endl;
                parser.parse(customScriptsDir.path() + "/" + script + ".script", data.getDir("working") + "/proc/" + script + ".com");
                parser.execute(script, &data);
            }
        }

        // Save Database
        data.save();
    } else {
        std::cout << cliParser.helpText().toStdString() << "\n";
        exit(0);
    }
}
