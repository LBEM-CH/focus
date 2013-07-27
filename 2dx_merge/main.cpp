#include <QApplication>
#include <QFileDialog>
#include <QDir>
#include <QMessageBox>
#include <QDebug>
#include <QProcess>
#include <scriptParser.h>
#include "mainWindow.h"

QString getAppDir()
{
  QString appDir = QApplication::applicationDirPath();
  QString sep = "/../";
  #ifdef Q_WS_MAC
    appDir+="/../../../";
  #endif
  int tries = 0;
  while(!QFileInfo(appDir + sep + "config/2dx_master.cfg").exists() && tries<3)
  {
    qDebug()<<(appDir + sep + "config/2dx_master.cfg")<<" does not exist!";
    sep+="../";
    tries++;
  }
  if(QFileInfo(appDir + sep + "config/2dx_master.cfg").exists())
  {
    return QString(appDir + sep);
  }
  else
    return QString();
  
}
void initializeProject(const QString &appDir, const QString &workingDir)
{
 
  QDir dir(workingDir);
  dir.mkdir("merge");
  dir.mkpath("merge/proc");
  dir.mkpath("merge/LOGS");
  confData data(workingDir + "/merge/" + "2dx_merge.cfg", appDir + "config/2dx_master.cfg");
  data.setSymLink("merge/2dx_merge.cfg", workingDir +"/2dx_master.cfg");
  data.save();
}

confData* readConfig(const QString workingPath, const QString applicationPath)
{
    confData* data;
    QString mergeConfigLocation = workingPath + "/merge/" + "2dx_merge.cfg";
    QString appConfigLocation = applicationPath + "config" + "/" + "2dx_master.cfg";
    if(!QFileInfo(mergeConfigLocation).exists())
    {
        std::cerr << "Config file " +mergeConfigLocation.toStdString() << " does not exist!";
        std::cerr << "Please initialize the project via the graphical user interface of 2dx_merge.";
        return 0;
    }
    data = new confData(mergeConfigLocation, appConfigLocation);
    std::cout << "set config defaults." << std::endl;
    data->setDefaults(workingPath);
    if(QFileInfo(appConfigLocation).exists())
    {
        data->updateConf(appConfigLocation);
    }

    data->setDir("project",QDir(workingPath));
    data->setDir("working",QDir(workingPath + "/merge"));
    data->save();
    return data;
}

bool scriptExists(QString scriptName, QDir scriptsDir)
{
    QStringList scriptList = scriptsDir.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted);
    return scriptList.contains(scriptName, Qt::CaseInsensitive);
}

void execute(QString scriptName, QString scriptsDir, confData* config)
{
    QProcess process;
    confData* scriptConf;
    QString scriptPath = scriptsDir +"/"+scriptName;
    scriptName.remove(QRegExp("\\.script$"));
    if(QFileInfo(scriptPath).exists())
    {
        scriptConf = new confData(scriptPath);
    }
    scriptParser parser(QList<confData *>()<< scriptConf<<config);
    std::cout<<"::  Executing in "<<config->getDir("working").toStdString()<<" : +"<<scriptName.toStdString()<<std::endl;
    parser.parse(scriptPath, config->getDir("working") + "/proc/" + scriptName + ".com");
    parser.execute(scriptName,config);
//    process.setWorkingDirectory(config->getDir("working"));
//    std::cout<< parser.executionString().toStdString()<<std::endl;
//    process.start('"' + parser.executionString() + '"', QIODevice::ReadOnly);

    if(scriptConf) delete scriptConf;
}


void commandLineMerge(const QString appDir, const QString workingDir, const QString fileSelection, const QString script)
{
    confData* config;
    config = readConfig(workingDir, appDir);
    QDir standardScriptsDir(appDir + "/2dx_merge/" + "scripts-standard/");
    QDir customScriptsDir(appDir + "/2dx_merge/" + "scripts-custom/");
    QDir scriptsDir;
    bool isCustomScript = false;
    //TODO: allow more then just one script
    QString strippedScript = QString(script).remove('"').trimmed();
    std::cout << "stripped script file: "<< strippedScript.toStdString() <<std::endl;
    if(strippedScript.startsWith('+'))
    {
        strippedScript = strippedScript.remove(0,1);
        isCustomScript = true;
    }
    QString scriptFile = strippedScript+".script";
    std::cout << "script file: "<< scriptFile.toStdString() <<std::endl;
    if(isCustomScript)
    {
        std::cout << "custom script!";
        scriptsDir = customScriptsDir;
    }
    else
    {
        scriptsDir = standardScriptsDir;
    }
    if (scriptExists(scriptFile, scriptsDir))
    {
        std::cout << "script exists" <<std::endl;
        execute(scriptFile, scriptsDir.absolutePath(), config);
    }
    else
        std::cerr << "There is no script " <<scriptFile.toStdString() <<std::endl;


    delete config;
}



int main(int argc, char **argv)
{
    if( argc >= 3 )
    {
        std::cout << "command line version of 2dx_merge" << std::endl;
        QCoreApplication app(argc, argv);
        QCoreApplication::setApplicationName("2dx_merge");
        QString applicationDir = getAppDir();
        std::cout << "Application dir: " << applicationDir.toStdString() <<std::endl;
        QString workingDir = argv[1];
        std::cout << "working dir: " << workingDir.toStdString() <<std::endl;
        QString script = argv[2];
        QString fileSelection;
        if (argc > 3)
        {
            fileSelection = QString(argv[3]);
        }
        else
        {
            fileSelection = workingDir + "/merge/2dx_merge_dirfile.dat";

            if(!QFileInfo(fileSelection).exists())
            {
                std::cerr << "The selection file " << fileSelection.toStdString() << " does not exist!" <<std::endl;
                return 1;
            }
        }

        std::cout << "selection file: " << fileSelection.toStdString() <<std::endl;
        commandLineMerge(applicationDir, workingDir, fileSelection, script);
    }
    else
    {
      QApplication app(argc,argv);
      app.setApplicationName("2dx_merge");

      QString appDir = getAppDir();
      if(appDir.isEmpty()) exit(0);

      QString configDir = appDir + "/config";

      // read user config to get the latest image dir
      confData* userData = new confData(QDir::homePath() + "/.2dx/" + "2dx_merge-user.cfg", configDir + "/" + "2dx_merge-user.cfg");
      userData->save();
      QString userDirPath = userData->get("workingDir","value");
      //qDebug() << "The last used working dir is: " << userDirPath;
      if(userDirPath.isEmpty())
          userDirPath = QDir::homePath();


      QString workingDir = QFileDialog::getExistingDirectory(0,"Select a Project Directory",userDirPath);
      if(workingDir.isEmpty() || !QDir(workingDir).exists()) exit(0);
      //save the selected dir as working dir in 2dx_merge-user.cfg
      userData->set("workingDir", workingDir);
      userData->save();

      if(!QFileInfo(workingDir +"/merge/" + "2dx_merge.cfg").exists())
      {
        quint32 choice = QMessageBox::question(NULL,"Confirm Create new Project?","No configuration data found in:\n"+workingDir+"\n\nCreate new config files in this directory?","Create","Cancel",QString(),0,1);
        if(choice) return 0;
        else
        {
          initializeProject(appDir, workingDir);
        }
      }


      mainWindow *win = new mainWindow(workingDir);
      win->show();
      win->raise(); // raises the window on top of the parent widget stack
      win->activateWindow(); // activates the window an thereby putting it on top-level
      win->resize(1024,740);

      return app.exec();
    }
}
