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

bool backupSelection(const QString selectionFileName)
{
    std::cout << "backing up " << selectionFileName.toStdString() <<std::endl;
    bool status = QFile::rename(selectionFileName, selectionFileName+".bak");
    if(!status)
         std::cerr << "moving " << selectionFileName.toStdString() << " to " << selectionFileName.toStdString() << ".bak failed \n";
     return status;
}

bool restoreSelection(const QString selectionFileName)
{
    return QFile::rename(selectionFileName+".bak", selectionFileName);
}

bool backupDefaultSelection(QDir mergeDir)
{
    QString defaultSelection(mergeDir.absolutePath()+"/2dx_merge_dirfile.dat");
    if(!QFileInfo(defaultSelection).exists())
        std::cerr << defaultSelection.toStdString() << " does not exist.\n";
    return backupSelection(defaultSelection);
}
bool restoreDefaultSelection(QDir mergeDir)
{
    QString defaultSelection(mergeDir.absolutePath()+"/2dx_merge_dirfile.dat");
    bool status = QFile::remove(defaultSelection);
    return restoreSelection(defaultSelection) && status;
}

bool writeSelection2File(QList<QString> selection, const QString fileName, QString projectPath)
{
    std::cout << "project-path " << projectPath.toStdString() << std::endl;
    QFile file(fileName);
    if (file.open(QFile::WriteOnly | QFile::Text))
    {
      QTextStream s(&file);
      for (int i = 0; i < selection.size(); ++i)
      {
          QString image = selection.at(i);
          if(QFileInfo(projectPath+image).exists())
            s << image << '\n';
          else
          {
              std::cerr << "file " << image.toStdString() <<" does not exist." <<std::endl ;
              return false;
          }
      }
    }
    else
    {
      std::cerr << "error opening selection file\n";
      return false;
    }
    file.close();
    return true;
}

bool setSelection(QList<QString> selection, QDir mergeDir)
{
    //QFile selectionFile;
    QString defaultSelection(mergeDir.absolutePath()+"/2dx_merge_dirfile.dat");
    if(QFileInfo(defaultSelection).exists())
    {
        if(!backupDefaultSelection(mergeDir))
        {
            std::cerr<< "error backing up selection " <<defaultSelection.toStdString() <<std::endl;
            return false;
        }
    }

    QString selectionString = selection[0];
    selectionString = selectionString.remove('"').trimmed();
    if(selectionString.endsWith(".dat"))
    {
        QString selectionPath(mergeDir.absolutePath()+"/"+selectionString);
        if(QFileInfo(defaultSelection).exists())
            std::cerr<< "default selection does still exist: " <<defaultSelection.toStdString() <<std::endl;

        if(QFileInfo(selectionPath).exists())
        {
            bool status = QFile::rename(selectionPath,defaultSelection);
            //TODO: check the status, but right now it seems to retunr false, even when the rename works
            if(!status)
            {
                std::cerr<< "error moving " <<selectionPath.toStdString()  << " to " << defaultSelection.toStdString()<<std::endl;
                return false;
            }
        }
        else
        {
            std::cerr<< "specified selection file " <<selectionPath.toStdString()  << " does not exist." <<std::endl;
            return false;
        }
    }
    else
    {
        QString projectPath = mergeDir.absoluteFilePath("../");
        return writeSelection2File(selection, defaultSelection, projectPath);
    }
    return true;
}

bool restoreSelections(QList<QString> selection, QDir mergeDir)
{
    QString defaultSelection(mergeDir.absolutePath()+"/2dx_merge_dirfile.dat");
    QString selectionString = selection[0];
    selectionString = selectionString.remove('"').trimmed();
    if(selectionString.endsWith(".dat"))
    {
         QString specSelection(mergeDir.absolutePath()+"/"+selectionString);
         if(!QFile::rename(defaultSelection, specSelection))
         {
             std::cerr<< "error moving " <<defaultSelection.toStdString()  << " to " << specSelection.toStdString()<<std::endl;
             return false;
         }
    }
    return restoreDefaultSelection(mergeDir);
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
    confData* scriptConf;
    QString scriptPath = scriptsDir +"/"+scriptName;
    scriptName.remove(QRegExp("\\.script$"));
    if(QFileInfo(scriptPath).exists())
    {
        scriptConf = new confData(scriptPath);
    }
    scriptParser parser(QList<confData *>()<< scriptConf<<config);
    std::cout<<"::  Executing in "<<config->getDir("working").toStdString()<<" : "<<scriptName.toStdString()<<std::endl;
    parser.parse(scriptPath, config->getDir("working") + "/proc/" + scriptName + ".com");
    parser.execute(scriptName,config);

    if(scriptConf) delete scriptConf;
}


void commandLineMerge(const QString appDir, const QString workingDir, const QStringList fileSelection, const QString script)
{
    confData* config;
    config = readConfig(workingDir, appDir);
    QDir mergeDir = config->getDir("working");
    if(!setSelection(fileSelection, mergeDir))
    {
        restoreDefaultSelection(mergeDir);
        return;
    }
    QDir standardScriptsDir(appDir + "/2dx_merge/" + "scripts-standard/");
    QDir customScriptsDir(appDir + "/2dx_merge/" + "scripts-custom/");
    QDir scriptsDir;
    bool isCustomScript = false;
    //TODO: allow more then just one script
    QString strippedScript = QString(script).remove('"').trimmed();
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
    restoreSelections(fileSelection, mergeDir);
    delete config;
}



int main(int argc, char **argv)
{
    if( argc >= 3 )
    {
        std::cout << "command line version of 2dx_merge" << std::endl;
        QCoreApplication app(argc, argv);
        QCoreApplication::setApplicationName("2dx_merge");
        QStringList arguments = QCoreApplication::arguments();
        QString applicationDir = getAppDir();
        std::cout << "Application dir: " << applicationDir.toStdString() <<std::endl;
        QString workingDir = arguments[1];
        std::cout << "working dir: " << workingDir.toStdString() <<std::endl;
        QString script = arguments[2];
        QList<QString> fileSelection;
        if (arguments.size() > 3)
        {
            //remove first 2 elements
            for(int i = 0; i < 3;++i)
                arguments.pop_front();
            fileSelection = arguments;
        }
        else
        {
            std::cout << "Warning! No selection specified." <<std::endl;
            std::cout << "  taking 2dx_merge_dirfile.dat" <<std::endl;
            fileSelection << "2dx_merge_dirfile.dat";
        }
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
