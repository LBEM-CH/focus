#include <QApplication>
#include <QFileDialog>
#include <QDir>
#include <QMessageBox>
#include <QDebug>
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


int main(int argc, char **argv)
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
 
  
  QString workingDir = QFileDialog::getExistingDirectory(NULL,"Select a Project Directory",userDirPath);
  if(workingDir.isEmpty() || !QDir(workingDir).exists()) exit(0);
  //save the selected dir as working dir in 2dx_merg-user.cfg
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
  win->resize(1024,740);

  return app.exec();
}
