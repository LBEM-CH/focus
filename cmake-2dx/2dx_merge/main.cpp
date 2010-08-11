#include <QApplication>
#include <QFileDialog>
#include <QMessageBox>
#include <QDebug>
#include "mainWindow.h"

void initializeProject(const QString &workingDir)
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
  if(!QFileInfo(appDir + sep + "config/2dx_master.cfg").exists()) exit(0);
  
  QDir dir(workingDir);
  dir.mkdir("merge");
  dir.mkpath("merge/proc");
  dir.mkpath("merge/LOGS");
  confData data(workingDir +"/2dx_master.cfg", appDir + sep + "config/2dx_master.cfg");
  data.save();
}

int main(int argc, char **argv)
{
  QApplication app(argc,argv);
  app.setApplicationName("2dx_merge");

  QString workingDir = QFileDialog::getExistingDirectory(NULL,"Select a Project Directory");
  if(workingDir.isEmpty() || !QDir(workingDir).exists()) exit(0);

  if(!QFileInfo(workingDir +"/merge/" + "2dx_merge.cfg").exists())
  {
    quint32 choice = QMessageBox::question(NULL,"Confirm Create new Project?","No configuration data found in:\n"+workingDir+"\n\nCreate new config files in this directory?","Create","Cancel",QString(),0,1);
    if(choice) return 0;
    else
    {
      initializeProject(workingDir);
    }
  }


  mainWindow *win = new mainWindow(workingDir);
  win->show();
  win->resize(1024,740);

  return app.exec();
}
