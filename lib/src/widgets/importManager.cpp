#include <importManager.h>

importManager::importManager(confData *projectData, QObject *parent)
              :QObject(parent)
{
  data = projectData;
  file = new QFile(data->getDir("working") + "/config/" + "importList.lst");
  if(!file->exists())
    file->open(QFile::WriteOnly);
  file->close();
}
