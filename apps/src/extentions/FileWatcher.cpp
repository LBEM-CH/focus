#include <FileWatcher.h>

FileWatcher::FileWatcher(QObject *parent)
						:QFileSystemWatcher(parent)
{
  connect(this,SIGNAL(directoryChanged(const QString &)),this,SLOT(setupPaths()));
  connect(this,SIGNAL(fileChanged(const QString &)),this,SLOT(setupPaths()));
}

void FileWatcher::setFile(const QString &path)
{
  file = path;
  QStringList remove;
  remove<<directories()<<files(); 
  if(!remove.isEmpty()) removePaths(remove);
  addPath(QFileInfo(file).path());
  setupPaths();
}

void FileWatcher::setupPaths()
{
  if(QFileInfo(file).exists() && !files().contains(file))
  {
    if(!files().isEmpty()) removePaths(files());
    addPath(file);
    //emit fileChanged(file);
  }
}


