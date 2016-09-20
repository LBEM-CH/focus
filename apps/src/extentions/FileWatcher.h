#ifndef FILESYSTEMWATCHER_H
#define FILESYSTEMWATCHER_H

#include <QFileSystemWatcher>
#include <QFileInfo>
#include <QSet>
#include <QStringList>

class FileWatcher : public QFileSystemWatcher
{
  Q_OBJECT

  public slots:
  void setupPaths();

  private:
  QString file;

  public:
  FileWatcher(QObject *parent = NULL);
  void setFile(const QString &path);

};

#endif
