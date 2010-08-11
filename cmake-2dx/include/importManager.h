#ifndef IMPORTMANAGER_H
#define IMPORTMANAGER_H

#include <QString>
#include <confData.h>

class importManager : public QObject
{
  private:
    confData *data;
    QFile *file;
  public:
    importManager(confData *data, QObject *parent = NULL);
  
};

#endif
