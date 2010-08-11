#ifndef AUTOIMPORTTOOL_H
#define AUTOIMPORTTOOL_H

#include <QWidget>
#include <QFileInfo>
#include <QGridLayout>
#include <QLineEdit>
#include <QFrame>
#include <QLabel>
#include <QPushButton>
#include <QListWidget>
#include <confData.h>
#include <importManager.h>
#include <QDebug>

class autoImportTool : public QWidget
{
  public slots:
    void update();
  private:
    QLineEdit *directoryName;
    QLabel *console;
    QListWidget *fileList;
    confData *data;
    importManager *importList;
  public:
    autoImportTool(confData *data, const QString &dir,QWidget *parent = NULL);
    
};

#endif
