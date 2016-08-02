#ifndef _CONFEDITOR_H_
#define _CONFEDITOR_H_

#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QResizeEvent>
#include <QHeaderView>
#include <QWidget>
#include <QLabel>
#include <QTableView>
#include <QPushButton>
#include <confData.h>
#include <confModel.h>
#include <QDebug>

class confEditor : public QWidget
{

Q_OBJECT

public slots:
  void save();

private:
  confData *data;
  QTableView *preferencesTable;

public:
  confEditor(confData *conf, QWidget *parent = NULL);

protected:
void resizeEvent(QResizeEvent *event);

};

#endif

