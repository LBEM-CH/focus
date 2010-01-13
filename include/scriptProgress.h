//
// C++ Interface: progressBar
//
// Description:
//
//
// Author: root <root@localhost>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef SCRIPTPROGRESS_H
#define SCRIPTPROGRESS_H

#include <QProgressBar>
#include <QPainter>
#include <QString>

class scriptProgress : public QProgressBar
{
  Q_OBJECT
  public:
  enum barType { aqua, silver, overlay, ticks, none };

  public slots:
  void setText(QString text);
  void setProgressType(scriptProgress::barType type);
  void incrementValue(int inc);

  private:
  barType progressType;
  QString title;

  public:
  scriptProgress(QWidget *parent = 0);
  ~scriptProgress();

  protected:
  void paintEvent(QPaintEvent *event);

};

#endif
