#ifndef MRCGRAPHICSITEM_H
#define MRCGRAPHICSITEM_H

#include <QThread>
#include <QRectF>
#include <QGraphicsItem>
#include <QGraphicsPixmapItem>
#include <QGraphicsView>
#include <QGraphicsScene>
#include <QStyleOptionGraphicsItem>
#include <QMutex>
#include <largeMRC.h>
#include <QDebug>

class mrcGraphicsItem : public QThread, public QGraphicsPixmapItem
{
  Q_OBJECT 

  public slots:
  void loadPixmap();

  signals:
  void load();
  void updated(const QRectF &rect);

  private:
  largeMRC *image;
  int pX,pY;
  int optimalThreads;
  int *threadCount;
  int timer;

  QMutex mutex;

  QPainter *mPainter;
  QStyleOptionGraphicsItem mOption;
  QWidget *mWidget;

  bool pixmapLoaded;

  public:
  mrcGraphicsItem(largeMRC *image, int x, int y, int *threads, QGraphicsItem *parent = NULL);
  ~mrcGraphicsItem();
 
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget* widget = NULL);
 
  protected:
  void run();
  void timerEvent(QTimerEvent *event);

};

#endif 
