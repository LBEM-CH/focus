
#ifndef PHASEVIEW_H
#define PHASEVIEW_H

#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsEllipseItem>
#include <QGraphicsLineItem>
#include <QVBoxLayout>
#include <math.h>

class phaseView : public QWidget
{
  Q_OBJECT

  public slots:
  void setPhase(float theta);
  void invert(bool value);
  void show(bool view);

  private:
  QGraphicsView *colorWheel;
  QGraphicsLineItem *phaseIndicator;
  QGraphicsEllipseItem *colorWheelEllipse;

  QConicalGradient *gradient, *iGradient;

  public:
  phaseView(QWidget *parent = NULL);

};


#endif
