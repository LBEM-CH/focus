
#ifndef PHASEVIEW_H
#define PHASEVIEW_H
#define _USE_MATH_DEFINES

#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsEllipseItem>
#include <QGraphicsLineItem>
#include <QVBoxLayout>
#include <math.h>

class PhaseView : public QWidget {
    Q_OBJECT

public:
    PhaseView(QWidget *parent = NULL);

public slots:
    void setPhase(float theta);
    void invert(bool value);
    void show(bool view);

private:
    QGraphicsView *colorWheel;
    QGraphicsLineItem *phaseIndicator;
    QGraphicsEllipseItem *colorWheelEllipse;

    QConicalGradient *gradient, *iGradient;

};


#endif
