#ifndef ZOOMWINDOW_H
#define ZOOMWINDOW_H

#include <QWidget>
#include <QMouseEvent>
#include <QGridLayout>
#include <QLabel>
#include <QPixmap>
#include <QPoint>
#include <QPainter>
#include <QDebug>
#include <math.h>

class ZoomWindow : public QWidget {
    Q_OBJECT

public:
    ZoomWindow(QWidget *imageWidget, QWidget *parent = NULL);

public slots:
    void zoom(const QPoint &pos);
    void zoom();

signals:
    void zoomClick(const QPoint &pos);
    void zoomDoubleClick(const QPoint &pos);
    void zoomMove(const QPoint &pos);

protected:
    void mousePressEvent(QMouseEvent *event);
    void mouseDoubleClickEvent(QMouseEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
    void paintEvent(QPaintEvent *event);
    void resizeEvent(QResizeEvent *event);

private:
    QWidget *widget;
    QPixmap image;
    QLabel *display;
    QWidget *p;

    int zoomX, zoomY;
    QPoint zoomPos;
    QPoint currentPosition(QMouseEvent *event);
};

#endif
