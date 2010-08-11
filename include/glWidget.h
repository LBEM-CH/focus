#ifndef GLWIDGET_H
#define GLWIDGET_H

#include <QGLWidget>
#include <QDebug>

class glWidget : public QGLWidget
{
  public:
  glWidget(QWidget *parent = NULL, Qt::WindowFlags f = 0);
};

#endif

