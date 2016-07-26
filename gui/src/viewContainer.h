/*
 *  viewContainer.h
 *  2DX
 *
 *  Created by Bryant Gipson on 3/10/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef VIEWCONTAINER_H
#define VIEWCONTAINER_H

#include <QWidget>
#include <QPalette>
#include <QFrame>
#include <QScrollArea>
#include <QSplitter>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QLinearGradient>
#include <QList>
#include <QMap>
#include <QLabel>
#include <QSpacerItem>

class viewContainer : public QFrame
{
  Q_OBJECT

  public:
  enum styleType {grey, black};

  public slots:
  void shade();
  void setText(const QString &text);
  void saveSplitterState(int state = 0);
  void restoreSplitterState(int state = 0);
  void showAll();

  void resizeSplitter();

  signals:
  void doubleClicked();

  public:
  enum type {control, image, data};

  private:
  class titleBar : public QWidget
  {
    private:
    QGridLayout *layout;
    QLabel *titleText;
    int headerWidgets;

    public:
    titleBar(QString title, viewContainer::type viewType, QWidget *parent = NULL, viewContainer::styleType gradientType = viewContainer::grey);
    void setTitle(const QString &title);
    void setWidget(QWidget *widget, Qt::Alignment align = Qt::AlignLeft);
  };

  QVBoxLayout *layout;
  titleBar *title;

  QSplitter *containerSplitter;
  QMap<int,QByteArray> splitterState;

  int maximizedWindow;

  void initialize(QString titleText, viewContainer::type viewType, viewContainer::styleType gradientStyle = viewContainer::grey);

  public:
  viewContainer(QString title, type, QWidget *parent=NULL, viewContainer::styleType gradientStyle = viewContainer::grey);
  viewContainer(QString title = "", QWidget *parent = NULL, viewContainer::styleType gradientStyle = viewContainer::grey);
  void setHeaderWidget(QWidget *widget, Qt::Alignment align = Qt::AlignLeft);
  void setHeaderToolTip(const QString &toolTip);
  void addWidget(QWidget *widget, bool addToSplitter = false);
  void addSplitterWidget(QWidget *widget);
  void addScrollWidget(QWidget *widget, bool addToSplitter = false);


  void maximizeWindow(int index);


  protected:
  void mouseDoubleClickEvent(QMouseEvent *event);
};

#endif

