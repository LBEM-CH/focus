/*
 *  latticeRefineTool.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/10/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef LATTICEREFINETOOL_H
#define LATTICEREFINETOOL_H

#include <QWidget>
#include <QPointF>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QTableWidget>
#include <QPushButton>
#include "confData.h"
#include "fullScreenImage.h"
#include "pointHash.h"

class latticeRefineTool : public QWidget
{
  Q_OBJECT

  public slots:
  void updatePoint(const QPointF &pos);
  void insertPoint();
  void updateTableView();
  void calculateLattice();
  void commitLattice();
  void commitSecondLattice();
  void clearAll();
  void deletePoint();

  bool saveRefinementList(const QString &fileName);
  bool save();

  bool loadRefinementList(const QString &fileName);
  bool load();

  private:
  QTableWidget *peaksTable;
  QPointF currentPoint;
  QLabel *currentPointx,  *currentPointy;
  QLineEdit millerX, millerY;
  QPointF lattice1, lattice2;
  QLabel *l1, *l2;

  QMap<QPoint,QPoint> peakList;

  confData *data;
  fullScreenImage *image;

  QString fileName;

  public:
  latticeRefineTool(confData *conf, fullScreenImage *sourceImage, QWidget *parent = NULL);

  protected:
  void resizeEvent(QResizeEvent *event);

};

#endif
