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

#include "ParameterConfiguration.h"
#include "FullScreenImage.h"
#include "PointHash.h"

class LatticeRefineTool : public QWidget {
    Q_OBJECT

public:
    LatticeRefineTool(const QString& workingDir, FullScreenImage *sourceImage, QWidget *parent = NULL);

protected:
    void resizeEvent(QResizeEvent *event);

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
    QLabel *currentPointx, *currentPointy;
    QLineEdit millerX, millerY;
    QPointF lattice1, lattice2;
    QLabel *l1, *l2;

    QMap<QPoint, QPoint> peakList;

    FullScreenImage *image;
    QString workingDir;
    
    QString fileName;

};

#endif
