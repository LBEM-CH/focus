/***************************************************************************
 *   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
 *   HStahlberg@ucdavis.edu                                                *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include "ProjectData.h"

#include "LatticeRefineTool.h"

LatticeRefineTool::LatticeRefineTool(const QString& workDir, FullScreenImage *sourceImage, QWidget *parent)
: QWidget(parent, Qt::Tool | Qt::WindowStaysOnTopHint) {
    workingDir = workDir;
    image = sourceImage;
    fileName = workingDir + "/" + "latticeRefinementList.dat";

    QGridLayout *layout = new QGridLayout(this);

    QGridLayout *subLayout = new QGridLayout;
    subLayout->setMargin(0);
    subLayout->addWidget(new QLabel("Lattice Refinement:"), 0, 0, 1, 1);
    l1 = new QLabel("<----,---->");
    l2 = new QLabel("<----,---->");
    subLayout->addWidget(l1, 0, 1, 1, 1);
    subLayout->addWidget(l2, 0, 2, 1, 1);
    subLayout->setAlignment(l1, Qt::AlignLeft);
    subLayout->setAlignment(l2, Qt::AlignLeft);
    QPushButton *deletePointButton = new QPushButton("Delete Point");
    QPushButton *clear = new QPushButton("Clear All");
    QHBoxLayout *subLayout2 = new QHBoxLayout;
    subLayout2->addWidget(deletePointButton);
    subLayout2->addStretch(2);
    subLayout2->addWidget(clear);
    subLayout->setAlignment(clear, Qt::AlignRight);
    //  subLayout->addLayout(subLayout2,1,0,1,3);
    layout->addLayout(subLayout, 0, 0, 1, 3);

    peaksTable = new QTableWidget(this);
    peaksTable->setColumnCount(4);
    peaksTable->setHorizontalHeaderLabels(QStringList() << "i" << "j" << "x" << "y");
    peaksTable->setSelectionBehavior(QAbstractItemView::SelectRows);
    peaksTable->setSelectionMode(QAbstractItemView::SingleSelection);
    layout->addWidget(peaksTable, 1, 0, 1, 3);

    currentPointx = new QLabel("--");
    currentPointy = new QLabel("--");
    layout->addWidget(new QLabel("Current Point: "), 2, 0, 1, 1);
    layout->addWidget(currentPointx, 2, 1, 1, 1);
    layout->addWidget(currentPointy, 2, 2, 1, 1);

    layout->addWidget(new QLabel("Miller Index: "), 3, 0, 1, 1);
    layout->addWidget(&millerX, 3, 1, 1, 1);
    layout->addWidget(&millerY, 3, 2, 1, 1);

    QPushButton *addPoint = new QPushButton("Add Point");
    layout->addWidget(addPoint, 4, 0, 1, 1);

    QPushButton *commit_first = new QPushButton("Accept First Lattice");
    layout->addWidget(commit_first, 4, 1, 1, 1);

    QPushButton *commit_second = new QPushButton("Accept Second Lattice");
    layout->addWidget(commit_second, 4, 2, 1, 1);
    layout->addLayout(subLayout2, 5, 0, 1, 3);

#ifdef Q_OS_MAC
    QFontMetrics metric(font());
    commit_first->setMinimumWidth(metric.width(commit_first->text()) + 60);
    commit_second->setMinimumWidth(metric.width(commit_second->text()) + 60);
    deletePointButton->setMinimumWidth(metric.width(deletePointButton->text()) + 60);
    clear->setMinimumWidth(metric.width(clear->text()) + 60);

#endif

    connect(addPoint, SIGNAL(clicked()), this, SLOT(insertPoint()));
    connect(commit_first, SIGNAL(clicked()), this, SLOT(commitLattice()));
    connect(commit_second, SIGNAL(clicked()), this, SLOT(commitSecondLattice()));
    connect(clear, SIGNAL(clicked()), this, SLOT(clearAll()));
    connect(deletePointButton, SIGNAL(clicked()), this, SLOT(deletePoint()));

    setLayout(layout);

    image->setLatticeRefinementList(peakList);
    image->setRefinementCandidate(currentPoint);
    updateTableView();

    load();
}

void LatticeRefineTool::updatePoint(const QPointF &pos) {
    currentPoint = pos;
    float i = currentPoint.x(), j = currentPoint.y();
    if (i < 0.0) i -= 0.5;
    else i += 0.5;
    if (j < 0.0) j -= 0.5;
    else j += 0.5;
    currentPoint.setX(i);
    currentPoint.setY(j);
    currentPointx->setText(QString::number((int) i));
    currentPointy->setText(QString::number((int) j));

    QStringList cell = projectData.parameterData(QDir(workingDir))->get("lattice")->value().toString().split(',');
    float lattice[2][2], inv[2][2];

    lattice[0][0] = cell[0].toFloat();
    lattice[1][0] = cell[1].toFloat();

    lattice[0][1] = cell[2].toFloat();
    lattice[1][1] = cell[3].toFloat();

    float det = lattice[0][0] * lattice[1][1] - lattice[0][1] * lattice[1][0];

    if (det != 0) {
        inv[0][0] = lattice[1][1] / det;
        inv[1][1] = lattice[0][0] / det;
        inv[0][1] = -lattice[0][1] / det;
        inv[1][0] = -lattice[1][0] / det;
    } else {
        inv[0][0] = 0;
        inv[0][1] = 0;
        inv[1][0] = 0;
        inv[1][1] = 0;
    }

    float x = ((float) (currentPoint.x()) * inv[0][0] + (float) (currentPoint.y()) * inv[0][1]);
    float y = ((float) (currentPoint.x()) * inv[1][0] + (float) (currentPoint.y()) * inv[1][1]);
    if (x < 0.0) x -= 0.5;
    else x += 0.5;
    if (y < 0.0) y -= 0.5;
    else y += 0.5;

    millerX.setText(QString::number((int) x));
    millerY.setText(QString::number((int) y));
    image->update();
}

void LatticeRefineTool::insertPoint() {
    QPoint millerIndex(millerX.text().toInt(), millerY.text().toInt());
    QPoint pInt(currentPoint.x(), currentPoint.y());
    peakList[millerIndex] = pInt;
    millerX.clear();
    millerY.clear();
    updateTableView();
    calculateLattice();
    image->update();
    save();
}

void LatticeRefineTool::updateTableView() {
    peaksTable->clear();
    peaksTable->setHorizontalHeaderLabels(QStringList() << "i" << "j" << "x" << "y");
    peaksTable->setRowCount(peakList.size());

    QMapIterator<QPoint, QPoint> i(peakList);
    int k = 0;
    while (i.hasNext()) {
        i.next();
        QTableWidgetItem * a[4] = {new QTableWidgetItem(QString::number(i.key().x())), new QTableWidgetItem(QString::number(i.key().y())),
            new QTableWidgetItem(QString::number(i.value().x())), new QTableWidgetItem(QString::number(i.value().y()))};

        for (int j = 0; j < 4; j++) {
            a[j]->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
            peaksTable->setItem(k, j, a[j]);
            peaksTable->setColumnWidth(j, peaksTable->maximumViewportSize().width() / 4);
        }
        k++;
    }
}

void LatticeRefineTool::calculateLattice() {
    double smx2 = 0.0, smy2 = 0.0, smxmy = 0.0, smxrx = 0.0, smyrx = 0.0, smxry = 0.0, smyry = 0.0;
    int k = 0;
    QMapIterator<QPoint, QPoint> i(peakList);
    while (i.hasNext()) {
        i.next();
        double mx = double(i.key().x()), my = double(i.key().y()), rx = double(i.value().x()), ry = double(i.value().y());
        smx2 += mx*mx;
        smy2 += my*my;
        smxmy += mx*my;
        smxrx += mx*rx;
        smyrx += my*rx;
        smxry += mx*ry;
        smyry += my*ry;
        k++;
    }

    double det = smx2 * smy2 - smxmy*smxmy;
    if (det != 0.0) {
        /*    lattice1.setX((smy2*smxrx-smxmy*smyrx)/det);
            lattice1.setY((smy2*smxry-smxmy*smyry)/det);

            lattice2.setX((-smxmy*smxrx+smx2*smyrx)/det);
            lattice2.setY((-smxmy*smxry+smx2*smyry)/det);
         */
        double c1 = (smx2 * smyrx - smxmy * smxrx) / det;
        double c2 = (smx2 * smyry - smxmy * smxry) / det;

        lattice1.setX(smxrx / smx2 - c1 * smxmy / smx2);
        lattice1.setY(smxry / smx2 - c2 * smxmy / smx2);

        lattice2.setX(c1);
        lattice2.setY(c2);

        l1->setText("<" + QString::number(lattice1.x()) + ", " + QString::number(lattice1.y()) + ">");
        l2->setText("<" + QString::number(lattice2.x()) + ", " + QString::number(lattice2.y()) + ">");
    } else {
        lattice1.setX(0.0);
        lattice1.setY(0.0);
        lattice2.setX(0.0);
        lattice2.setY(0.0);
        l1->setText("<----,---->");
        l2->setText("<----,---->");
    }

}

void LatticeRefineTool::commitLattice() {
    ParametersConfiguration* data = projectData.parameterData(QDir(workingDir));
    data->set("lattice", QString::number(lattice1.x()) + "," + QString::number(lattice1.y()) + "," + QString::number(lattice2.x()) + "," + QString::number(lattice2.y()));
    data->setModified(true);
    data->set("LATTICE_done", "y");
    data->set("SPOTS_done", "n");
    data->set("UNBENDING_done", "n");
    data->set("CTF_done", "n");
    image->update();
}

void LatticeRefineTool::commitSecondLattice() {
    ParametersConfiguration* data = projectData.parameterData(QDir(workingDir));
    data->set("secondlattice", QString::number(lattice1.x()) + "," + QString::number(lattice1.y()) + "," + QString::number(lattice2.x()) + "," + QString::number(lattice2.y()));
    data->setModified(true);
    image->update();
}

void LatticeRefineTool::clearAll() {
    peakList.clear();
    updateTableView();
    lattice1.setX(0.0);
    lattice1.setY(0.0);
    lattice2.setX(0.0);
    lattice2.setY(0.0);
    l1->setText("<----,---->");
    l2->setText("<----,---->");
    image->update();
    save();
}

void LatticeRefineTool::resizeEvent(QResizeEvent *) {
    updateTableView();
}

void LatticeRefineTool::deletePoint() {
    if (peaksTable->currentRow() != -1) {
        QMapIterator<QPoint, QPoint> i(peakList);
        int k = 0;

        while (i.hasNext() && k <= peaksTable->currentRow()) {
            i.next();
            if (k == peaksTable->currentRow()) peakList.remove(i.key());
            k++;
        }
        updateTableView();
        calculateLattice();
        image->update();
    }
    save();
}

bool LatticeRefineTool::saveRefinementList(const QString &fileName) {
    QFile list(fileName);
    if (!list.open(QIODevice::WriteOnly | QIODevice::Text)) return false;
    QMapIterator<QPoint, QPoint> i(peakList);
    while (i.hasNext()) {
        i.next();
        list.write(QString(QString::number(i.key().x()) + ',' + QString::number(i.key().y()) + ',' + QString::number(i.value().x()) + ',' + QString::number(i.value().y()) + '\n').toLatin1());
    }
    list.close();
    return true;
}

bool LatticeRefineTool::save() {
    return saveRefinementList(fileName);
}

bool LatticeRefineTool::loadRefinementList(const QString &fileName) {
    peakList.clear();
    QFile list(fileName);
    if (!list.open(QIODevice::ReadOnly | QIODevice::Text)) return false;

    QString line;
    QStringList values;
    QPoint m, v;
    while (!list.atEnd()) {
        line = list.readLine();
        values = line.split(',');
        m = QPoint(values[0].toInt(), values[1].toInt());
        v = QPoint(values[2].toInt(), values[3].toInt());
        peakList[m] = v;
    }

    updateTableView();
    calculateLattice();
    image->update();

    return true;
}

bool LatticeRefineTool::load() {
    return loadRefinementList(fileName);
}

