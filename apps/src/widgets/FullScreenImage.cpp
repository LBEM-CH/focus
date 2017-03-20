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
#include "PointHash.h"

#include "FullScreenImage.h"
#include "ParameterElementData.h"

const float pi = 3.14159265;

FullScreenImage::FullScreenImage(mrcImage *source_image, QString workDir, QWidget *parent)
: QWidget(parent) {
    QTime timer;
    timer.start();
    workingDir = workDir;
    imageHeader = source_image->getHeader();
    image = source_image->getImage();
    sourceImage = source_image;
    pixmap = source_image->getPixmap();

    qDebug() << "Full Screen Image Initialization: " << timer.elapsed();
    timer.restart();

    QRect screenRect = QApplication::desktop()->screenGeometry(QApplication::desktop()->primaryScreen());
    screenWidth = screenRect.width();
    screenHeight = screenRect.height();

    scale = 1.0;
    float w = 1.0, h = 1.0;
    if (image->width() * scale < screenWidth)
        w = (screenWidth);
    else
        w = ((int) (image->width() * scale));

    if (image->height() * scale < screenHeight)
        h = (screenHeight);
    else
        h = ((int) (image->height() * scale));

    QGridLayout *layout = new QGridLayout;
    layout->addItem(new QSpacerItem(w, h), 0, 0);
    setLayout(layout);
    rescaleWidget();

    overlayVisible = true;
    peakListVisible = false;
    latticeVisible = false;
    secondLatticeVisible = false;
    refineLatticeVisible = false;
    ctfVisible = false;
    resolutionRingVisible = false;
    selectionVisible = false;
    particlesVisible = false;
    
    resolutionRingValue = 3.7;

    ParametersConfiguration* conf = projectData.parameterData(QDir(workingDir));
    peakListFileName = workingDir + '/' + conf->getValue("nonmaskimagename") + ".spt";
    particlesFileName = workingDir + '/' + conf->getValue("movie_stackname") + "_automatch.star";
    selectionListFileName = workingDir + "/selectionList.dat";
    psPeakListFile = workingDir + "/peaks_xy.dat";

    refBoxes << "boxa1" << "boxa2" << "boxb1" << "boxb2";

    saved.insert("peaklist", false);
    saveStrings.insert("peaklist", "Spot list not saved, exit?");
    saveFunctions["peaklist"] = &FullScreenImage::savePeakList;

    loadPeakList();
    if (!loadParticles()) qDebug() << particlesFileName << "does not exits";
    if (!loadPSPeaks()) qDebug() << "peaks_xy.dat does not exist." << endl;
    
    latticeRefineList = NULL;
    refinementCandidate = NULL;

    QStringList cell = conf->getValue("defocus").split(',');
    if(cell.size() > 2) {
        float defocusX = cell[0].toFloat();
        float defocusY = cell[1].toFloat();
        float astigmatism = cell[2].toFloat();
        calculateCTF(defocusX, defocusY, astigmatism);
    }

    setMouseTracking(true);
}

void FullScreenImage::setImage(QImage *source_image) {
    *image = *source_image;
    pixmap = QPixmap::fromImage(*source_image);
    setFixedWidth(source_image->width());
    setFixedHeight(source_image->height());
}

QPointF FullScreenImage::coordinates(const QPoint &i) {
    return QPointF(lattice[0][0]*(float) i.x() + lattice[0][1]*(float) i.y(), lattice[1][0]*(float) i.x() + lattice[1][1]*(float) i.y());
}

int FullScreenImage::loadPeakList() {
    saved["peaklist"] = true;
    int x, y;
    char comma;
    peakList.clear();
    QFile f(peakListFileName);
    if (!f.open(QIODevice::ReadOnly | QIODevice::Text)) return 0;
    QTextStream p(&f);
    int i = 0;

    f.seek(0);
    p >> x >> comma>>y;
    while (!p.atEnd()) {
        peakList << QPoint(x, y);
        p >> x >> comma>>y;
        i++;
    }
    peakNum = i;
    f.close();
    return 1;
}

bool pointOrder(const QPoint &a, const QPoint &b) {
    if (a.x() == b.x()) return a.y() < b.y();
    return a.x() < b.x();
}

int FullScreenImage::savePeakList() {
    saved["peaklist"] = true;
    QFile f(peakListFileName);
    if (!f.open(QIODevice::WriteOnly | QIODevice::Text)) return 0;
    QList<QPoint> spots = peakList.values();
    qSort(spots.begin(), spots.end(), pointOrder);

    foreach(QPoint p, spots) {
        f.write(QString("\t" + QString::number(p.x()) + ",\t" + QString::number(p.y()) + "\n").toLatin1());
    }

    f.close();
    projectData.parameterData(workingDir)->set("SPOTS_done", "y");
    return 1;
}

bool FullScreenImage::loadPSPeaks() {
    double x, y, str;
    QFile peaks(psPeakListFile);
    if (!peaks.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
    QTextStream p(&peaks);
    peaks.seek(0);
    p >> x >> y>>str;
    psPeaks.clear();
    while (!p.atEnd()) {
        psPeaks << QPoint((int) x, (int) y);
        p >> x >> y>>str;
    }
    peaks.close();
    return true;
}

bool FullScreenImage::loadParticles() {
    QFile particlesFile(particlesFileName);
    if(!particlesFile.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
    
    particlePositionsToFom.clear();
    int xColumn = 0;
    int yColumn = 1;
    int fomColumn = 4;
    while (!particlesFile.atEnd()) {
        QString line = particlesFile.readLine().simplified().trimmed().toLower();
        if(!line.isEmpty()) {
            QStringList cells = line.split(' ');
            if(cells[0].startsWith('_')) {
                if(cells[0].endsWith("CoordinateX", Qt::CaseInsensitive) && cells.size() >1) xColumn = cells[1].remove('#').toInt()-1;
                else if (cells[0].endsWith("CoordinateY", Qt::CaseInsensitive) && cells.size() >1) yColumn = cells[1].remove('#').toInt()-1;
                else if (cells[0].endsWith("FigureOfMerit", Qt::CaseInsensitive) && cells.size() >1) fomColumn = cells[1].remove('#').toInt()-1;
            }
            else if(cells.size() > xColumn && cells.size() > yColumn) {
                float fom = -1.0;
                if(cells.size() > fomColumn) fom  = cells[fomColumn].toFloat();
                particlePositionsToFom.insert(QPoint(cells[xColumn].toInt(), cells[yColumn].toInt()), fom);
            } 
        }
    }
    particlesFile.close();
    return true;
}


void FullScreenImage::drawPeakList() {
    QPoint r;
    QPen pen(image_base->pen());
    pen.setColor(QColor(150, 250, 240));
    image_base->setPen(pen);

    foreach(r, psPeaks) {
        image_base->drawLine(QPointF(r.x() - peakEllipseSize / 2.0, -r.y()), QPointF(r.x() + peakEllipseSize / 2.0, -r.y()));
        image_base->drawLine(QPointF(r.x(), -r.y() + peakEllipseSize / 2.0), QPointF(r.x(), -r.y() - peakEllipseSize / 2.0));

        image_base->drawLine(-QPointF(r.x() - peakEllipseSize / 2.0, -(r.y())), -QPointF(r.x() + peakEllipseSize / 2.0, -(r.y())));
        image_base->drawLine(-QPointF(r.x(), -(r.y() + peakEllipseSize / 2.0)), -QPointF(r.x(), -(r.y() - peakEllipseSize / 2.0)));
    }
}

void FullScreenImage::drawParticles() {
    int maxWidth = 15;
    int maxAlpha = 255;
    int particleDiameter = projectData.parameterData(QDir(workingDir))->getVariant("gautomatch_diameter").toInt();
    
    for(QPoint r : particlePositionsToFom.keys()) {
        float fom = particlePositionsToFom[r];
        if(fom < 0.0 || fom > 1.0) fom = 1.0;
        
        QPen pen(image_base->pen());
        pen.setWidth(maxWidth*fom);
        pen.setColor(QColor(150, 250, 240, fom*maxAlpha));
        image_base->setPen(pen);
    
        int height = -1*(r.y())-particleDiameter/2+image->height()/2;
        image_base->drawEllipse(QRect(r.x()-particleDiameter/2 - image->width()/2, height, particleDiameter, particleDiameter));
    }
}

void FullScreenImage::drawSpotList() {
    if (peakListVisible) {
        //    if(!latticeVisible)

        foreach(QPoint v, peakList) {
            QPointF r = coordinates(v);
            if (visible["drawpeakellipses"]) {
                image_base->drawEllipse(QRectF(r.x() - peakEllipseSize / 2.0, -r.y() - peakEllipseSize / 2.0, peakEllipseSize, peakEllipseSize));
                image_base->drawEllipse(QRectF(-(r.x()) - peakEllipseSize / 2.0, -(-r.y() + peakEllipseSize / 2.0), peakEllipseSize, peakEllipseSize));
            } else {
                image_base->drawLine(QPointF(r.x() - peakEllipseSize / 2.0, -r.y()), QPointF(r.x() + peakEllipseSize / 2.0, -r.y()));
                image_base->drawLine(QPointF(r.x(), -r.y() + peakEllipseSize / 2.0), QPointF(r.x(), -r.y() - peakEllipseSize / 2.0));

                image_base->drawLine(-QPointF(r.x() - peakEllipseSize / 2.0, -(r.y())), -QPointF(r.x() + peakEllipseSize / 2.0, -(r.y())));
                image_base->drawLine(-QPointF(r.x(), -(r.y() + peakEllipseSize / 2.0)), -QPointF(r.x(), -(r.y() - peakEllipseSize / 2.0)));
            }
        }
        /*    else
              foreach(QPoint v, peakList)
              {
                QPointF r = coordinates(v);

                float x = r.x()-peakEllipseSize/2.0, y = r.y()-peakEllipseSize/2.0;
                float x2 = -r.x()-peakEllipseSize/2.0, y2 = -r.y()-peakEllipseSize/2.0;

        //        image_base->drawLine(int(x+1+(sqrt(2.0)-1.0)*ellipseSize/2),int(y-2+(sqrt(2.0)-1.0)*ellipseSize/2),int(x+(2.0*sqrt(2.0)-1.0)*ellipseSize/2),int(y-3+(2.0*sqrt(2.0)-1.0)*ellipseSize/2));
        //        image_base->drawLine(int(x+(sqrt(2.0)-1.0)*ellipseSize/2),int(y-2+(2.0*sqrt(2.0)-1.0)*ellipseSize/2),int(x-1+(2.0*sqrt(2.0)-1.0)*ellipseSize/2),int(y-2+(sqrt(2.0)-1.0)*ellipseSize/2));

        //        image_base->drawLine(x2+1+(sqrt(2.0)-1.0)*ellipseSize/2,y2-2+(sqrt(2.0)-1.0)*ellipseSize/2,x2+(2.0*sqrt(2.0)-1.0)*ellipseSize/2,y2-3+(2.0*sqrt(2.0)-1.0)*ellipseSize/2);
        //        image_base->drawLine(x2+(sqrt(2.0)-1.0)*ellipseSize/2,y2-2+(2.0*sqrt(2.0)-1.0)*ellipseSize/2,x2-1+(2.0*sqrt(2.0)-1.0)*ellipseSize/2,y2-2+(sqrt(2.0)-1.0)*ellipseSize/2);

                image_base->drawLine(int(x),-int(y+1.0),int(x+peakEllipseSize),-int(y+peakEllipseSize+1.0));
                image_base->drawLine(int(x),-int(y+peakEllipseSize+1.0),int(x+peakEllipseSize),-int(y+1.0));

                image_base->drawLine(QPointF(x2,-y2-1.0),QPointF(x2+peakEllipseSize,-y2-peakEllipseSize-1.0));
                image_base->drawLine(QPointF(x2,-y2-peakEllipseSize-1.0),QPointF(x2+peakEllipseSize,-y2-1.0));
              }
         */
    }
}

void FullScreenImage::clearPeakList() {
    saved["peaklist"] = false;
    peakList.clear();
    update();
}

void FullScreenImage::drawLattice(float lattice[2][2], bool primary) {
    QPen temp = image_base->pen();
    bool millerIndicesVisible = isVisible("millerindices");
    for (int i = -latticeOrders / 2; i <= latticeOrders / 2; i++)
        for (int j = -latticeOrders / 2; j <= latticeOrders / 2; j++) {
            float u = ((lattice[0][0]*((float) i) + lattice[0][1]*((float) j)) - latticeEllipseSize / 2.0);
            float v = ((lattice[1][0]*((float) i) + lattice[1][1]*((float) j)) + latticeEllipseSize / 2.0);
            if (i == 0 && j == 0) {
                u = -latticeEllipseSize / 2.0;
                v = latticeEllipseSize / 2.0;
            }

            if (primary) {
                QPen pen1 = temp;
                pen1.setWidth(latticeEllipseLineThickness);

                if (i == 1 && j == 0) pen1.setColor(QColor(230, 26, 13));
                if (i == 0 && j == 1) pen1.setColor(QColor(1, 3, 230));
                if (i == 0 && j == 0) pen1.setColor(Qt::black);
                image_base->setPen(pen1);
                image_base->drawEllipse(QRectF(u, -v, latticeEllipseSize, latticeEllipseSize));
            } else {
                QPen pen2 = QPen(QColor(113, 126, 230));
                pen2.setWidth(latticeEllipseLineThickness);

                if (i == 1 && j == 0) pen2.setColor(QColor(230, 26, 13));
                if (i == 0 && j == 1) pen2.setColor(QColor(1, 3, 230));
                if (i == 0 && j == 0) pen2.setColor(Qt::black);
                image_base->setPen(pen2);
                image_base->drawRect(QRectF(u, -v, latticeEllipseSize, latticeEllipseSize));
            }

            QFont font_new = QFont("Times", fontSize);
            QFont font_old = image_base->font();
            image_base->setFont(font_new);

            if (millerIndicesVisible) {
                image_base->drawText(QPoint(u, -(v + latticeEllipseSize / 2.0)), '(' + QString::number(i) + ',' + QString::number(j) + ')');
            }

            if (visible["latticenodecenters"]) {
                image_base->drawPoint((int) (u + latticeEllipseSize / 2.0), (int) (-v + latticeEllipseSize / 2.0 - 1));
            }

            image_base->setPen(temp);
            image_base->setFont(font_old);
        }
}

void FullScreenImage::grabScreen() {
    this->grab(QRect(0, 0, width(), height())).save(workingDir + "/screenShot.png");
    qDebug() << "Screenshot written at: " << workingDir + "/screenShot.png";
}

void FullScreenImage::drawRefinementList() {
    if (latticeRefineList != NULL) {
        if (refinementCandidate != NULL && *refinementCandidate != QPoint(0, 0) && !visible["maximumvaluefit"]) {
            QPen temp = image_base->pen();
            image_base->setPen(QPen(QColor(118, 215, 255)));
            float x = int(refinementCandidate->x()), y = int(refinementCandidate->y());
            //      image_base->drawEllipse(QRectF(x,-y,refinementEllipseSize,refinementEllipseSize));
            image_base->drawLine(QPointF(x - refinementEllipseSize / 2.0, -y), QPointF(x + refinementEllipseSize / 2.0, -y));
            image_base->drawLine(QPointF(x, -y + refinementEllipseSize / 2.0), QPointF(x, -y - refinementEllipseSize / 2.0));

            image_base->setPen(temp);
        }

        QMapIterator<QPoint, QPoint> i(*latticeRefineList);
        while (i.hasNext()) {
            i.next();
            //      float x = i.value().x() - refinementEllipseSize/2.0 , y = i.value().y() + refinementEllipseSize/2.0;
            float x = i.value().x(), y = i.value().y();
            //      image_base->drawEllipse(QRectF(x,-y,refinementEllipseSize,refinementEllipseSize));

            image_base->drawLine(QPointF(x - refinementEllipseSize / 2.0, -y), QPointF(x + refinementEllipseSize / 2.0, -y));
            image_base->drawLine(QPointF(x, -y + refinementEllipseSize / 2.0), QPointF(x, -y - refinementEllipseSize / 2.0));
        }
    }
}

void FullScreenImage::calculateCTF(float defocusX, float defocusY, float astigmatism) {
    int n = 100;
    QPainterPath p;
    // QPainterPath q[n];
    QPainterPath q[100];

    ParametersConfiguration* data = projectData.parameterData(QDir(workingDir));
    
    // if((imageHeader->mx() - 1 ) * 2 != imageHeader->my() && imageHeader->mx() != imageHeader->my()){
    //   std::cout << std::endl << "ERROR: Only square images supported for CTF display." <<  std::endl;
    //   std::cout << "Current image is not square. Dimensions are " << imageHeader->mx() << ", " << imageHeader->my() << std::endl << std::endl;
    //   return;
    // };

    float cellA = imageHeader->cellA();
    float cellB = imageHeader->cellB();
    float imageWidth  = imageHeader->nx();
    float imageHeight = imageHeader->ny();
    float PixelSiz = cellB / imageHeader->my();
    float phacon = data->getValue("phacon").toFloat();
    float Cs = data->getValue("cs").toFloat();
    float kV = data->getValue("kv").toFloat();

    // Quick fix to allow focus to be used on Cs-corrected mircoscopes
    if (Cs > -0.001 && Cs < 0.001) {
        Cs = 0.001;
    }

    float N = 1000;
    float dz = 0.0;
    float theta = 0.0;
    double k0 = 0.0;
    float cost = 0.0, sint = 0.0;


    kV *= 1000.0;
    // stepSize *= 1.0e4;
    float lambda = 12.26 / sqrt(kV + 0.9785 * kV * kV * 1.0e-6);
    Cs *= 1.0e7;
    float ampcon = sqrt(1.0 - phacon * phacon);
    astigmatism *= pi / 180.0;

    // qDebug()<<"defocusX:"<<defocusX<<" defocusY:"<<defocusY<<" imageWidth: "<<imageWidth<<" imageHeight: "<<imageHeight<<" PixelSiz: "<<PixelSiz<<" kV:"<<kV<<" Cs:"<<Cs<<" lambda:"<<lambda<<endl;

    QPoint next;
    QPointF d;

    float u, v;
    float jx;

    /* 
    CHEN: Updated on Dec. 21, 2014.

    Here, we will calculate the radius q=k0 of j-th zero crossing of the CTF, under angle theta:
    The CTF equation is:
    CTF(q) = phacon * sin(pi/2*( Cs * lambda**3 * q**4 - 2 * dz * lambda * q**2)) + 
             ampcon * cos(pi/2*( Cs * lambda**3 * q**4 - 2 * dz * lambda * q**2)) 

    with ampcon = sqrt( 1 - phacon**2 ) 

    We set this to zero:    CTF(q) = 0

    phacon*sin(pi/2*phi)+sqrt(1-phacon**2)*cos(pi/2*phi) = 0

    Solution according to Wolfram Alpha:    

    phi = 2*(j - atan(sqrt(1-phacon**2)/(phacon +/- 1))/pi)

    another exact solution:
    phi = -acos(phacon)

    Here, using:
    phi = (jx)    
              =================================================
       with   jx := j - 2*atan(sqrt(1-phacon**2)/(phacon-1))/pi
              =================================================
          


    That gives:  

    Cs * lambda**3 * q**4 - 2 * dz * lambda * q**2 = 2 * jx
 
    we need to solve this for q:
    q**4 * Cs*lambda**3 - q**2 * 2*dz*lambda - 2*jx = 0

    we substitute q2 for q**2:

    q2**2 * Cs*lambda**3 - q2 * 2*dz*lambda - 2*jx = 0

    q2**2 - q2 * 2*dz*lambda/(Cs*lambda**3) - 2*jx/(Cs*lambda**3) = 0

    q2**2 - q2 * 2*dz/(Cs*lambda**2) - 2*jx/(Cs*lambda**3) = 0

    gives:

    q2 = dz/(Cs*lambda**2) +/- sqrt( (dz/(Cs*lambda**2))**2       + 2*jx/(Cs*lambda**3) ) 

    q2 = dz/(Cs*lambda**2) +/- sqrt( (dz**2 /(Cs**2 * lambda**4)) + 2*jx/(Cs*lambda**3) ) 

    q2 = dz/(Cs*lambda**2) +/- sqrt( (dz**2                       + 2*jx *Cs*lambda)    / (Cs**2 * lambda**4) ) 

    q2 = dz/(Cs*lambda**2) +/- sqrt( (dz**2 + 2*jx*Cs*lambda) ) / ( Cs * lambda**2 ) 

    q2 = ( dz +/- sqrt( (dz**2 + 2*jx*Cs*lambda) ))  / (Cs * lambda**2)

    q = sqrt ( ( dz +/- sqrt(dz**2 + 2*jx*Cs*lambda) ) / (Cs * lambda**2) )

    q = sqrt ( 1 / Cs * ( dz +/- sqrt(dz**2 + 2*jx*Cs*lambda) ) ) / lambda

    ============================================================================
    q = 1 / lambda * sqrt ( 1 / Cs * ( dz +/- sqrt(dz**2 + 2*lambda*Cs * jx) ) )
    ============================================================================
    jx := j - 2*atan(sqrt(1-phacon**2)/(phacon-1))/pi
    ============================================================================

     */

    for (float i = 0; i <= N; i += 1.0) {
        theta = i / N * 2.0 * pi;
        cost = cos(theta);
        sint = sin(theta);

        dz = 0.5 * (defocusX + defocusY + cos(2.0 * (theta - astigmatism))*(defocusX - defocusY));
        for (int j = 1; j < n + 1; j++) {

            /* 
            if ( phacon < 1.0 )
              if(dz>0.0) jx = 2.0*atan(sqrt(1.0-phacon*phacon)/(phacon-1.0))/pi - float(j-1) ; 
              else       jx = 2.0*atan(sqrt(1.0-phacon*phacon)/(phacon-1.0))/pi + float(j-1) ; 
            else
              if(dz>0.0) jx = - (float)j;
              else       jx = + (float)j;

            if(dz>0.0) k0 = imageWidth/magnification*stepSize/(lambda)*sqrt(1.0/Cs*(dz-sqrt(dz*dz+2.0*lambda*Cs*jx)));
            else       k0 = imageWidth/magnification*stepSize/(lambda)*sqrt(1.0/Cs*(dz+sqrt(dz*dz+2.0*lambda*Cs*jx)));
             */

            // The following gives the same result:
            if (dz > 0.0) k0 = (double) ( imageWidth - 1 )  * 2.0 * PixelSiz / (double) (lambda) * sqrt(1.0 / Cs * (dz - sqrt(dz * dz + 2.0 * lambda * (double) Cs * (acos(phacon) / pi - (double) (j - 1)))));
            else          k0 = (double) ( imageWidth - 1 ) * 2.0 * PixelSiz / (double) (lambda) * sqrt(1.0 / Cs * (dz + sqrt(dz * dz + 2.0 * lambda * (double) Cs * (acos(phacon) / pi + (double) (j - 1)))));

            u = (float) k0*cost;
            v = (float) k0*sint;
            // Scale u and v for non-square image dimensions:
            v = v * imageHeader->ny() / ((imageHeader->nx()-1)*2) ;

            if (u > 0.0) u += 0.5;
            else u -= 0.5;
            if (v > 0.0) v += 0.5;
            else v -= 0.5;
            next = QPoint(int(u), -int(v));
            d = (next - q[j - 1].currentPosition().toPoint());
            if (i == 0 || d.x() * d.x() + d.y() * d.y() > imageWidth * imageWidth / (16.0) || abs(u)>imageHeader->nx() || abs(v)>imageHeader->ny()/2 )
                q[j - 1].moveTo(next);
            else
                q[j - 1].lineTo(next);
        }
    }

    for (int i = 0; i < n; i++)
        p.addPath(q[i]);

    ctfCurves = p;

    update();
}

void FullScreenImage::drawCTF() {
    image_base->drawPath(ctfCurves);
}

void FullScreenImage::drawResolutionRing() {
    double res = resolutionRingValue;
    int nx = imageHeader->nx();
    int ny = imageHeader->ny();
    double adjustedNx = (double)(nx-1)*2 ;
    double xScale = imageHeader->mx() / imageHeader->cellA();
    double yScale = imageHeader->my() / imageHeader->cellB();
    double radx = adjustedNx/(xScale*res);
    double rady = ny/(yScale*res);
    image_base->drawEllipse(QPoint(0,0), radx, rady);
}


void FullScreenImage::drawRealLattice(float lattice[2][2]) {

    // CHEN: updated on Dec. 22, 2014.

    float u1 = lattice[0][0], u2 = lattice[1][0], v1 = lattice[0][1], v2 = lattice[1][1];
    float w = image->width();

    float AA1, AA2, BB1, BB2;

    float ASTR = sqrt(u1 * u1 + u2 * u2);
    float BSTR = sqrt(v1 * v1 + v2 * v2);
    float SINASTR = u2 / ASTR;
    float COSASTR = u1 / ASTR;
    float SINBSTR = v2 / BSTR;
    float COSBSTR = v1 / BSTR;

    float SINGMSTR = SINASTR * COSBSTR - COSASTR*SINBSTR;
    float COSGMSTR = SINASTR * SINBSTR + COSASTR*COSBSTR;
    float GAMMASTR = atan2(SINGMSTR, COSGMSTR);

    if (GAMMASTR >= 0.0) {
        AA1 = -v2;
        AA2 = v1;
        BB1 = u2;
        BB2 = -u1;
    } else {
        AA1 = v2;
        AA2 = -v1;
        BB1 = -u2;
        BB2 = u1;
    }
    float SINA = AA2 / BSTR;
    float COSA = AA1 / BSTR;
    float SINB = BB2 / ASTR;
    float COSB = BB1 / ASTR;

    if (GAMMASTR < 0.0) GAMMASTR = -GAMMASTR;

    float A = w / (ASTR * sin(GAMMASTR));
    float B = w / (BSTR * sin(GAMMASTR));

    float a1 = A*COSA;
    float a2 = -A*SINA;
    float b1 = B*COSB;
    float b2 = -B*SINB;

    QPointF a(a1, a2), b(b1, b2), o1(1.9, 0.0), o2(0.0, 1.9);

    QPointF r1, r2;
    float rlx = image->width() / 2.0;
    float rly = image->height() / 2.0;

    // std::cout << "Reci lattice is u = " << u1 << "," << u2 << "     v = " << v1 << "," << v2 << endl;
    // std::cout << "Real lattice is a = " << a1 << "," << a2 << "     b = " << b1 << "," << b2 << endl;

    QPen pen(image_base->pen());
    pen.setColor(QColor(255, 240, 0));
    pen.setWidth(2);
    image_base->setPen(pen);
    for (int i = -60; i <= 60; i++)
        for (int j = -60; j <= 60; j++) {
            r1 = (i) * a + (j + 0.1) * b;
            r2 = (i) * a + (j + 0.9) * b;
            if (r1.x()>-rlx && r1.x() < rlx && r2.x()>-rlx && r2.x() < rlx &&
                    r1.y()>-rly && r1.y() < rly && r2.y()>-rly && r2.y() < rly)
                image_base->drawLine(r1, r2);
            r1 = (i + 0.1) * a + (j) * b;
            r2 = (i + 0.9) * a + (j) * b;
            if (r1.x()>-rlx && r1.x() < rlx && r2.x()>-rlx && r2.x() < rlx &&
                    r1.y()>-rly && r1.y() < rly && r2.y()>-rly && r2.y() < rly)
                image_base->drawLine(r1, r2);

            // image_base->drawLine(i*a-60*b,(i)*a+(60)*b);
            // image_base->drawLine((-60)*a+i*b,(60)*a+(i)*b);
        }
    pen.setWidth(1);
    image_base->setPen(pen);
}

void FullScreenImage::drawTiltAxis(const QString &axis, const QString &coAxis, bool realSpace, bool invertAngle) {
    float w = screenWidth - 30, h = screenHeight - 30;
    double r = sqrt(image->width() * image->width() + image->height() * image->height()) / 2.0;

    ParametersConfiguration* data = projectData.parameterData(QDir(workingDir));
    
    float realang = data->getValue("realang").toFloat();
    float recipang = 180.0 - realang;
    bool inverttiltang = false;
    bool revhk = data->getVariant("revhk").toBool();
    bool rot90 = data->getVariant("rot90").toBool();
    bool rot180 = data->getVariant("rot180").toBool();
    bool sgnxch = data->getVariant("sgnxch").toBool();
    bool revhnd = data->getVariant("revhnd").toBool();
    bool revxsgn = data->getVariant("revxsgn").toBool();

    float thetaD = data->getValue(axis).toFloat();
    while (thetaD > 90.0) thetaD -= 180.0;
    while (thetaD <= -90.0) thetaD += 180.0;

    if (visible["tiltaxa"]) {
        if (revhk) {
            thetaD = recipang - thetaD;
            inverttiltang ^= true;
            if (thetaD > 90.0) {
                thetaD -= 180.0;
                inverttiltang ^= true;
            }
            if (thetaD <= -90.0) {
                thetaD += 180.0;
                inverttiltang ^= true;
            }
        }
        if (rot90) {
            thetaD -= recipang;
            if (thetaD > 90.0) {
                thetaD -= 180.0;
                inverttiltang ^= true;
            }
            if (thetaD <= -90.0) {
                thetaD += 180.0;
                inverttiltang ^= true;
            }
        }
        if (rot180) {
            inverttiltang ^= true;
        }
        if (sgnxch) {
            thetaD = -thetaD;
            inverttiltang ^= true;
        }
        if (revhnd) {
            inverttiltang ^= true;
        }
        if (revxsgn) {
            thetaD = -thetaD;
        }
    }
    if (thetaD > 90.0) {
        thetaD -= 180.0;
        inverttiltang ^= true;
    }
    if (thetaD <= -90.0) {
        thetaD += 180.0;
        inverttiltang ^= true;
    }

    if (invertAngle) thetaD *= -1;

    float theta = -(thetaD) / 180.0 * pi;

    QTransform temp = image_base->worldTransform();

    image_base->rotate(-thetaD);
    float x = fabs(r * cos(theta)), y = fabs(r * sin(theta));
    if (x > image->width() / 2.0) {
        x = image->width() / 2.0;
        y = x * tan(theta);
    }
    if (y > image->height() / 2.0) {
        y = image->height() / 2.0;
        x = y / tan(theta);
    }

    r = sqrt(x * x + y * y);


    QPen pen = QPen(Qt::black);
    pen.setWidth(2);
    image_base->setPen(pen);
    image_base->drawLine(QPointF(-r, -1.0 + 1.0 / scale), QPointF(r, -1.0 + 1.0 / scale));
    pen.setColor(QColor(254, 254, 0));
    image_base->setPen(pen);
    image_base->drawLine(QPointF(-r, -1.0), QPointF(r, -1.0));
    pen.setColor(Qt::black);
    image_base->setPen(pen);
    if (realSpace) {
        QFont font = QFont("Times", 24);
        font.setPixelSize(int(30.0 * 0.70 / scale));
        font.setWeight(QFont::Bold);
        image_base->setFont(font);

        QString tltTitle = axis.toUpper();
        QString tltAxis = "  " + axis.toUpper() + " = " + data->getValue(axis) + QChar(Qt::Key_degree) + " ";
        QString tltAng = " " + coAxis.toUpper() + " = " + data->getValue(coAxis) + QChar(Qt::Key_degree) + "  ";
        QString moreDefocus = " more underfocus ";
        QString lessDefocus = " less underfocus ";

        if (invertAngle) {
            moreDefocus = " lower ";
            lessDefocus = " higher ";
        }
        QFontMetrics metric(font);

        double rho = sqrt(w * w + h * h) / 2.0;
        x = fabs(rho * cos(theta)), y = fabs(rho * sin(theta));
        if (x > w / 2.0) {
            x = w / 2.0;
            y = x * tan(theta);
        }
        if (y > h / 2.0) {
            y = h / 2.0;
            x = y / tan(theta);
        }

        rho = sqrt(x * x + y * y) / scale;
        float p = std::min(h / 2.0 / scale, image->height() / 2.0);

        float edgeL = -std::min(rho, r), edgeR = (std::min(rho, r) - float(metric.boundingRect(tltAng).width()));
        float signTltAng = data->getValue("TLTANG").toFloat();
        if (inverttiltang) signTltAng = -signTltAng;
        if (signTltAng >= 0.0) signTltAng = 1.0;
        else signTltAng = -1.0;
        //    if(thetaD == 90.0) signTltAng*=-1;

        image_base->drawText(QPointF(-metric.boundingRect(tltTitle).width() / 2.0 + 1.0 / (float) scale, -5.0 + 1.0 / (float) scale), tltTitle);
        image_base->drawText(QPointF(edgeL + 1.0 / (float) scale, metric.boundingRect(tltAxis).height() + 1.0 / (float) scale), tltAxis);
        image_base->drawText(QPointF(edgeR + 1.0 / (float) scale, metric.boundingRect(tltAng).height() + 1.0 / (float) scale), tltAng);
        image_base->drawText(QPointF(-metric.boundingRect(moreDefocus).width() / 2.0 + 1.0 / (float) scale, (signTltAng * (-p + metric.boundingRect(moreDefocus).height())) + 1.0 / (float) scale), moreDefocus);
        image_base->drawText(QPointF(-metric.boundingRect(lessDefocus).width() / 2.0 + 1.0 / (float) scale, (signTltAng * (p - metric.boundingRect(lessDefocus).height())) + 1.0 / (float) scale), lessDefocus);


        pen.setColor(QColor(254, 254, 0));
        pen.setWidth(2);
        image_base->setPen(pen);

        image_base->drawText(QPointF(-metric.boundingRect(tltTitle).width() / 2.0, -5.0), tltTitle);
        image_base->drawText(QPointF(edgeL, metric.boundingRect(tltAxis).height()), tltAxis);
        image_base->drawText(QPointF(edgeR, metric.boundingRect(tltAng).height()), tltAng);
        image_base->drawText(QPointF(-metric.boundingRect(moreDefocus).width() / 2.0, (signTltAng * (-p + metric.boundingRect(moreDefocus).height()))), moreDefocus);
        image_base->drawText(QPointF(-metric.boundingRect(lessDefocus).width() / 2.0, (signTltAng * (p - metric.boundingRect(lessDefocus).height()))), lessDefocus);

    }
    image_base->setWorldTransform(temp);
}

void FullScreenImage::drawSelectionPath() {
    image_base->setPen(QPen(QColor(0, 255, 17), 5.0)); //, Qt::SolidLine, Qt::FlatCap, Qt::MiterJoin));
    image_base->drawPath(selectionPath);
}

void FullScreenImage::drawReferenceLocation(int i) {
    QPointF p;

    ParametersConfiguration* data = projectData.parameterData(QDir(workingDir));
    p = data->getQPointF("refori") - QPointF(image->width() / 2.0, image->height() / 2.0);
    p = QPointF(p.x(), -p.y());

    float sideLength = data->getValue(refBoxes[i]).toFloat();

    QPen pen;

    if (i == 0) pen.setColor(QColor(120, 255, 137));
    if (i == 1) pen.setColor(QColor(173, 229, 255));
    if (i == 2) pen.setColor(QColor(255, 90, 117));
    if (i == 3) pen.setColor(QColor(237, 255, 129));

    pen.setWidth(2);
    image_base->setPen(pen);


    image_base->drawRect(QRectF(p.x() - sideLength / 2.0, p.y() - sideLength / 2.0, sideLength, sideLength));
    image_base->drawLine(p + QPointF(0.0, 10.0), p + QPointF(0.0, -10.0));
    image_base->drawLine(p + QPointF(10.0, 0.0), p + QPointF(-10.0, 0.0));

    QFont font("Times", 24);
    font.setPixelSize(int(30.0 * 0.70 / scale));
    QFontMetrics metric(font);
    image_base->setFont(font);
    image_base->setPen(Qt::black);
    image_base->drawText(p - QPointF(sideLength / 2.0 - 10.0, sideLength / 2.0 - 1.0 / scale - metric.boundingRect(refBoxes[i]).height()), refBoxes[i]);
    image_base->setPen(Qt::white);
    image_base->drawText(p - QPointF(sideLength / 2.0 - 10.0, sideLength / 2.0 - metric.boundingRect(refBoxes[i]).height()), refBoxes[i]);
}

void FullScreenImage::drawMaximumValueFit() {
    QPen pen(image_base->pen());
    image_base->setPen(QColor(100, 204, 210));

    image_base->drawRect(refinementCandidate->x() - maximumValueFitRange, -refinementCandidate->y() - maximumValueFitRange, 2 * maximumValueFitRange + 1, 2 * maximumValueFitRange + 1);
    image_base->drawImage(refinementCandidate->x() - maximumValueFitRange, -refinementCandidate->y() - maximumValueFitRange, profile);
    image_base->setPen(pen);
}

void FullScreenImage::drawUnbendProfile() {
    QString file = workingDir + "/SCRATCH/prof/prof" + projectData.parameterData(QDir(workingDir))->getValue("imagename") + ".dat";
    QFile prof(file);
    if (!prof.open(QIODevice::ReadOnly | QIODevice::Text)) return;

    QTextStream is(&file);
    float x, y, s;
    while (!is.atEnd()) {
        is >> x >> y>>s;
        if (x != 0 || y != 0 || s != 0) image_base->drawPoint(QPointF(x, image->height() - y));
    }
}

void FullScreenImage::drawImage() {
    image_base->drawPixmap(QPointF(float(-scale * image->width()) / 2.0 + 0.5 * scale, float(-scale * image->height()) / 2.0 + 0.5 * scale), pixmap);
}

void FullScreenImage::drawOverlay() {
    updateLattice();
    if (imageHeader->mode() == 3 || imageHeader->mode() == 4) {
        image_base->setPen(QPen(QColor(0, 0, 0)));
        image_base->drawPoint(0, 0);
    }

    image_base->setPen(QPen(QColor(0, 255, 17)));
    image_base->setBrush(Qt::NoBrush);
    if (refineLatticeVisible) drawRefinementList();
    if (peakListVisible) drawSpotList();
    if (particlesVisible) drawParticles();
    if (latticeVisible) drawLattice(lattice, true);
    if (visible["reallattice"]) drawRealLattice(lattice);
    if (secondLatticeVisible) drawLattice(secondLattice, false);
    if (ctfVisible) drawCTF();
    if (resolutionRingVisible) drawResolutionRing();
    if (selectionVisible) drawSelectionPath();
    for (int i = 0; i < refBoxes.size(); i++)
        if (visible[refBoxes[i]]) drawReferenceLocation(i);
    if (visible["pspeaklist"]) drawPeakList();
    if (visible["maximumvaluefit"]) drawMaximumValueFit();
    drawUnbendProfile();
}

void FullScreenImage::drawRealOverlay() {
    if (visible["tiltaxis"]) drawTiltAxis("tltaxis", "tltang", true, false);
    if (visible["tiltaxa"]) drawTiltAxis("taxa", "tangl", true, true);
}

void FullScreenImage::paintEvent(QPaintEvent *event) {
    float nx = (float) ((int*) (imageHeader->rawData()))[0];
    float ny = (float) ((int*) (imageHeader->rawData()))[1];
    float cellA = ((float*) (imageHeader->rawData()))[10];
    float cellB = ((float*) (imageHeader->rawData()))[11];
    float cellC = ((float*) (imageHeader->rawData()))[15];
    float pixelSize = 1.0;
    float shearScale = -1.0 / tan((cellC) * pi / 180.0);
    //float shearScale = cellB/cellA*cos(cellC);

    if (imageHeader->mode() < 3 && cellA != 0.0 && cellB != 0.0) pixelSize = cellB / cellA * 1.0 / sqrt(fabs(1 + shearScale * shearScale));

    //qDebug()<<"CellA: "<<cellA<<" cellB: "<<cellB<<"shear scale: "<<shearScale<<" y-scale: "<<pixelSize<<" "<<sqrt(pixelSize*pixelSize*cellB*cellB+shearScale*shearScale*cellA*cellA);

    QWidget::paintEvent(event);
    image_base = new QPainter(this);
    image_base->setPen(QPen(QColor(0, 0, 0)));
    image_base->setBrush(QColor(0, 0, 0));
    image_base->setBackground(QBrush(QColor(0, 0, 0, 255)));
    image_base->setViewTransformEnabled(true);

    if (imageHeader->mode() == 3 || imageHeader->mode() == 4)
        image_base->fillRect(QRect(0, 0, width(), height()), QBrush(QColor(0, 0, 0, 255)));

#ifdef Q_OS_MAC
    image_base->translate(QPointF(float(width()) / 2.0 - 0.5, float(height()) / 2.0 - 0.5));
#else
    image_base->translate(QPointF(float(width()) / 2.0, float(height()) / 2.0));
#endif

    if (cellC != 0.0 && cellC != 90.0) image_base->shear(shearScale, 0.0);
    if (imageHeader->mode() < 3)
        if (nx != 0.0 && ny != 0.0 && cellA != 0.0 && cellB != 0.0)
            image_base->scale(1.0, pixelSize);
    drawImage();
    if (scale != 1.0) image_base->scale(scale, scale);
    sourceImage->setMatrix(image_base->worldMatrix());
    if (overlayVisible) drawOverlay();
    image_base->resetTransform();

#ifdef Q_OS_MAC
    image_base->translate(QPointF(float(width()) / 2.0 - 0.5, float(height()) / 2.0 - 0.5));
#else
    image_base->translate(QPointF(float(width()) / 2.0, float(height()) / 2.0));
#endif
    if (scale != 1.0) image_base->scale(scale, scale);
    if (visible["tiltaxis"]) drawTiltAxis("tltaxis", "tltang", imageHeader->mode() != 3 && imageHeader->mode() != 4, false);
    if (visible["tiltaxa"]) drawTiltAxis("taxa", "tangl", imageHeader->mode() != 3 && imageHeader->mode() != 4, true);
    delete image_base;
}

void FullScreenImage::update() {
    QWidget::update();
    emit painted();
}

QImage *FullScreenImage::getImage() {
    return image;
}

QPixmap *FullScreenImage::getPixmap() {
    return &pixmap;
}

void FullScreenImage::viewOverlay() {
    overlayVisible = 1;
}

void FullScreenImage::togglePeakList() {
    peakListVisible = peakListVisible^1;
    update();
}

void FullScreenImage::toggleLatticeView() {
    latticeVisible = latticeVisible^1;
    update();
}

void FullScreenImage::toggleSecondLatticeView() {
    secondLatticeVisible = secondLatticeVisible^1;
    update();
}

void FullScreenImage::toggleCTFView() {
    ctfVisible ^= 1;
    update();
}

void FullScreenImage::toggleResolutionRing() {
    resolutionRingVisible ^= 1;
    update();
}

void FullScreenImage::setResolutionRingValue(double value) {
    resolutionRingValue = value;
    update();
}


void FullScreenImage::toggleParticleView() {
    particlesVisible = particlesVisible^1;
    update();
}

void FullScreenImage::setPeakListView(bool enable) {
    peakListVisible = enable;
}

void FullScreenImage::setParticlesView(bool enable) {
    particlesVisible = enable;
}

void FullScreenImage::setLatticeView(bool enable) {
    latticeVisible = enable;
}

void FullScreenImage::setRefineLatticeView(bool enable) {
    refineLatticeVisible = enable;
}

void FullScreenImage::setCTFView(bool enable) {
    ctfVisible = enable;
}

void FullScreenImage::setResolutionRingView(bool enable) {
    resolutionRingVisible = enable;
}


void FullScreenImage::toggleSpot(const QPoint &pos) {
    float inv[2][2];
    float det = lattice[0][0] * lattice[1][1] - lattice[0][1] * lattice[1][0];

    if (det != 0) {
        inv[0][0] = lattice[1][1] / det;
        inv[1][1] = lattice[0][0] / det;
        inv[0][1] = -lattice[0][1] / det;
        inv[1][0] = -lattice[1][0] / det;

        float x = ((float) (pos.x()) * inv[0][0] + (float) (pos.y()) * inv[0][1]);
        float y = ((float) (pos.x()) * inv[1][0] + (float) (pos.y()) * inv[1][1]);
        if (x < 0.0) x -= 0.5;
        else x += 0.5;
        if (y < 0.0) y -= 0.5;
        else y += 0.5;

        if (x < 0.0) {
            x *= -1;
            y *= -1;
        }

        QPoint spot = QPoint(int(x), int(y));

        if (peakList.contains(spot)) peakList.remove(spot);
        else if (peakList.contains(-spot)) peakList.remove(-spot);
        else peakList << spot;
        update();
        saved["peaklist"] = false;
    }
}

void FullScreenImage::updateLattice() {
    QStringList cell;
    
    cell = projectData.parameterData(QDir(workingDir))->getValue("lattice").split(',');

    if(cell.size() > 3) {
        lattice[0][0] = cell[0].toFloat();
        lattice[1][0] = cell[1].toFloat();

        lattice[0][1] = cell[2].toFloat();
        lattice[1][1] = cell[3].toFloat();
    }

    cell = projectData.parameterData(QDir(workingDir))->getValue("secondlattice").split(',');
    if(cell.size() > 3) {
        secondLattice[0][0] = cell[0].toFloat();
        secondLattice[1][0] = cell[1].toFloat();

        secondLattice[0][1] = cell[2].toFloat();
        secondLattice[1][1] = cell[3].toFloat();
    }
}

void FullScreenImage::setLatticeRefinementList(QMap<QPoint, QPoint> &list) {
    latticeRefineList = &list;
}

void FullScreenImage::setRefinementCandidate(QPointF &candidate) {
    refinementCandidate = &candidate;
}

void FullScreenImage::setLatticeEllipseSize(int size) {
    if (size < 1)size = 1;
    latticeEllipseSize = size;
    update();
}

void FullScreenImage::setFontSize(int size) {
    fontSize = size;
    update();
}

void FullScreenImage::setLatticeEllipseLineThickness(int thickness) {
    std::cout << "Lattice Ellipse Line Thickness set";
    latticeEllipseLineThickness = thickness;
    update();
}

void FullScreenImage::setLatticeOrders(int orders) {
    latticeOrders = orders;
    update();
}

void FullScreenImage::setPeakEllipseSize(int size) {
    peakEllipseSize = size;
    update();
}

void FullScreenImage::setRefinementEllipseSize(int size) {
    refinementEllipseSize = size;
    update();
}

void FullScreenImage::rescale(float max, float min, bool invert) {
    sourceImage->rescale(max, min);
    image = sourceImage->getImage();
    if (!invert)
        pixmap = sourceImage->getPixmap().scaled((int) (scale * image->width()), (int) (scale * image->height()), Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
    else {
        image->invertPixels();
        pixmap = QPixmap::fromImage(*image).scaled((int) (scale * image->width()), (int) (scale * image->height()), Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
    }
    repaint();
}

void FullScreenImage::addSelectionVertex(const QPoint &point) {
    rawSelectionList << point;
    if (selectionPath.isEmpty()) {
        selectionPath.addEllipse(point.x() / scale, point.y() / scale, 2, 2);
        selectionPath.moveTo(point / scale);
    } else
        selectionPath.lineTo(point / scale);
    QPoint p = (point / scale);
    p = QPoint(p.x() + image->width() / 2 - 1, -p.y() + image->height() / 2);
    selectionList << p;
    update();
}

void FullScreenImage::addNativeSelectionVertex(const QPoint &point) {
    if (selectionPath.isEmpty()) {
        selectionPath.addEllipse(point.x(), point.y(), 2, 2);
        selectionPath.moveTo(point);
    } else
        selectionPath.lineTo(point);
    selectionList << point;
    update();
}

const QList<QPoint> &FullScreenImage::selectionVertexList() {
    return selectionList;
}

const QList<QPoint> &FullScreenImage::rawSelectionVertexList() {
    return rawSelectionList;
}

void FullScreenImage::clearSelectionVertices() {
    selectionList.clear();
    rawSelectionList.clear();
    selectionPath = QPainterPath();
    update();
}

float FullScreenImage::imageMax() {
    return sourceImage->max();
}

float FullScreenImage::imageMin() {
    return sourceImage->min();
}

void FullScreenImage::rescaleWidget() {
    pixmap = sourceImage->getPixmap().scaled((int) (scale * image->width()), (int) (scale * image->height()), Qt::IgnoreAspectRatio, Qt::SmoothTransformation);

    float nx = (float) ((int*) (imageHeader->rawData()))[0];
    float ny = (float) ((int*) (imageHeader->rawData()))[1];
    float cellA = ((float*) (imageHeader->rawData()))[10];
    float cellB = ((float*) (imageHeader->rawData()))[11];
    float cellC = ((float*) (imageHeader->rawData()))[15];
    float pixelSize = 1.0;
    float shearScale = float(image->height()) * tan((cellC - 90.0) * pi / 180.0) / float(image->width());

    if (imageHeader->mode() < 3 && cellA != 0.0 && cellB != 0.0) pixelSize = (cellB * nx) / (cellA * ny);

    if (image->width() * scale * (1.0 + 2.0 * fabs(shearScale)) < screenWidth)
        setFixedWidth(screenWidth);
    else
        setFixedWidth((int) (image->width() * scale * (1.0 + 2.0 * fabs(shearScale))));

    if (image->height() * pixelSize * scale < screenHeight)
        setFixedHeight(screenHeight);
    else
        setFixedHeight((int) (image->height() * scale * pixelSize));
}

void FullScreenImage::zoomIn() {
    if (image->width() * scale * 2.0 > 8192 || image->height() * scale * 2.0 > 8192) return;
    scale *= 1.25;
    //  if(scale>1.0) scale = 1.0;
    rescaleWidget();
    update();
}

void FullScreenImage::zoomOut() {
    if (image->width() * scale * 2.0 < 16 || image->height() * scale * 2.0 < 16) return;
    scale *= 0.8;
    rescaleWidget();
    update();
}

void FullScreenImage::zoomStandard() {
    scale = 1.0;
    rescaleWidget();
    update();
}

float FullScreenImage::imageScale() {
    return scale;
}

void FullScreenImage::setPathView(bool enable) {
    selectionVisible = enable;
}

int FullScreenImage::saveSelectionList() {
    float imageWidth = imageHeader->cellA();

    QFile f(selectionListFileName);
    if (!f.open(QIODevice::WriteOnly | QIODevice::Text)) return 0;

    f.write(QString(QString::number(imageWidth) + ", " + QString::number(imageWidth) +
            "    ! X,Y dimensions of the original image, origin bottom left (0,0)" + '\n').toLatin1());

    for (int i = 0; i < selectionList.size(); i++)
        f.write(QString(QString::number(selectionList[i].x()) + ", " + QString::number(selectionList[i].y()) + '\n').toLatin1());

    f.close();

    return 1;
}

mrcImage *FullScreenImage::getSourceImage() {
    return sourceImage;
}

void FullScreenImage::setVisible(const QString &overlay, bool enable) {
    visible[overlay.toLower()] = enable;
    update();
}

void FullScreenImage::toggleVisible(const QString &overlay) {
    if (!visible.contains(overlay.toLower())) visible.insert(overlay.toLower(), true);
    else
        visible[overlay.toLower()] ^= true;
    update();
}

bool FullScreenImage::isVisible(const QString &overlay) {
    if (visible.contains(overlay.toLower()))
        return visible[overlay.toLower()];
    else
        return false;
}

void FullScreenImage::setPSPeakListFile(const QString &file) {
    psPeakListFile = file;
    loadPSPeaks();
    update();
}

bool FullScreenImage::isSaved(const QString &name) {
    return saved[name.toLower()];
}

bool FullScreenImage::checkSaved() {
    int choice = 1;
    QHashIterator<QString, bool> it(saved);
    while (it.hasNext()) {
        it.next();
        if (!it.value()) {
            choice = QMessageBox::question(this, tr("Confirm Exit"), saveStrings[it.key().toLower()], tr("Save && Exit"), tr("Exit"), QString("Cancel"), 0, 1);
            if (choice == 2) return false;
            if (choice == 0) {
                (this->*saveFunctions[it.key().toLower()])();
            }
        }
    }
    return true;
}

void FullScreenImage::setMaxFitRange(int range) {
    maximumValueFitRange = range;
    createProfile();
}

void FullScreenImage::setSigma(double sigma) {
    maximumValueSigma = sigma;
    createProfile();
}

void FullScreenImage::setMaxSearchMethod(mrcImage::maxValueMethod method) {
    maxSearchMethod = method;
    createProfile();
}

int FullScreenImage::gaussian(int i, int j) {
    return (exp(-(i * i + j * j) / (4.0 * maximumValueSigma * maximumValueSigma))*255.0);
}

void FullScreenImage::createProfile() {
    int m, n;
    profile = QImage(2 * maximumValueFitRange, 2 * maximumValueFitRange, QImage::Format_ARGB32);
    profile.fill(0);
    if (maxSearchMethod == mrcImage::gauss_fit)
        for (int j = 0; j < 2 * maximumValueFitRange; j++)
            for (int i = 0; i < 2 * maximumValueFitRange; i++) {
                m = i - maximumValueFitRange;
                n = j - maximumValueFitRange;
                profile.setPixel(i, j, qRgba(100, 204, 210, gaussian(m, n)));
            } else if (maxSearchMethod == mrcImage::maximum_value) {
        profile.setPixel(maximumValueFitRange, maximumValueFitRange, qRgba(100, 204, 210, 255));
    }
    update();
}

void FullScreenImage::setCurrentMousePos(const QPoint &pos) {

}

void FullScreenImage::setPhaseOrigin(const QPoint &pos) {

}
