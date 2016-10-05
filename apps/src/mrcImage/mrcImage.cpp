/*
 *  mrcImage.cpp
 *  2DX
 *
 *  Created by Bryant Gipson on 3/6/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <QFileInfo>
#include <QFile>
#include <QRgb>
#include <math.h>
#include <fftw3.h>
#include <iostream>
#include <QtGui/qimage.h>

#include "mrcImage.h"

using namespace std;

#define PI 3.14159265

void byteSwap(void *data, int size);

void byteSwap(void *data, int size) {
    if (size == 1) return;
    for (int i = 0; i < size / 2; i++) {
        char temp = ((char*) data)[i];
        ((char*) data)[i] = ((char*) data)[size - i - 1];
        ((char*) data)[size - i - 1] = temp;
    }
}

mrcImage::mrcImage(QString filename, bool thumbnail, QObject *parent)
: QThread(parent) {
    //    QTime timer; timer.start();

    image = NULL;
    imageData = NULL;
    rawData = NULL;
    showPhase = false;
    fileName = filename;

    minLength = 128;

    if (parent != NULL) {
        connect(this, SIGNAL(showProgress()), parent, SLOT(progressDialog()));
        connect(this, SIGNAL(setProgress(int)), parent, SIGNAL(setProgress(int)));
    }

    if (QFileInfo(filename).exists()) {
        empty = false;
        initialize();
        imageFile.setFileName(filename);
        headers << new mrcHeader(filename);
        determineCellSize(headers[0]);
        if (!thumbnail)
            loadImage(headers[0]);
        else {
            run();
        }
    } else
        empty = true;

    pixmap = QPixmap::fromImage(*image);
    //cerr<<"ms: "<<timer.elapsed()<<endl;		
}

mrcImage::~mrcImage() {
    clear();
}

void mrcImage::run() {
    if (!thumbnailPresent()) {
        //        cerr<<"Generating thumbnail."<<endl;
        //  	    emit showProgress();
        // qDebug()<<"Generating thumbnail";
        generateThumbnail();
        // qDebug()<<"Thumbnail generated";
    }

    quint32 nz = headers[0]->nz();
    if (nz == 0) nz = 1;

    if (headers[0]->mode() == 3 || headers[0]->mode() == 4)
        headers << new mrcHeader(fileName, headers[0]->nx() * headers[0]->ny() * nz * cellSize + 1024);
    else {
        headers << new mrcHeader(fileName, headers[0]->nx() * headers[0]->ny() * nz * cellSize + 1024);
    }

    determineCellSize(headers[1]);
    loadImage(headers[1]);
}

void mrcImage::initialize() {
    rawData = NULL;
    imageData = NULL;
    image = NULL;
}

void mrcImage::clear() {
    if (rawData != NULL) {
        delete rawData;
        rawData = NULL;
    }
    if (imageData != NULL) {
        delete[] imageData;
        imageData = NULL;
    }
    if (image != NULL) {
        delete image;
        image = NULL;
    }
}

void mrcImage::determineCellSize(mrcHeader *header) {
    quint32 mode = header->mode();
    if (mode == 0) cellSize = 1;
    else if (mode == 1) cellSize = 2;
    else if (mode == 2) cellSize = 4;
    else if (mode == 3) cellSize = 4;
    else if (mode == 4) cellSize = 8;
}

bool mrcImage::loadData(mrcHeader *header) {
    if (!imageFile.open(QIODevice::ReadOnly)) return false;
    imageFile.seek(header->dataOffset());
    quint32 mode = headers[0]->mode();
    qint64 dataSize;
    if (mode <= 4) {
        dataSize = header->nx() * header->ny() * cellSize;
    } else {
        std::cerr << "The MRC mode of image " << imageFile.fileName().toStdString() << " (mode = " << mode << ") is not a supported MRC mode.\n";
        return false;
    }
    rawData = new char[dataSize];
    imageFile.read(rawData, dataSize);
    //emit setProgress(10);
    if (mode != 0 && header->swapEndian()) {
        if (mode == 3 || mode == 4) {
            // CHEN: 4.1.2015
            // image 512x512 gives FFT of 513x513 in display, but 2x256x512 in data ("2x" is for complex values). 
            // This is 2 * nx/2 * ny
            // Shouldn't this then be: ???
            // for(quint32 i=0;i<header->nx()*header->ny()*cellSize/2;i++)
//#pragma omp parallel for shared(rawData)
            for (quint32 i = 0; i < header->nx() * header->ny() * cellSize / 2 - cellSize / 2; i++) {
                byteSwap(&rawData[i * cellSize / 2], cellSize / 2);
            }
        }
        else {
//#pragma omp parallel for shared(rawData)
            for (quint32 i = 0; i < header->nx() * header->ny() - 1; i++) {
                byteSwap(&rawData[i * cellSize], cellSize);
            }
        }
    }

    imageFile.close();
    return true;
}

float mrcImage::fastMagnitude(float a, float b) {
    return sqrt(a * a + b * b);
}

float mrcImage::stdDev(mrcHeader *header) {
    float x2 = 0.0;
    float nx = header->nx();
    float ny = header->ny();
    float r = 0.0;

    if (header->mode() == 3 || header->mode() == 4) {
        if (header->mode() == 3)
         {
//#pragma omp parallel for shared(rawData)
            for (quint32 k = 0; k < (nx - 1) * ny; k++)
                x2 += ((unsigned short*) rawData)[2 * k]*((unsigned short*) rawData)[2 * k]+((unsigned short*) rawData)[2 * k + 1]*((unsigned short*) rawData)[2 * k + 1];
            return sqrt(x2 / ((nx - 1) * ny) - header->mean() * header->mean());
         }
        else
         {
//#pragma omp parallel for shared(rawData)
            //CHEN: In an attempt to skip the center of the FFT with its super-strong peak:
            // for (quint32 k = 0; k < (nx - 1) * ny; k++) {
            for (quint32 k = nx + ny; k < (nx - 1) * (ny - 1) / 2; k++) {
                r = ((float*) rawData)[2 * k];
                x2 += r*r;
                r = ((float*) rawData)[2 * k + 1];
                x2 += r*r;
            }
            for (quint32 k = (nx - 1) * (ny + 1) / 2 + nx + ny; k < (nx - 1) * ny; k++) {
                r = ((float*) rawData)[2 * k];
                x2 += r*r;
                r = ((float*) rawData)[2 * k + 1];
                x2 += r*r;
            }
            return sqrt(x2 / (((nx - 1) * ny) - 2 * (nx + ny)) - header->mean() * header->mean());
         }
        // return sqrt(x2 / ((nx - 1) * ny) - header->mean() * header->mean());
    } else return 0.0;
}

void mrcImage::scaleData(mrcHeader *header, QImage::Format format) {
    quint32 mode = header->mode();
    qint64 dataSize = header->nx() * header->ny();

    quint32 nx = header->nx();
    quint32 ny = header->ny();

    if (imageData != NULL) delete[] imageData;

    if (mode == 3 || mode == 4) {
        imageData = new uchar[dataSize * 2 * 4];

        QTime timer;
        timer.start();

        int threadCount = 6;
        loadThread * l[threadCount];
        for (int t = 0; t < threadCount; t++) {
            if (t != threadCount - 1)
                l[t] = new loadThread(rawData, imageData, nx, ny, imageMin, imageMax, mode, (nx - 1) * ny / threadCount * t, (nx - 1) * ny / threadCount * (t + 1), format, loadThread::fft, showPhase);
            else
                l[t] = new loadThread(rawData, imageData, nx, ny, imageMin, imageMax, mode, (nx - 1) * ny * 5 / threadCount, (nx - 1) * ny, format, loadThread::fft, showPhase);
            l[t]->start();
        }

        for (int t = 0; t < threadCount; t++)
            l[t]->wait();

        qDebug() << "Load time: " << timer.elapsed();
    } else {
        int width;
        if (format == QImage::Format_Indexed8) width = 1;
        else if (format == QImage::Format_RGB32) width = 4;
        else return;

        imageData = new uchar[dataSize * width];
        int threadCount = 6;
        loadThread * l[threadCount];
        for (int t = 0; t < threadCount; t++) {
            if (t != threadCount - 1)
                l[t] = new loadThread(rawData, imageData, nx, ny, imageMin, imageMax, mode, (dataSize * width) / threadCount * t, (dataSize * width) / threadCount * (t + 1), format, loadThread::real, showPhase);
            else
                l[t] = new loadThread(rawData, imageData, nx, ny, imageMin, imageMax, mode, (dataSize * width)*5 / threadCount, dataSize*width, format, loadThread::real, showPhase);
            l[t]->start();
        }

        for (int t = 0; t < threadCount; t++)
            l[t]->wait();
        
    }

    // CHEN:  Should for non-suqare images this be:
    // if (format == QImage::Format_Indexed8 && header->ny() < 256 && header->nx() < 256) {
    if (format == QImage::Format_Indexed8 && header->ny() < 256) {
        uchar *temp = imageData;
        imageData = new uchar[1024 * 1024];
        for (unsigned int j = 0; j < header->ny(); j++)
            for (unsigned int i = 0; i < header->nx(); i++)
                imageData[i + j * 1024] = temp[i + j * header->nx()];
        delete temp;
    }
}

void mrcImage::formatImage(mrcHeader *header, QImage::Format format) {
    quint32 mode = header->mode();

    if (image != NULL) delete image;

    // CHEN:  Should for non-suqare images this be:
    //if (format == QImage::Format_Indexed8 && header->ny() < 256) {
    if (format == QImage::Format_Indexed8 && header->ny() < 256 && header->nx() < 256) {
        image = new QImage(imageData, 1024, 1024, format);
        if (mode == 3 || mode == 4)
            // *image = image->copy(0,0,(header->nx()-1)*2, header->ny());
            *image = image->copy(0, 0, (header->nx() - 1)*2, header->ny());
        else
            *image = image->copy(0, 0, header->nx(), header->ny());
    } else {
        if (mode == 3 || mode == 4)
            image = new QImage(imageData, (header->nx() - 1)*2, header->ny(), format);
        else {
            image = new QImage(imageData, header->nx(), header->ny(), format);
        }
    }
    // cout<<"In mrcImage.cpp, line 374:  Image mode = "<<mode<<",  Image dimensions = "<<image->width()<<" "<<image->height()<<endl;
    // *image = image->mirrored(false,true);
    QImage mirrorimage = image->mirrored(false, true);
    // cout<<"In mrcImage.cpp, line 376:  Image now mirrored."<<endl;
    // CHEN   Why is this crashing, for a non-square image ????? :
    *image = mirrorimage;
    // cout<<"In mrcImage.cpp, line 378:  Image now copied back."<<endl;
    if (format == QImage::Format_Indexed8) {
        image->setColorCount(256);
        int i;
        for (i = 0; i < 256; i++) {
            image->setColor(i, QColor(i, i, i).rgb());
        }
    }


    if (mode == 3 || mode == 4)
        type = "fft";
    else
        type = "real";
}

bool mrcImage::loadImage(mrcHeader *header, QImage::Format format) {
    QTime timer;
    //emit setProgress(1);
    loadData(header);
    if (header->mode() == 3 || header->mode() == 4) {
        //rescale(header,header->min(),2.0*header->mean() /*+1.0*stdDev(header) */,format);	
        //rescale(header,header->min(), header->mean() + 1.1*stdDev(header), format);	
        rescale(header, header->min(), 3.0 * header->mean() + 1.1 * stdDev(header), format);
    } else
        rescale(header, header->min(), header->max(), format);
    return true;
}

void mrcImage::rescale(mrcHeader *header, float min, float max, QImage::Format format) {
    imageMax = max;
    imageMin = min;
    QTime timer;
    timer.start();
    scaleData(header, format);
    // qDebug()<<"Scale data: "<<timer.elapsed();
    timer.restart();
    formatImage(header, format);
    // qDebug()<<"format Image: "<<timer.elapsed();
}

void mrcImage::rescale(float min, float max) {
    rescale(headers[0], min, max);
}

bool mrcImage::thumbnailPresent() {
    QFileInfo inf(imageFile);
    //  quint32 mode = headers[0]->mode();
    qint64 dataSize = headers[0]->nx() * headers[0]->ny() * headers[0]->nz() * cellSize;
    if (inf.size()-(dataSize + 1024) != 200 * 200 + 1024) return false;
    return true;
}

bool mrcImage::generateThumbnail() {
    qint64 dataSize = headers[0]->nx() * headers[0]->ny() * headers[0]->nz() * cellSize;

    loadImage(headers[0], QImage::Format_Indexed8);
    //  loadImage(headers[0], QImage::Format_RGB32);
    mrcHeader *header = new mrcHeader(*headers[0]);
    header->setNX(200);
    header->setNY(200);
    header->setNZ(1);
    header->setMode(0);
    header->setMax(255.0);
    header->setMin(0);

    if (!header->saveHeader(dataSize)) cerr << "Header write failed" << endl;
    if (!imageFile.open(QIODevice::Append)) return false;
    imageFile.seek(dataSize + 1024);
    //  QImage scaledImage = image->scaled(200,200).mirrored(false,true);
    imageFile.write((char*) (image->scaled(200, 200).mirrored(false, true).bits()), 200 * 200);
    delete header;
    imageFile.close();
    clear();
    return true;
}

QImage *mrcImage::getImage() {
    return image;
}

QPixmap &mrcImage::getPixmap() {
    if (!pixmap.isNull())
        pixmap = QPixmap::fromImage(*image);
    return pixmap;
}

const QString &mrcImage::getType() {
    return type;
}

mrcHeader *mrcImage::getHeader(int h) {
    if (!headers.empty())
        return headers[h];
    else
        return NULL;
}

bool mrcImage::isEmpty() {
    return empty;
}

float mrcImage::max() {
    return imageMax;
}

float mrcImage::min() {
    return imageMin;
}

float mrcImage::value(const QPoint &pos) {
    quint32 mode = headers[0]->mode();
    quint32 index;
    index = pos.x() + pos.y() * headers[0]->nx();

    if (validPosition(pos.x(), pos.y())) {
        if (mode == 0) return float(((unsigned char*) rawData)[index]);
        if (mode == 1) return float(((unsigned short*) rawData)[index]);
        if (mode == 2) return float(((float*) rawData)[index]);
        if (mode == 3) return float(fastMagnitude(((unsigned short*) rawData)[2 * index], ((unsigned short*) rawData)[2 * index + 1]));
        if (mode == 4) return float(fastMagnitude(((float*) rawData)[2 * index], ((float*) rawData)[2 * index + 1]));
    }

    return 0.0;
}

bool mrcImage::validPosition(int x, int y) {
    if (x < 0 || y < 0) return false;
    if (x > (int) headers[0]->nx() || y > (int) headers[0]->ny()) return false;
    return true;
}

QPoint mrcImage::maxValue(const QPoint &pos, int distance, mrcImage::maxValueMethod method, float sigma) {
    //quint32 mode = headers[0]->mode();
    QPoint location;
    //QPoint origin = pos;
    if (validPosition(pos.x(), pos.y())) {
        float max = 0.0, v;
        int k, l;

        if (method == mrcImage::maximum_value) {
            for (int j = -distance; j <= distance; j++)
                for (int i = -distance; i <= distance; i++) {
                    k = pos.x() + i;
                    l = pos.y() + j;
                    v = value(QPoint(k, l));
                    if (v > max) {
                        max = v;
                        location = QPoint(k, l);
                    }
                }
        } else if (method == mrcImage::gauss_fit) {
            fftwf_complex *data = new fftwf_complex[36 * int(distance * distance)];
            fftwf_complex *gaussian = new fftwf_complex[36 * int(distance * distance)];
            //fftwf_complex *transform;
            fftwf_plan p;
            float m, n;
            int index;
            float max = 0.0, max2 = 0.0, v, v2;
            for (int j = -3 * distance; j < 3 * distance; j++)
                for (int i = -3 * distance; i < 3 * distance; i++) {
                    index = i + 3 * distance + (j + 3 * distance)*6 * distance;
                    if (i >= -distance && i < distance && j >= -distance && j < distance) {
                        k = pos.x() + i;
                        l = pos.y() + j;
                        m = i;
                        n = j;
                        data[index][0] = value(QPoint(k, l)) / (6 * (int) distance);
                        gaussian[index][0] = exp(-(m * m + n * n) / (4.0 * sigma * sigma));

                        v = data[index][0];
                        v2 = gaussian[index][0];
                        if (v > max) max = v;
                        if (v2 > max2) max2 = v2;
                    } else {
                        gaussian[index][0] = 0.0;
                        data[index][0] = 0.0;
                    }

                    gaussian[index][1] = 0.0;
                    data[index][1] = 0.0;
                }

#ifdef VIEW_PROFILE
            QFile datav("./data.pgm");
            datav.open(QIODevice::WriteOnly | QIODevice::Text);
            datav.write(("P2\n" + QString::number(2 * distance) + " " + QString::number(2 * distance) + '\n').toAscii());
            datav.write((QString::number(255) + '\n').toAscii());

            for (int j = -distance; j < distance; j++) {
                for (int i = -distance; i < distance; i++) {
                    index = i + 3 * distance + (j + 3 * distance)*6 * distance;
                    v = data[index][0] * data[index][0] + data[index][1] * data[index][1];
                    datav.write((QString::number(int(v / max * 255.0)) + " ").toAscii());
                }
                datav.write(QString('\n').toAscii());
            }

            datav.close();

            QFile gaussianv("./gaussian.pgm");
            gaussianv.open(QIODevice::WriteOnly | QIODevice::Text);
            gaussianv.write(("P2\n" + QString::number(2 * distance) + " " + QString::number(2 * distance) + '\n').toAscii());
            gaussianv.write((QString::number(255) + '\n').toAscii());

            for (int j = -distance; j < distance; j++) {
                for (int i = -distance; i < distance; i++) {
                    index = i + 3 * distance + (j + 3 * distance)*6 * distance;
                    v = gaussian[index][0] * gaussian[index][0] + gaussian[index][1] * gaussian[index][1];
                    gaussianv.write((QString::number(int(v / max2 * 255.0)) + " ").toAscii());
                }
                gaussianv.write(QString('\n').toAscii());
            }

            gaussianv.close();
#endif

            p = fftwf_plan_dft_2d(6 * (int) distance, 6 * (int) distance, data, data, FFTW_FORWARD, FFTW_ESTIMATE);
            fftwf_execute(p);
            fftwf_destroy_plan(p);

            p = fftwf_plan_dft_2d(6 * (int) distance, 6 * (int) distance, gaussian, gaussian, FFTW_FORWARD, FFTW_ESTIMATE);
            fftwf_execute(p);
            fftwf_destroy_plan(p);

            float a, b, ga, gb;

            for (int j = -3 * distance; j < 3 * distance; j++)
                for (int i = -3 * distance; i < 3 * distance; i++) {
                    index = i + 3 * distance + (j + 3 * distance)*6 * distance;
                    a = data[index][0];
                    b = data[index][1];
                    ga = gaussian[index][0];
                    gb = gaussian[index][1];
                    data[index][0] = (a * ga - b * gb) / (6 * distance) * powf(-1, i + j);
                    data[index][1] = (a * gb + b * ga) / (6 * distance) * powf(-1, i + j);
                }

            p = fftwf_plan_dft_2d(6 * (int) distance, 6 * (int) distance, data, data, FFTW_BACKWARD, FFTW_ESTIMATE);
            fftwf_execute(p);
            fftwf_destroy_plan(p);

            max = 0.0;

            for (int j = -distance; j < distance; j++)
                for (int i = -distance; i < distance; i++) {
                    k = pos.x() + i;
                    l = pos.y() + j;

                    index = i + 3 * distance + (j + 3 * distance)*6 * distance;
                    v = data[index][0] * data[index][0] + data[index][1] * data[index][1];
                    if (v > max) {
                        max = v;
                        location = QPoint(k, l);
                    }
                }

#ifdef VIEW_PROFILE
            QFile cc("./cc.pgm");
            cc.open(QIODevice::WriteOnly | QIODevice::Text);
            cc.write(("P2\n" + QString::number(2 * distance) + " " + QString::number(2 * distance) + '\n').toAscii());
            cc.write((QString::number(255) + '\n').toAscii());

            for (int j = -distance; j < distance; j++) {
                for (int i = -distance; i < distance; i++) {
                    index = i + 3 * distance + (j + 3 * distance)*6 * distance;
                    v = data[index][0] * data[index][0] + data[index][1] * data[index][1];
                    cc.write((QString::number(int(v / max * 255.0)) + " ").toAscii());
                }
                cc.write(QString('\n').toAscii());
            }

            cc.close();
#endif

            delete[] gaussian;
            delete[] data;
        }
    }
    return location;
}

float mrcImage::phase(const QPoint &pos) {
    quint32 mode = headers[0]->mode();
    quint32 index;
    index = pos.x() + pos.y() * headers[0]->nx();
    float rotatePhase = 1.0;
    rotatePhase = powf(-1, pos.x() + pos.y());

    if (pos.x() >= 0 && quint32(pos.x()) < headers[0]->nx() && pos.y() > 0 && quint32(pos.y()) < headers[0]->ny()) {
        if (mode == 0 || mode == 1 || mode == 2) return 0.0;
        if (mode == 3) return fmod(atan2(((unsigned short*) rawData)[2 * index + 1] * rotatePhase, ((unsigned short*) rawData)[2 * index] * rotatePhase) + 2.0 * PI, 2.0 * PI);
        if (mode == 4) return fmod(atan2(((float*) rawData)[2 * index + 1] * rotatePhase, ((float*) rawData)[2 * index] * rotatePhase) + 2.0 * PI, 2.0 * PI);
    }

    return 0.0;
}

void mrcImage::setViewPhase(bool value) {
    showPhase = value;
}

void printMatrix(QMatrix m) {
    cout << m.m11() << " " << m.m12() << endl;
    cout << m.m21() << " " << m.m22() << endl;
    cout << "---" << endl;
    cout << m.determinant() << endl;
    cout << endl;
}

void mrcImage::setMatrix(const QMatrix &matrix) {
    tMatrix = matrix;
}

const QMatrix &mrcImage::matrix() {
    return tMatrix;
}

const QString &mrcImage::getFileName() {
    return fileName;
}

