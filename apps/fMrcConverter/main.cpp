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

#include <QApplication>
#include <QDebug>
#include <QCommandLineParser>
#include <QFileInfo>
#include <QVariant>
#include <QString>
#include <QStringList>
#include <QDir>
#include <QImageWriter>
#include <QByteArray>

#include <iostream>

#include "mrcImage.h"

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    QCoreApplication::setApplicationName("fMrcConverter");
    QCoreApplication::setOrganizationName("LBEM");
    QCoreApplication::setOrganizationDomain("lbem.epfl.ch");

    QCommandLineParser cliParser;
    cliParser.setApplicationDescription("Focus Software Suite:\n\nfMrcConverter:  Converts mrc file to other image file formats.\nNOTE that it works only on 2D MRC images, if provided with volumes it converts z=0 plane.\n\n");
    cliParser.addHelpOption();
    cliParser.addVersionOption();
    cliParser.addPositionalArgument("input", "Input MRC format file to be converted");
    cliParser.addPositionalArgument("output", "Output file");
    
    QCommandLineOption formatOption(QStringList() << "f" << "format", "Output file format (if not specified it will be automatically guessed by file name)", "STRING");
    cliParser.addOption(formatOption);

    QCommandLineOption sizeOption(QStringList() << "s" << "size", "Comma separated values for size in x and y (if square image provide only one value)", "INT");
    cliParser.addOption(sizeOption);
    
    QCommandLineOption qualityOption(QStringList() << "q" << "quality", "The quality factor must be in the range 0 to 100. Specify 0 to obtain small compressed files, 100 for large uncompressed files", "INT");
    cliParser.addOption(qualityOption);
    
    QCommandLineOption ignoreAspectRatioOption("ignore", "Ignore the aspect ratio and prefer the size provided");
    cliParser.addOption(ignoreAspectRatioOption);
    
    cliParser.process(app);
    
    if (cliParser.positionalArguments().size() != 2) {
        std::cout << cliParser.helpText().toStdString() << "\n";
        exit(1);
    }

    //Check the input
    QString input = cliParser.positionalArguments().first();
    QString inputPath = QFileInfo(input).absolutePath() + "/" + QFileInfo(input).fileName();
    if(!QFileInfo(inputPath).exists()) {
        std::cout << "\nERROR: The input file: " << QString(inputPath).toStdString() + " does not exit!!\n\n";
        exit(1);
    }
    
    mrcImage inputMrc(inputPath);
    if(!inputMrc.getImage() || inputMrc.getImage()->isNull()) {
        std::cout << "\nERROR: Couldn't read input file: " << QString(inputPath).toStdString() + "!!\n\n";
        exit(1);
    }
    std::cout << "Read the input file: " << inputPath.toStdString() << std::endl;
    
    QImage inputImage = *(inputMrc.getImage());
    
    //Check the output
    QString output = cliParser.positionalArguments()[1];
    QString outputPath = QFileInfo(output).absolutePath() + "/" + QFileInfo(output).fileName();
    std::cout << "The output will be written on: " << outputPath.toStdString() << std::endl;
    if(QFileInfo(outputPath).exists()) {
        std::cout << "\nWARNING: The output file: " << QString(outputPath).toStdString() + " already exits!!\n\n";
    }
    
    //Get the format
    QStringList supportedFormat;
    for(QByteArray arr : QImageWriter::supportedImageFormats()) supportedFormat.append(QString(arr).toLower());
    QString format = cliParser.value(formatOption).toLower();
    if(format.isEmpty()) format = QFileInfo(output).suffix().toLower();
    if(!supportedFormat.contains(format)) {
        qDebug() << "ERROR: The format: " << format << " is not a supported format. Only following formats are supported:";
        qDebug() << supportedFormat;
        exit(1);
    }
    std::cout << "Output image will be formatted to: " << format.toStdString() << std::endl;
    
    //Get the size;
    QSize userSize = inputImage.size();
    if (cliParser.isSet(sizeOption)) {
        QString sizeStr = cliParser.value(sizeOption);
        QString sizex, sizey;
        if (sizeStr.contains(',')) {
            QStringList cell = sizeStr.split(',');
            if (cell.size() > 2) qDebug() << "WARNING: Ignoring last values from the size provided";
            sizex = cell[0];
            sizey = cell[1];
        } else {
            sizex = sizeStr;
            sizey = sizeStr;
        }
        if (!QVariant(sizex).canConvert<int>() || !QVariant(sizey).canConvert<int>()) {
            qDebug() << "WARNING: The size can not be interpreted. Check: " << sizex << sizey;
        } else {
            userSize = QSize(QVariant(sizex).toInt(), QVariant(sizey).toInt());
            std::cout << "The defined size for the output image: " << userSize.width() << ", " << userSize.height() << std::endl;
        }
    }
    
    //Scale the size with image
    QSize outputSize = inputImage.size();
    if(cliParser.isSet(ignoreAspectRatioOption)) outputSize.scale(userSize, Qt::IgnoreAspectRatio);
    else outputSize.scale(userSize, Qt::KeepAspectRatio);
    std::cout << "The output image will have the size: " << outputSize.width() << ", " << outputSize.height() << std::endl;
    
    if(cliParser.isSet(ignoreAspectRatioOption)) inputImage = inputImage.scaled(outputSize, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
    else inputImage = inputImage.scaled(outputSize, Qt::KeepAspectRatio, Qt::SmoothTransformation);
    
    //Save the image with the quality provided
    int quality = -1;
    if(cliParser.isSet(qualityOption)) {
        quality = QVariant(cliParser.value(qualityOption)).toInt();
        if(quality < 0 || quality > 100) {
            qDebug() << "The output quality could not be understood: " << cliParser.value(qualityOption) << " using the default";
            quality = -1;
        }
    }

    inputImage.save(outputPath, format.toLatin1().constData(), quality);
    std::cout << "The output file was written." << std::endl;
        
    return 0;
}
