/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   import_result.hpp
 * Author: biyanin
 *
 * Created on June 10, 2016, 4:17 PM
 */

#ifndef IMPORT_RESULT_HPP
#define IMPORT_RESULT_HPP

#include <iostream>

#include <QWizardPage>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QAbstractItemView>
#include <QHeaderView>
#include <QLabel>
#include <QProgressDialog>
#include <QVBoxLayout>
#include <QFileInfo>
#include <QString>
#include <QStringList>
#include <QDir>
#include <QMap>
#include <QPair>
#include <QDebug>
#include <QApplication>

#include "ProjectData.h"

#include "project_preferences.h"

class ImportExecuteWizardPage : public QWizardPage {

    Q_OBJECT

public:
    ImportExecuteWizardPage(QWidget* parent = NULL){
        setTitle("Execute the import");

        createFilesTable();
        resultLabel_ = new QLabel(this);

        QVBoxLayout *mainLayout = new QVBoxLayout;
        mainLayout->addWidget(resultLabel_);
        mainLayout->addWidget(resultsTable_);
        setLayout(mainLayout);
    }

public slots:

    void initializePage() override {
        analyzeImport();
        if (numberToImage_.keys().isEmpty()) {

            resultLabel_->setText("No such files could be found. (if not intended, please check the options again.)");
            resultLabel_->setWordWrap(true);
            resultsTable_->hide();
            setCommitPage(false);
        } else {
            setSubTitle("Following actions would be executed. Please check and edit. Once committed, you won't be able to change it back.!");
            showFiles();
            setCommitPage(true);
        }
    }

    bool validatePage() override {

        if (!numberToImage_.keys().isEmpty()) {
            QDir imagesDir = projectData.projectDir();
            if (!imagesDir.exists()) QDir().mkpath(imagesDir.absolutePath());

            QStringList numbers = numberToImage_.keys();

            QProgressDialog progressDialog(this);
            progressDialog.setCancelButtonText(tr("&Cancel"));
            progressDialog.setRange(0, numbers.size());
            progressDialog.setWindowTitle(tr("Setting up images"));
            
            imagesDir.mkpath(importGroup_);
            QFile(projectData.projectDir().absolutePath()+"/merge").link("../2dx_master.cfg", projectData.projectPath() + "/" + importGroup_ + "/2dx_master.cfg");

            for (int i = 0; i < numberToImage_.keys().size(); ++i) {

                progressDialog.setValue(i);
                progressDialog.setLabelText(tr("Importing image %1 of %2...")
                        .arg(i).arg(numbers.size()));
                qApp->processEvents();

                if (progressDialog.wasCanceled()) break;
                
                QString number = numbers.at(i);
                imagesDir.mkpath(importGroup_ + "/" + number);
                
                ProjectPreferences(projectData.projectPath()).setLastImageNumber(number.toInt());

                //Copy Config File
                if(!QFile(projectData.workingPath() + "/2dx_merge.cfg").copy(imagesDir.absolutePath() + "/" + importGroup_ + "/" + number + "/2dx_image.cfg")) {
                    qDebug() << "Was not able to copy config file";
                }
                
                //Change some parameters
                QFile cfgFile(imagesDir.absolutePath() + "/" + importGroup_ + "/" + number + "/2dx_image.cfg");
                
                if (!cfgFile.open(QIODevice::Append | QIODevice::Text)) {
                    qDebug() << "Error in opening file: " << imagesDir.absolutePath() + "/" + importGroup_ + "/" + number + "/2dx_image.cfg\n"; 
                    continue;
                }
                                
                cfgFile.write((QString("set imagename = ") + '"' + number + '"' + "\n#\n").toLatin1());
                cfgFile.write((QString("set nonmaskimagename = ") + '"' + number + '"' + "\n#\n").toLatin1());
                cfgFile.write((QString("set imagenumber = ") + '"' + number + '"' + "\n#\n").toLatin1());
                cfgFile.write((QString("set imagename_original = ") + '"' + numberToImage_[number] + '"' + "\n#\n").toLatin1());
                
                //Check for movie files
                if (numberToMovie_.contains(number)) {
                    cfgFile.write((QString("set movie_stackname = ") + '"' + numberToMovie_[number] + '"' + "\n#\n").toLatin1());
                }
                
                cfgFile.write(QString("set initialization_reset = y \n#\n").toLatin1());
                cfgFile.write(QString("set initialization_executable = y \n#\n").toLatin1());
                
                cfgFile.close();
                
                //Copy Image File
                QFile(numberToImage_[number]).copy(imagesDir.absolutePath() + "/" + importGroup_ + "/" + number + "/" + number + "_raw.mrc");

                //copy movie files
                if (numberToMovie_.contains(number)) {
                    QFile(numberToMovie_[number]).copy(imagesDir.absolutePath() + "/" + importGroup_ + "/" + number + "/" + number + "_stack.mrc");
                }

            }
        }
        return true;
    }




private:

    void analyzeImport() {

        numberToImage_.clear();
        numberToMovie_.clear();

        ProjectPreferences projectConfig(projectData.projectPath());
        importImagesPath_ = projectConfig.importImageDir();
        if (importImagesPath_.isEmpty() || !QFileInfo(importImagesPath_).exists()) {
            std::cerr << "The import image path does not exist\n";
            return;
        }

        importMoviesPath_ = projectConfig.importMovieDir();
        QDir movieDir(importMoviesPath_);

        importGroup_ = projectConfig.importGroup();
        if (importGroup_.isEmpty()) importGroup_ = "import";

        QString ignoreImagePattern = projectConfig.importIgnorePattern();
        int imageNumberLength = projectConfig.importImageLength();

        int uid = projectConfig.lastImageNumber();

        foreach(QString image, QDir(importImagesPath_).entryList(QStringList("*.mrc"), QDir::Files | QDir::NoSymLinks)) {
            QString imageNumber = commitIntToStringLength(++uid, imageNumberLength);
            while(QDir(projectData.projectPath() + "/" + importGroup_ + "/" +imageNumber).exists()) {
                imageNumber = commitIntToStringLength(++uid, imageNumberLength);
            }
            numberToImage_.insert(imageNumber, QDir(importImagesPath_).absoluteFilePath(image));

            //Check if the file exist in the movie dir
            if (movieDir.exists()) {
                QString baseName = QFileInfo(importImagesPath_ + "/" + image).completeBaseName();
                if (!ignoreImagePattern.isEmpty()) {
                    baseName.contains(ignoreImagePattern, Qt::CaseInsensitive);
                    baseName.remove(ignoreImagePattern, Qt::CaseInsensitive);
                }

                QStringList possibleMovieFiles = movieDir.entryList(QStringList(baseName + "*.mrc*"), QDir::Files | QDir::NoSymLinks);
                if (!possibleMovieFiles.isEmpty()) numberToMovie_.insert(imageNumber, movieDir.absoluteFilePath(possibleMovieFiles[0]));

            }
        }
    }

    void showFiles() {

        resultsTable_->show();
        resultsTable_->setRowCount(0);

        foreach(QString number, numberToImage_.keys()) {

            QTableWidgetItem* numberItem = new QTableWidgetItem(number);
            numberItem->setFlags(numberItem->flags() ^ Qt::ItemIsEditable);

            QTableWidgetItem* imageItem = new QTableWidgetItem(QFileInfo(numberToImage_[number]).fileName());
            imageItem->setFlags(imageItem->flags() ^ Qt::ItemIsEditable);

            QTableWidgetItem *movieItem;
            if (numberToMovie_.contains(number)) {
                movieItem = new QTableWidgetItem(QFileInfo(numberToMovie_[number]).fileName());
            } else {
                movieItem = new QTableWidgetItem("-");
            }
            movieItem->setFlags(movieItem->flags() ^ Qt::ItemIsEditable);

            int row = resultsTable_->rowCount();
            resultsTable_->insertRow(row);
            resultsTable_->setItem(row, 0, numberItem);
            resultsTable_->setItem(row, 1, imageItem);
            resultsTable_->setItem(row, 2, movieItem);
        }

        resultLabel_->setText(tr("%1 image(s) would be added to the project in group <").arg(numberToImage_.keys().size())
                + importGroup_ + tr(">"));
        resultLabel_->setWordWrap(true);
    }

    void createFilesTable() {
        resultsTable_ = new QTableWidget(0, 3);
        resultsTable_->setSelectionBehavior(QAbstractItemView::SelectRows);

        QStringList labels;
        labels << tr("Target Number") << tr("Found Image") << tr("Found Stack");
        resultsTable_->setHorizontalHeaderLabels(labels);
        resultsTable_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
        resultsTable_->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
        resultsTable_->verticalHeader()->hide();
        resultsTable_->setShowGrid(false);
        resultsTable_->setAlternatingRowColors(true);
    }

    QString commitIntToStringLength(int num, int length) {
        QString value = QString::number(num);
        int diff = length - value.size();
        for (int i = 0; i < diff; ++i) {
            value.push_front('0');
        }
        return value;
    }

    QMap<QString, QString> numberToImage_;
    QMap<QString, QString> numberToMovie_;

    QString importImagesPath_;
    QString importMoviesPath_;
    QString importGroup_;

    //Widgets
    QTableWidget *resultsTable_;
    QLabel *resultLabel_;

};

#endif /* IMPORT_RESULT_HPP */

