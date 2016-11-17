/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   import_images.hpp
 * Author: biyanin
 *
 * Created on June 10, 2016, 11:07 AM
 */

#ifndef IMPORT_IMAGES_WIZARD_PAGE_HPP
#define IMPORT_IMAGES_WIZARD_PAGE_HPP

#include <QWizardPage>
#include <QFormLayout>
#include <QComboBox>
#include <QString>
#include <QStringList>
#include <QLineEdit>

#include "project_preferences.h"
#include "ApplicationData.h"
#include "ProjectData.h"

#include "browser_widget.h"
#include "combo_input_widget.h"
#include "edit_set_widget.h"

class ImportOptionsWizardPage : public QWizardPage {

    Q_OBJECT

public:
    ImportOptionsWizardPage(QWidget* parent = NULL)
    : QWizardPage(parent) {

        setTitle("Import images and movies");
        setSubTitle(QString("Select the directory containing averaged images") +
                QString(" and (optionally) select the corresponding movie/stacks") +
                QString(" directory. Note that the names of the corresponding images and stacks") +
                QString(" should be same for correct import"));
        
        QString projectPath = projectData.projectPath();
        
        BrowserWidget* filesDirBrowser_ = new BrowserWidget(BrowserWidget::BrowseType::DIRECTORY);
        QString currImagePath = ProjectPreferences(projectPath).importImageDir();
        if (currImagePath != "") filesDirBrowser_->setPath(currImagePath);
        connect(filesDirBrowser_, &BrowserWidget::pathChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportImageDir(value);});


        BrowserWidget* moviesDirBrowser_ = new BrowserWidget(BrowserWidget::BrowseType::DIRECTORY);
        QString currMoviePath = ProjectPreferences(projectPath).importMovieDir();
        if (currMoviePath != "") moviesDirBrowser_->setPath(currMoviePath);
        connect(moviesDirBrowser_, &BrowserWidget::pathChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportMoiveDir(value);});

        EditSetWidget* ignoreImagePattern = new EditSetWidget;
        ignoreImagePattern->setValue(ProjectPreferences(projectPath).importIgnorePattern());
        connect(ignoreImagePattern, &EditSetWidget::valueChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportIgnorePattern(value);});

        ComboInputWidget* addToGroup_ = new ComboInputWidget();
        addToGroup_->setEditable(true);
        addToGroup_->addItems(imageGroups());
        addToGroup_->setCurrentText(ProjectPreferences(projectPath).importGroup());
        addToGroup_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        connect(addToGroup_, static_cast<void(QComboBox::*)(const QString &)> (&QComboBox::currentTextChanged),
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportGroup(value);});

        EditIntSetWidget* numberLength_ = new EditIntSetWidget();
        numberLength_->setAllRanges(5, 10);
        int pastNumberLength = ProjectPreferences(projectPath).importImageLength();
        if (pastNumberLength >= 5 && pastNumberLength <= 10) numberLength_->setValue(QString::number(pastNumberLength));
        else {
            numberLength_->setValue("5");
            ProjectPreferences(projectPath).setImportImageLength(5);
        }
        connect(numberLength_, &EditSetWidget::valueChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportImageLength(value.toInt());});

        QFormLayout* layout = new QFormLayout;
        layout->setRowWrapPolicy(QFormLayout::WrapLongRows);
        layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
        layout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
        layout->setLabelAlignment(Qt::AlignRight);
        layout->addRow("Averaged Images directory", filesDirBrowser_);
        layout->addRow("Stacks directory", moviesDirBrowser_);
        layout->addRow("String to be ignored at the end of image file names", ignoreImagePattern);
        layout->addRow("Add the imported images to group", addToGroup_);
        layout->addRow("Length of the target image number string (>=5 & <=10)", numberLength_);

        setLayout(layout);

    }

private:

    QStringList imageGroups() {
        QString projectFolder = projectData.projectPath();

        //Get the list of current folders
        QStringList imageFolders = QDir(projectFolder).entryList(QDir::NoDotAndDotDot | QDir::Dirs);

        //Remove non-linked folders
        QString imageFolder;

        foreach(imageFolder, imageFolders) {
            if (!QFile(projectFolder + "/" + imageFolder + "/2dx_master.cfg").exists()) imageFolders.removeAll(imageFolder);
        }
        
        return imageFolders;
    }


};


#endif

