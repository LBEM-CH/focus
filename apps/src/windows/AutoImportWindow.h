#ifndef AUTOIMPORTWINDOW_H
#define AUTOIMPORTWINDOW_H

#include <QDialog>
#include <QDebug>

#include "BlockContainer.h"
#include "BrowserWidget.h"
#include "LineEditSet.h"
#include "NoScrollComboBox.h"
#include "ProjectData.h"

class AutoImportWindow : public QDialog {
public:

    AutoImportWindow(QWidget* parent)
    : QDialog(parent) {
    }

private:

    BlockContainer* setupOptionsContainter() {
        QDir projectPath = projectData.projectDir();

        BrowserWidget* filesDirBrowser_ = new BrowserWidget(BrowserWidget::BrowseType::DIRECTORY);
        QString currImagePath = ProjectPreferences(projectPath).importImageDir();
        if (currImagePath != "") filesDirBrowser_->setPath(currImagePath);
        connect(filesDirBrowser_, &BrowserWidget::pathChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportImageDir(value);});

        LineEditSet* moviesDir = new LineEditSet;
        moviesDir->setValue("aligned");
        connect(moviesDir, &LineEditSet::valueChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportIgnorePattern(value);});
                
        LineEditSet* rawDir = new LineEditSet;
        rawDir->setValue("raw");
        connect(rawDir, &LineEditSet::valueChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportIgnorePattern(value);});

        LineEditSet* ignoreImagePattern = new LineEditSet;
        ignoreImagePattern->setValue(ProjectPreferences(projectPath).importIgnorePattern());
        connect(ignoreImagePattern, &LineEditSet::valueChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportIgnorePattern(value);});

        NoScrollComboBox* addToGroup_ = new NoScrollComboBox();
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
        connect(numberLength_, &LineEditSet::valueChanged,
                [ = ] (const QString & value){ProjectPreferences(projectPath).setImportImageLength(value.toInt());});

        QFormLayout* layout = new QFormLayout;
        layout->setRowWrapPolicy(QFormLayout::WrapLongRows);
        layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
        layout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
        layout->setLabelAlignment(Qt::AlignRight);
        layout->addRow("Images directory", filesDirBrowser_);
        layout->addRow("Subdirectory containing aligned movies", moviesDir);
        layout->addRow("Subdirectory containing raw movies", rawDir);
        layout->addRow("String to be ignored at the end of image file names", ignoreImagePattern);
        layout->addRow("Add the imported images to group", addToGroup_);
        layout->addRow("Length of the target image number string (>=5 \& <=10)", numberLength_);

        QWidget* parameters = new QWidget;
        parameters->setLayout(layout);

        //Setup advanced level button
        QRadioButton* advancedLevelButton = new QRadioButton();
        advancedLevelButton->setFixedSize(20, 20);

        QWidget* parameterLevelWidget = new QWidget();
        QHBoxLayout* parameterLevelLayout = new QHBoxLayout(parameterLevelWidget);
        parameterLevelLayout->setMargin(0);
        parameterLevelLayout->setSpacing(0);
        parameterLevelWidget->setLayout(parameterLevelLayout);

        parameterLevelLayout->addWidget(advancedLevelButton, 0, Qt::AlignVCenter);
        parameterLevelLayout->addWidget(new QLabel("Automatically import new images"), 0, Qt::AlignVCenter);

        //Setup the window and add widgets
        BlockContainer* parameterContainer = new BlockContainer("Setup", parameters, parameterLevelWidget);
        parameterContainer->setMinimumWidth(400);
        parameterContainer->setMinimumHeight(200);

        return parameterContainer;
    }

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

#endif /* AUTOIMPORTWINDOW_H */

