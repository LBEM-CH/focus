#include <QDir>

#include "LibraryImageStatus.h"
#include "ApplicationData.h"
#include "ProjectData.h"
#include "GroupContainer.h"

LibraryImageStatus::LibraryImageStatus(ProjectModel* model, QWidget* parent) 
: QWidget(parent) {
    projModel = model;
    
    this->readParamsList();
    
    imageLabel = new QLabel();
    imageLabel->setAlignment(Qt::AlignCenter);
    QFont font = imageLabel->font();
    font.setBold(true);
    font.setItalic(true);
    imageLabel->setFont(font);
    
    dataLayout = fillFormLayout();
    setLayout(dataLayout);
}

QFormLayout* LibraryImageStatus::fillFormLayout() {
    QFormLayout* dataLayout = new QFormLayout;
    dataLayout->setRowWrapPolicy(QFormLayout::DontWrapRows);
    dataLayout->setFieldGrowthPolicy(QFormLayout::FieldsStayAtSizeHint);
    dataLayout->setFormAlignment(Qt::AlignTop);
    dataLayout->setLabelAlignment(Qt::AlignRight);
    dataLayout->setHorizontalSpacing(20);
    dataLayout->setVerticalSpacing(5);
    
    dataLayout->addRow(imageLabel);
    for(int i=0; i< labelsList.size(); ++i) {
        QLabel* valueLabel = new QLabel;
        valueLabel->setWordWrap(true);
        dataLayout->addRow(labelsList[i], valueLabel);
        QLabel* label = static_cast<QLabel*>(dataLayout->itemAt(i+1, QFormLayout::LabelRole)->widget());
        QFont font = label->font();
        font.setBold(true);
        label->setFont(font);
    }
    
    return dataLayout;
}

void LibraryImageStatus::updateData() {
    updateFormData();
}

void LibraryImageStatus::updateFormData() {
    imageLabel->setText(QDir(projModel->getCurrentRowPath()).dirName());
    for(int i=0; i< labelsList.size(); ++i) {
        if(i < paramsList.size()) {
            QLabel* label = static_cast<QLabel*>(dataLayout->itemAt(i+1, QFormLayout::FieldRole)->widget());
            if(label != NULL) label->setText(projModel->getCurrentRowParameterValue(paramsList[i]).toString());
        }
    } 
}

void LibraryImageStatus::readParamsList() {
    QFile s(ApplicationData::configDir().canonicalPath() + "/lib.params.list");
    
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "ERROR: Lib Params file read failed: " << ApplicationData::configDir().canonicalPath()+ "/lib.params.list";
        return;
    }

    while (!s.atEnd()) {
        QString line = s.readLine().simplified();
        QStringList cell = line.split(':');
        if(cell.size() == 1) {
            cell.append(cell[0]);
        }
        paramsList.append(cell[0].trimmed());
        labelsList.append(cell[1].trimmed());
    }
    s.close();
}


