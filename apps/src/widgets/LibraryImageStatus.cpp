#include "LibraryImageStatus.h"
#include "ApplicationData.h"
#include "ProjectData.h"

LibraryImageStatus::LibraryImageStatus(ProjectModel* model, QWidget* parent) 
: QWidget(parent) {
    projModel = model;
    
    this->readParamsList();
    
    dataWidget = new QGroupBox("Image Data");
    dataLayout = fillFormLayout();
    dataWidget->setLayout(dataLayout);
    dataWidget->hide();
    
    selectImageLabel = new QLabel("Select image from the list");
    QFont font = selectImageLabel->font();
    font.setBold(true);
    font.setPointSize(20);
    selectImageLabel->setFont(font);
    
    QPalette pal = selectImageLabel->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    selectImageLabel->setPalette(pal);
    
    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(selectImageLabel, 0, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    mainLayout->addWidget(dataWidget, 1, 0);
    
    setLayout(mainLayout);
}

QFormLayout* LibraryImageStatus::fillFormLayout() {
    QFormLayout* dataLayout = new QFormLayout;
    dataLayout->setRowWrapPolicy(QFormLayout::DontWrapRows);
    dataLayout->setFieldGrowthPolicy(QFormLayout::FieldsStayAtSizeHint);
    dataLayout->setFormAlignment(Qt::AlignTop);
    dataLayout->setLabelAlignment(Qt::AlignLeft);
    dataLayout->setHorizontalSpacing(20);
    dataLayout->setVerticalSpacing(5);
    
    for(int i=0; i< labelsList.size(); ++i) {
        QLabel* valueLabel = new QLabel;
        valueLabel->setWordWrap(true);
        dataLayout->addRow(labelsList[i], valueLabel);
        QLabel* label = static_cast<QLabel*>(dataLayout->itemAt(i, QFormLayout::LabelRole)->widget());
        QFont font = label->font();
        font.setBold(true);
        label->setFont(font);
    }
    
    return dataLayout;
}

void LibraryImageStatus::updateData() {
    if(!projModel->isCurrentRowValidImage()) {
        dataWidget->hide();
        selectImageLabel->show();
        return;
    }
    
    updateFormData();
    dataWidget->show();
    selectImageLabel->hide();  
}

void LibraryImageStatus::updateFormData() {
    
    for(int i=0; i< labelsList.size(); ++i) {
        if(i < paramsList.size()) {
            QLabel* label = static_cast<QLabel*>(dataLayout->itemAt(i, QFormLayout::FieldRole)->widget());
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


