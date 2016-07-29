#include "libraryImageStatus.h"

libraryImageStatus::libraryImageStatus(projectModel* model, QWidget* parent) 
: QWidget(parent) {
    projModel = model;
    
    QGroupBox* dataBox = new QGroupBox("Image Data");
    dataLayout = fillFormLayout(QStringList() << "Defocus" << "Magnification" << "Sample Pixel" << "Comment");
    dataBox->setLayout(dataLayout);
    
    QGroupBox* qvalBox = new QGroupBox("Q-Values");
    qvalLayout = fillFormLayout(QStringList() << "Latest" << "Unbend II" << "Movie A" << "Movie B");
    qvalBox->setLayout(qvalLayout);
    
    QGroupBox* tiltBox = new QGroupBox("Tilt Info");
    tiltLayout = fillFormLayout(QStringList() << "Grid Axis" << "Grid Angle" << "Crystal Axis" << "Crystal Angle");
    tiltBox->setLayout(tiltLayout);
    
    QGroupBox* mergeBox = new QGroupBox("Merge Info");
    mergeLayout = fillFormLayout(QStringList() << "Phase Residual" << "TAXA Change" << "TANGL Change" << "Phase Origin Change");
    mergeBox->setLayout(mergeLayout);
    
    QGridLayout* dataWidgetLayout = new QGridLayout;
    dataWidgetLayout->addWidget(dataBox, 0, 0);
    dataWidgetLayout->addWidget(qvalBox, 0, 1);
    dataWidgetLayout->addWidget(tiltBox, 1, 0);
    dataWidgetLayout->addWidget(mergeBox, 1, 1);
    
    dataWidget = new QWidget;
    dataWidget->setLayout(dataWidgetLayout);
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

QFormLayout* libraryImageStatus::fillFormLayout(const QStringList& labels) {
    QFormLayout* dataLayout = new QFormLayout;
    dataLayout->setHorizontalSpacing(20);
    for(int i=0; i< labels.size(); ++i) {
        QLabel* valueLabel = new QLabel;
        dataLayout->addRow(labels[i], valueLabel);
    }
    
    return dataLayout;
}

void libraryImageStatus::updateData() {
    if(!projModel->isCurrentRowValidImage()) {
        dataWidget->hide();
        selectImageLabel->show();
        return;
    }
    
    updateFormData(dataLayout, QStringList() << "defocus" << "magnification" << "sample_pixel" << "comment");
    updateFormData(qvalLayout, QStringList() << "QVAL" << "QVAL2" << "QVALMA" << "QVALMB");
    updateFormData(tiltLayout, QStringList() << "tltaxa" << "tltang" << "taxa" << "tangl");
    updateFormData(mergeLayout, QStringList() << "MergePhaseResidual" << "taxa_change" << "tangl_change" << "phaori_last_change");
    dataWidget->show();
    selectImageLabel->hide();
    
}

void libraryImageStatus::updateFormData(QFormLayout* layout, const QStringList& params) {
    for(int i=0; i< params.size(); ++i) {
        QLabel* label = static_cast<QLabel*>(layout->itemAt(i, QFormLayout::FieldRole)->widget());
        QVariant valueVar = projModel->getCurrentRowParameterValue(params[i]);
        QString valueStr = valueVar.toString();
        if(valueVar.canConvert<double>()) valueStr = QString::number(valueVar.toDouble(), 'f', 2);
        if(label != NULL) label->setText(valueStr);
    }
}


