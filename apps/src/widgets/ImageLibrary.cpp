#include <QtWidgets>

#include "ProjectData.h"
#include "ParameterConfiguration.h"
#include "ImageLibrary.h"

ImageLibrary::ImageLibrary(QWidget* parent)
: QWidget(parent){
    watcher.setFile(projectData.selectionDirfile());
    timer.setSingleShot(true);
    
    QWidget* header = setupHeader();
    
    thumbnails_ = new ImageThumbnails;
    thumbnails_->setFixedHeight(80);
    
    connect(thumbnails_, &ImageThumbnails::clicked, [=](const QModelIndex& i){
        updateData(thumbnails_->getPath(i.column()));
    });
    
    connect(thumbnails_, &ImageThumbnails::doubleClicked, [=](const QModelIndex& i){
        emit shouldLoadImage(thumbnails_->getPath(i.column()));
    });
    
    connect(thumbnails_, &ImageThumbnails::selectionCountChanged, [=] (int count) {
        setSelectionCount(count);
    });
    
    dataBox = new QGroupBox();
    dataLayout = fillFormLayout(QStringList() << "Defocus" << "QVal" << "TAXA" << "TANGL");
    dataBox->setLayout(dataLayout);   
    
    expandedWidget = new QWidget;
    QGridLayout* expandedWidgetLayout = new QGridLayout;
    expandedWidgetLayout->setMargin(0);
    expandedWidgetLayout->setSpacing(5);
    expandedWidgetLayout->addWidget(thumbnails_, 0, 0);
    expandedWidgetLayout->addWidget(dataBox, 0, 1);
    expandedWidget->setLayout(expandedWidgetLayout);
    
    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(header, 0, 0);
    mainLayout->addWidget(expandedWidget, 1, 0);
    setLayout(mainLayout);
    
    connect(&watcher, SIGNAL(fileChanged(const QString &)), this, SLOT(timedLoad()));
    connect(&timer, SIGNAL(timeout()), this, SLOT(updateChecks()));
    
    setSelectionCount(thumbnails_->getSlectionCount());
    infoButton->setChecked(false);
    expandedWidget->setVisible(false);
    updateData();
    updateChecks();
}

QWidget* ImageLibrary::setupHeader() {
    QWidget* bar = new QWidget(this);
    bar->setAutoFillBackground(true);
    
    //Set Height
    bar->setFixedHeight(24);
    
    //Setup Label
    headerTitle = new QLabel("", bar);
    headerTitle->setAutoFillBackground(false);
    headerTitle->setAlignment(Qt::AlignLeft);
    
    QFont font;
    font.setBold(true);
    font.setStretch(QFont::SemiExpanded);
    font.setCapitalization(QFont::Capitalize);
    headerTitle->setFont(font);
    
    //Setup header Widget
    selectionLabel = new QLabel;
    QPalette titlePal(selectionLabel->palette());
    titlePal.setColor(QPalette::WindowText, Qt::darkGray);
    selectionLabel->setPalette(titlePal);
    
    setSelectionCount(0);
    
    infoButton = new QToolButton();
    infoButton->setIcon(ApplicationData::icon("expand"));
    infoButton->setToolTip("Show More Information");
    infoButton->setAutoRaise(false);
    infoButton->setCheckable(true);
    connect(infoButton, &QToolButton::toggled, [=] (bool show){
        expandedWidget->setVisible(show);
    });
    
    //Setup spacer
    QWidget* s = new QWidget();
    s->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    
    //setup layout
    QGridLayout* headerLayout = new QGridLayout(bar);
    headerLayout->setSpacing(3);
    headerLayout->setMargin(3);
    
    //Add widgets
    headerLayout->addItem(new QSpacerItem(3,3),0,0,1,1);
    headerLayout->addWidget(headerTitle, 0, 1, 1, 1, Qt::AlignLeft | Qt::AlignVCenter);
    headerLayout->addWidget(s, 0, 2, 1, 1);
    headerLayout->addWidget(selectionLabel, 0, 3, 1, 1, Qt::AlignRight | Qt::AlignVCenter);
    headerLayout->addWidget(infoButton, 0, 4, 1, 1, Qt::AlignRight | Qt::AlignVCenter);
    
    bar->setLayout(headerLayout);
    
    return bar;
}

QGridLayout* ImageLibrary::fillFormLayout(const QStringList& labels) {
    valueLabels.clear();
    QGridLayout* dataLayout = new QGridLayout;
    for(int i=0; i< labels.size(); ++i) {
        QLabel* valueLabel = new QLabel;
        QLabel* nameLabel = new QLabel(labels[i]);
        QFont font = nameLabel->font();
        font.setBold(true);
        nameLabel->setFont(font);
        dataLayout->addWidget(nameLabel, 0, i, Qt::AlignCenter);
        dataLayout->addWidget(valueLabel, 1, i, Qt::AlignCenter);
        valueLabels.append(valueLabel);
    }
    
    return dataLayout;
}

void ImageLibrary::updateChecks() {
    QFile s(projectData.selectionDirfile());
    
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Dirfile read failed.";
        return;
    }

    QStringList checkdImages;
    QString projectDir = projectData.projectDir().canonicalPath();
    while (!s.atEnd()) checkdImages << projectDir + '/' + s.readLine().simplified();
    s.close();
    
    thumbnails_->updateChecks(checkdImages);
}

void ImageLibrary::updateData(const QString& imagePath) {
    if(imagePath.isEmpty()) {
        dataBox->hide();
        return;
    }
    
    updateFormData(imagePath, QStringList() << "defocus" << "QVAL" << "taxa" << "tangl");
    dataBox->show();
    
}

void ImageLibrary::updateFormData(const QString& imagePath, const QStringList& params) {
    ParametersConfiguration* data = projectData.parameterData(imagePath);
    dataBox->setTitle(projectData.projectDir().relativeFilePath(imagePath));
    for(int i=0; i< params.size(); ++i) {
        QVariant valueVar = data->get(params[i])->value();
        QString valueStr = valueVar.toString();
        if(valueVar.canConvert<double>()) valueStr = QString::number(valueVar.toDouble(), 'f', 2);
        if(valueLabels[i]) valueLabels[i]->setText(valueStr);
    }
}

void ImageLibrary::timedLoad() {
    timer.start(100);
}

void ImageLibrary::setHeaderTitle(const QString& title) {
    headerTitle->setText(title.toUpper());
}


void ImageLibrary::setSelectionCount(int count) {
    selectionLabel->setText(QString::number(count) + " Images Selected");
}
