#include <QtWidgets>

#include "ProjectData.h"
#include "ParameterConfiguration.h"
#include "ImageLibrary.h"

ImageLibrary::ImageLibrary(QWidget* parent)
: QWidget(parent){
    QWidget* header = setupHeader();
    
    thumbnails_ = new ImageThumbnails;
    thumbnails_->setFixedHeight(CONTENT_HEIGHT);
    
    connect(thumbnails_, &ImageThumbnails::clicked, [=](const QModelIndex& i){
        updateData(thumbnails_->getPath(i.column()));
    });
    
    connect(thumbnails_, &ImageThumbnails::doubleClicked, [=](const QModelIndex& i){
        emit shouldLoadImage(thumbnails_->getPath(i.column()));
    });
    
    connect(&projectData, &ProjectData::selectionChanged, [=] (const QStringList& sel) {
        setSelectionCount(sel.count());
    });
    
    dataBox = new QFrame();
    dataBox->setFrameShadow(QFrame::Plain);
    dataBox->setFrameShape(QFrame::StyledPanel);
    dataLayout = fillFormLayout(QStringList() << "Defocus" << "QVal" << "TAXA" << "TANGL");
    dataBox->setLayout(dataLayout);   
    
    expandedWidget = new QWidget;
    expandedWidget->setFixedHeight(CONTENT_HEIGHT);
    QGridLayout* expandedWidgetLayout = new QGridLayout;
    expandedWidgetLayout->setMargin(0);
    expandedWidgetLayout->setSpacing(0);
    expandedWidgetLayout->addWidget(thumbnails_, 0, 0);
    expandedWidgetLayout->addWidget(dataBox, 0, 1);
    expandedWidget->setLayout(expandedWidgetLayout);
    
    QGridLayout* mainLayout = new QGridLayout;
    setLayout(mainLayout);
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(header, 0, 0);
    mainLayout->addWidget(expandedWidget, 1, 0);
    
    setSelectionCount(thumbnails_->getSlectionCount());
    infoButton->setChecked(false);
    expandedWidget->setVisible(false);
    setFixedHeight(HEADER_HEIGHT);
    updateData();
    update();
}

QWidget* ImageLibrary::setupHeader() {
    QFrame* bar = new QFrame(this);
    bar->setAutoFillBackground(true);
    bar->setFrameShadow(QFrame::Plain);
    bar->setFrameShape(QFrame::StyledPanel);
    
    //Set Height
    bar->setFixedHeight(HEADER_HEIGHT);
    
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
    
    QToolButton* reloadButton = new QToolButton();
    reloadButton->setIcon(ApplicationData::icon("refresh"));
    reloadButton->setToolTip("Update Thumbnails");
    reloadButton->setAutoRaise(false);
    reloadButton->setCheckable(false);
    reloadButton->setVisible(false);
    connect(reloadButton, &QToolButton::clicked, [=] (){
        thumbnails_->updateThumbanils();
        update();
    });
    
    infoButton = new QToolButton();
    infoButton->setIcon(ApplicationData::icon("expand"));
    infoButton->setToolTip("Show More Information");
    infoButton->setAutoRaise(false);
    infoButton->setCheckable(true);
    connect(infoButton, &QToolButton::toggled, [=] (bool show){
        expandedWidget->setVisible(show);
        reloadButton->setVisible(show);
        if(show) {
            setFixedHeight(HEADER_HEIGHT+CONTENT_HEIGHT);
            thumbnails_->updateThumbanils();
        }
        else setFixedHeight(HEADER_HEIGHT);
        update();
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
    headerLayout->addWidget(reloadButton, 0, 4, 1, 1, Qt::AlignRight | Qt::AlignVCenter);
    headerLayout->addWidget(infoButton, 0, 5, 1, 1, Qt::AlignRight | Qt::AlignVCenter);
    
    bar->setLayout(headerLayout);
    
    return bar;
}

QGridLayout* ImageLibrary::fillFormLayout(const QStringList& labels) {
    valueLabels.clear();
    QGridLayout* dataLayout = new QGridLayout;
    dataLayout->setMargin(5);
    dataLayout->setSpacing(3);
    
    imageLabel = new QLabel("No image selected. Please select one from list.");
    imageLabel->setAlignment(Qt::AlignCenter);
    QFont titleFont = imageLabel->font();
    titleFont.setBold(true);
    titleFont.setItalic(true);
    imageLabel->setFont(titleFont);
    dataLayout->addWidget(imageLabel, 0, 0, 1, labels.size(), Qt::AlignCenter);
    for(int i=0; i< labels.size(); ++i) {
        QLabel* valueLabel = new QLabel;
        QLabel* nameLabel = new QLabel(labels[i]);
        QFont font = nameLabel->font();
        font.setBold(true);
        nameLabel->setFont(font);
        dataLayout->addWidget(nameLabel, 1, i, Qt::AlignHCenter | Qt::AlignTop);
        dataLayout->addWidget(valueLabel, 2, i, Qt::AlignHCenter | Qt::AlignTop);
        valueLabels.append(valueLabel);
    }
    
    return dataLayout;
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
    QString imageName = projectData.projectDir().relativeFilePath(imagePath);
    if(imageName.length() > 30) imageName = "..." + imageName.remove(0, imageName.length()-30);
    
    imageLabel->setText(imageName);
    
    QString color = data->getValue("image_flag");
    QPalette pal = imageLabel->palette();
    if(color == "red") pal.setColor(QPalette::WindowText, Qt::red);
    else if(color == "green") pal.setColor(QPalette::WindowText, Qt::green);
    else if(color == "blue") pal.setColor(QPalette::WindowText, Qt::blue);
    else if(color == "gold") pal.setColor(QPalette::WindowText, Qt::darkYellow);
    else pal.setColor(QPalette::WindowText, Qt::black);
    
    imageLabel->setPalette(pal);
    
    for(int i=0; i< params.size(); ++i) {
        QVariant valueVar = data->get(params[i])->value();
        QString valueStr = valueVar.toString();
        if(valueVar.canConvert<double>()) valueStr = QString::number(valueVar.toDouble(), 'f', 2);
        if(valueLabels[i]) valueLabels[i]->setText(valueStr);
    }
}

void ImageLibrary::setHeaderTitle(const QString& title) {
    headerTitle->setText(title.toUpper());
}


void ImageLibrary::setSelectionCount(int count) {
    selectionLabel->setText(QString::number(count) + " Images Selected");
}
