#include <QtWidgets>

#include "ImageViewer.h"
#include "ProjectData.h"
#include "OverviewWidget.h"
#include "ProjectPreferences.h"
#include "ApplicationData.h"

OverviewWidget::OverviewWidget(QWidget* parent) :
QWidget(parent) {
    properties_ = OverviewSettings().overviews();
    
    QGridLayout* previewsGridLayout = new QGridLayout;
    previewsGridLayout->setMargin(0);
    previewsGridLayout->setSpacing(10);
    previewsGridLayout->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
    
    for(int i=1; i<6; i+=2 ){
        for(int j=0; j<2; ++j) {
            //Create the preview
            ImageViewer* preview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), "Not found");
            previewList_.append(preview);
            
            //Create the title label
            QLabel* label =  new QLabel;
            label->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
            QFont font = label->font();
            font.setBold(true);
            label->setFont(font);
            previewLabels_.append(label);
            
            //Create the maximize button
            GraphicalButton* maximizeButton = new GraphicalButton(ApplicationData::icon("maximize"));
            maximizeButton->setFixedSize(16, 16);
            maximizeButton->setCheckable(true);
            connect(maximizeButton, &QAbstractButton::toggled, [=](bool check) {
                maximizePreview(preview, check);
            });
            
            //Add them to a layout
            QHBoxLayout* layout = new QHBoxLayout();
            layout->addStretch(0);
            layout->addWidget(label, 0);
            layout->addWidget(maximizeButton, 0);
            layout->addStretch(0);
            QWidget* widget =  new QWidget;
            widget->setLayout(layout);
            previewTitleWidgets_.append(widget);
            
            //Add the preview to grid
            previewsGridLayout->addWidget(preview, i, j);
        }
    }
    
    previewsGridLayout->addWidget(previewTitleWidgets_[0], 0, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    previewsGridLayout->addWidget(previewTitleWidgets_[1], 0, 1, Qt::AlignHCenter | Qt::AlignVCenter);
    previewsGridLayout->addWidget(previewTitleWidgets_[2], 2, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    previewsGridLayout->addWidget(previewTitleWidgets_[3], 2, 1, Qt::AlignHCenter | Qt::AlignVCenter);
    previewsGridLayout->addWidget(previewTitleWidgets_[4], 4, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    previewsGridLayout->addWidget(previewTitleWidgets_[5], 4, 1, Qt::AlignHCenter | Qt::AlignVCenter);
    
    previewGridWidget_ = new QWidget;
    previewGridWidget_->setLayout(previewsGridLayout);
    // previewGridWidget_->setMinimumSize(425, 510);
    previewGridWidget_->setMinimumSize(425, 765);
    // previewGridWidget_->setMaximumSize(825, 910);
    previewGridWidget_->setMaximumSize(825, 1365);
    
    QVBoxLayout* mainLayout = new QVBoxLayout();
    mainLayout->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
    mainLayout->setMargin(5);
    mainLayout->setSpacing(10);
    mainLayout->addStretch(0);
    
    overviewLabel_ = new QLabel;
    overviewLabel_->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
    QFont font = overviewLabel_->font();
    font.setBold(true);
    font.setPixelSize(18);
    overviewLabel_->setFont(font);
    
    rightButton_ = new GraphicalButton(ApplicationData::icon("move_right"));
    rightButton_->setFixedSize(32, 32);
    connect(rightButton_, &GraphicalButton::clicked, [=]() {
        overviewIndex_ = (overviewIndex_ + 1)%properties_.size();
        ProjectPreferences().setOverviewIndex(overviewIndex_);
        resetOverview();
    });
    
    leftButton_ = new GraphicalButton(ApplicationData::icon("move_left"));
    leftButton_->setFixedSize(32, 32);
    connect(leftButton_, &GraphicalButton::clicked, [=]() {
        overviewIndex_ = (overviewIndex_ + properties_.size() - 1)%properties_.size();
        ProjectPreferences().setOverviewIndex(overviewIndex_);
        resetOverview();
    });
    
    QHBoxLayout* changerLayout = new QHBoxLayout;
    changerLayout->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
    changerLayout->setMargin(0);
    changerLayout->setSpacing(0);
    changerLayout->addWidget(leftButton_, 0);
    changerLayout->addWidget(overviewLabel_, 1);
    changerLayout->addWidget(rightButton_, 0);
    
    overlayWidgets_ = new QStackedWidget;
    overlayWidgets_->addWidget(new ImageViewer(projectData.projectWorkingDir().canonicalPath()));
    overlayWidgets_->addWidget(new ImageViewer(projectData.projectWorkingDir().canonicalPath()));
    overlayWidgets_->setCurrentIndex(0);
    overlayWidgets_->hide();
    
    showHeaderButton_ = new QToolButton();
    showHeaderButton_->setIcon(ApplicationData::icon("header_info"));
    showHeaderButton_->setText("Show Header/Info");
    showHeaderButton_->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
    showHeaderButton_->setToolTip("Toggle Image Header/Info");
    showHeaderButton_->setCheckable(true);
    showHeaderButton_->setChecked(false);
    connect(showHeaderButton_, &QToolButton::toggled, [=] (bool check) {
	if(check) showHeaderButton_->setText("Show Image Preview");
        else showHeaderButton_->setText("Show Header/Info");
        setPreviewImages();
    });
    
    overlayButton_ = new QToolButton();
    overlayButton_->setIcon(ApplicationData::icon("overlay"));
    overlayButton_->setText("Activate Overlay");
    overlayButton_->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
    overlayButton_->setToolTip("Auto switch between two images, creates a overlay effect");
    overlayButton_->setCheckable(true);
    overlayButton_->setChecked(false);
    connect(overlayButton_, &QToolButton::toggled, this, &OverviewWidget::activateOverlay);
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->setSpacing(0);
    buttonLayout->setMargin(0);
    buttonLayout->addWidget(overlayButton_);
    buttonLayout->addWidget(showHeaderButton_);
    
    mainLayout->addLayout(changerLayout, 0);
    mainLayout->addWidget(previewGridWidget_, 1);
    mainLayout->addWidget(overlayWidgets_, 1);
    mainLayout->addLayout(buttonLayout, 0);
    
    setLayout(mainLayout);
    
    connect(&overlayTimer_, &QTimer::timeout, this, &OverviewWidget::changeOverlaidWidget);
    
    overviewIndex_ = 0;
    if(ProjectPreferences().overviewIndex() < properties_.size()) overviewIndex_ = ProjectPreferences().overviewIndex();
    resetOverview();
}

void OverviewWidget::resetOverview() {
    if(overviewIndex_ < properties_.size()) {
        OverviewSettings::OverviewProps props = properties_[overviewIndex_];
        overviewLabel_->setText(props.title);
        previewLabels_[0]->setText(props.frameTitles[0]);
        previewLabels_[1]->setText(props.frameTitles[1]);
        previewLabels_[2]->setText(props.frameTitles[2]);
        previewLabels_[3]->setText(props.frameTitles[3]);
        previewLabels_[4]->setText(props.frameTitles[4]);
        previewLabels_[5]->setText(props.frameTitles[5]);
    
        setPreviewImages();
        
        if(props.overlay.isEmpty()) overlayButton_->hide();
        else overlayButton_->show();
    }
}

void OverviewWidget::setPreviewImages() {
    QString imagePath = currentImagePath_;
    if (overviewIndex_ < properties_.size() && QFileInfo(imagePath + "/2dx_image.cfg").exists()) {
        ParametersConfiguration* imageConf = projectData.parameterData(QDir(imagePath));
        QStringList paths = properties_[overviewIndex_].framePaths;
        if (previewGridWidget_->isVisible()) {
            for (int i = 0; i < 6; i++) {
                QString parsedPath = parseVariables(paths[i], imageConf);
                QString extension = QFileInfo(parsedPath).suffix();
                previewList_[i]->setWorkDir(imagePath);
                previewList_[i]->loadFile(imagePath + "/" + parsedPath, extension, showHeaderButton_->isChecked());
            }
        }

        if (overlayWidgets_->isVisible()) {
            QString overlay = properties_[overviewIndex_].overlay;
            QString path1 = parseVariables(paths[QString(overlay.at(0)).toInt()-1], imageConf);
            QString path2 = parseVariables(paths[QString(overlay.at(1)).toInt()-1], imageConf);
            static_cast<ImageViewer*> (overlayWidgets_->widget(0))->setWorkDir(imagePath);
            static_cast<ImageViewer*> (overlayWidgets_->widget(1))->setWorkDir(imagePath);
            static_cast<ImageViewer*> (overlayWidgets_->widget(0))->loadFile(imagePath + "/" + path1, QFileInfo(path1).suffix(), showHeaderButton_->isChecked());
            static_cast<ImageViewer*> (overlayWidgets_->widget(1))->loadFile(imagePath + "/" + path2, QFileInfo(path2).suffix(), showHeaderButton_->isChecked());
        }
    }

}

void OverviewWidget::setCurrentImagePath(const QString& imagePath) {
    currentImagePath_ = imagePath;
}

void OverviewWidget::activateOverlay(bool play) {
    if (play) {
        overlayTimer_.start(1000);
        previewGridWidget_->hide();
        rightButton_->hide();
        leftButton_->hide();
        overlayWidgets_->show();
        overlayButton_->setText("Deactivate Overlay");
        setPreviewImages();
    }
    else {
        overlayTimer_.stop();
        overlayWidgets_->hide();
        rightButton_->show();
        leftButton_->show();
        previewGridWidget_->show();
        previewGridWidget_->update();
        overlayButton_->setText("Activate Overlay");
        resetOverview();
    }
}

void OverviewWidget::changeOverlaidWidget() {
    int id = (overlayWidgets_->currentIndex() + 1) % 2;
    overlayWidgets_->setCurrentIndex(id);
    QStringList titles = properties_[overviewIndex_].frameTitles;
    QString overlay = properties_[overviewIndex_].overlay;
    if(id == 0) overviewLabel_->setText(titles[QString(overlay.at(0)).toInt()-1]);
    else if(id == 1) overviewLabel_->setText(titles[QString(overlay.at(1)).toInt()-1]);
    else overviewLabel_->setText("");
}

void OverviewWidget::maximizePreview(ImageViewer* viewer, bool maximize) {
    for(int i=0; i<6; ++i) {
        ImageViewer* preview = previewList_[i];
        QWidget* title = previewTitleWidgets_[i];
        if(maximize && preview != viewer) {
            preview->hide();
            title->hide();
        }
        else {
            preview->show();
            title->show();
        }
    }
}


QString OverviewWidget::parseVariables(const QString& orig, ParametersConfiguration* conf) {
    QString string = orig;
    if(string.contains("${")) {
        int start = string.indexOf('{');
        int end = string.indexOf('}', start);
        QString param = string.mid(start+1, end-start-1);
        QString value;
        if(conf) value = conf->getValue(param);
        string.replace(param, value);
        string.remove("${");
        string.remove('}');
    }
    return string;
}

