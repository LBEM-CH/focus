#include <QtWidgets>

#include "ProjectWindow.h"
#include "ProjectImage.h"
#include "ParameterWidget.h"
#include "ScriptModuleProperties.h"

ProjectWindow::ProjectWindow(QWidget* parent)
: QWidget(parent) {

    QVBoxLayout* mainLayout = new QVBoxLayout;
    mainLayout->setAlignment(Qt::AlignLeft);
    mainLayout->setSpacing(5);
    mainLayout->setMargin(0);
    mainLayout->addStretch(0);
    
    mainLayout->addLayout(setupTitleContainer(), 0);

    QFrame* hLine = new QFrame(this);
    hLine->setFrameStyle(QFrame::HLine | QFrame::Sunken);
    mainLayout->addWidget(hLine, 0);

    ImageConfigChanger* configChanger = new ImageConfigChanger(this);
    
    QStringList subdirs = ScriptModuleProperties(ApplicationData::scriptsDir().canonicalPath() + "/project").subfolders();
    ExecutionWindow* exeWindow = new ExecutionWindow(subdirs);
    
    QTabWidget* tabWidget = new QTabWidget();
    tabWidget->addTab(setupParametersWidget(), ApplicationData::icon("search"), "Search Parameters");
    tabWidget->addTab(configChanger, ApplicationData::icon("sync"), "Synchronize Image Parameters");
    tabWidget->addTab(exeWindow, ApplicationData::icon("utility"), "Utility Scripts");

    mainLayout->addWidget(tabWidget, 1);

    setLayout(mainLayout);

    connect(&projectData, &ProjectData::projectNameChanged, [ = ](const QString & name){
        setProjectTitle(name);
    });
    
    connect(&projectData, &ProjectData::imageCountChanged, [ = ](int count){
        setImagesCount(count);
    });

}

QHBoxLayout* ProjectWindow::setupTitleContainer() {
    QHBoxLayout* mainLayout = new QHBoxLayout;
    mainLayout->setMargin(5);
    mainLayout->setSpacing(10);
    mainLayout->addStretch(0);
    
    GraphicalButton* modeIcon = new GraphicalButton(projectData.projectMode().getIcon());
    modeIcon->setFixedSize(78, 78);
    mainLayout->addWidget(modeIcon, 0);
    
    QVBoxLayout* titleLayout = new QVBoxLayout();
    titleLayout->setMargin(0);
    titleLayout->setSpacing(5);
    titleLayout->addStretch(0);
    
    projectNameLabel_ = new QLabel;
    QFont nameFont = projectNameLabel_->font();
    nameFont.setPointSize(16);
    projectNameLabel_->setFont(nameFont);
    setProjectTitle(projectData.projectName());
    titleLayout->addWidget(projectNameLabel_, 0);

    projectPathLabel_ = new QLabel;
    projectPathLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    setProjectPath(projectData.projectDir().canonicalPath());
    titleLayout->addWidget(projectPathLabel_, 0);
    
    QHBoxLayout* titleButtonsLayout = new QHBoxLayout;
    titleButtonsLayout->addStretch(0);
    
    QPushButton* changeNameButton = new QPushButton(ApplicationData::icon("rename"), "Rename Project");
    connect(changeNameButton, &QPushButton::clicked, &projectData, &ProjectData::changeProjectName);
    titleButtonsLayout->addWidget(changeNameButton, 0);
    
    QPushButton* changeModeButton = new QPushButton(ApplicationData::icon("change_mode"), "Change Project Mode");
    connect(changeModeButton, &QPushButton::clicked, &projectData, &ProjectData::changeProjectMode);
    titleButtonsLayout->addWidget(changeModeButton, 0);
    
    QPushButton* repiarLinksButton = new QPushButton(ApplicationData::icon("repair"), "Repair Project Links");
    connect(repiarLinksButton, &QPushButton::clicked, &projectData, &ProjectData::repairLinks);
    titleButtonsLayout->addWidget(repiarLinksButton, 0);
    
    titleButtonsLayout->addStretch(1);
    
    titleLayout->addLayout(titleButtonsLayout, 0);
    titleLayout->addStretch(1);
    
    QVBoxLayout* imagesLayout = new QVBoxLayout();
    imagesLayout->setMargin(5);
    imagesLayout->setSpacing(5);
    imagesLayout->addStretch(0);
    
    imagesCountLabel_ = new QLabel;
    imagesLayout->setAlignment(Qt::AlignRight);
    setImagesCount(projectData.projectImageList().count());
    
    imagesLayout->addWidget(imagesCountLabel_, 0, Qt::AlignRight);
    imagesLayout->addLayout(setupImageActions());
    
    imagesLayout->addStretch(1);
    
    QFrame* imagesFrame = new QFrame;
    imagesFrame->setFrameShadow(QFrame::Raised);
    imagesFrame->setFrameShape(QFrame::StyledPanel);
    imagesFrame->setLayout(imagesLayout);
    
    mainLayout->addLayout(titleLayout);
    mainLayout->addStretch(1);
    mainLayout->addWidget(imagesFrame);

    return mainLayout;
}

QGridLayout* ProjectWindow::setupImageActions() {
    QGridLayout* layout = new QGridLayout;
    layout->setAlignment(Qt::AlignLeft);
    layout->setSpacing(5);
    layout->setMargin(0);

    QPushButton* reindexButton = new QPushButton(ApplicationData::icon("refresh"), "Re-index Images");
    connect(reindexButton, &QPushButton::clicked, &projectData, &ProjectData::indexImages);
    layout->addWidget(reindexButton, 0, 0);

    QPushButton* resetImageConfigsButton = new QPushButton(ApplicationData::icon("reset"), "Reset Image Parameters");
    connect(resetImageConfigsButton, &QPushButton::clicked, &projectData, &ProjectData::resetImageConfigs);
    layout->addWidget(resetImageConfigsButton, 0, 1);

    QPushButton* renumberButton = new QPushButton(ApplicationData::icon("renumber"), "Renumber Images");
    connect(renumberButton, &QPushButton::clicked, &projectData, &ProjectData::renumberImages);
    layout->addWidget(renumberButton, 1, 0);

    QPushButton* evenOddButton = new QPushButton(ApplicationData::icon("odd"), "Assign Even/Odd");
    connect(evenOddButton, &QPushButton::clicked, &projectData, &ProjectData::assignEvenOdd);
    layout->addWidget(evenOddButton, 1, 1);

    for (int i = 0; i < layout->columnCount(); ++i) layout->setColumnStretch(i, 0);

    return layout;
}

QWidget* ProjectWindow::setupParametersWidget() {
    ParametersWidget* parameters = new ParametersWidget(projectData.projectParameterData(), projectData.projectParameterData()->getLookupTable().keys(), 1);

    QGridLayout* mainLayout = new QGridLayout();
    
    QHBoxLayout* headerLayout = new QHBoxLayout;
    headerLayout->addStretch(0);
    
    QLabel* searchLabel = new QLabel("Search Parameters:");
    QLineEdit* searchEditBox = new QLineEdit();
    searchEditBox->setFrame(false);
    searchLabel->setBuddy(searchEditBox);
    searchEditBox->setPlaceholderText("Search Parameters");
    searchEditBox->setFixedWidth(200);
    connect(searchEditBox, &QLineEdit::editingFinished, [=]() {
        parameters->searchParams(searchEditBox->text());
    });
    
    headerLayout->addWidget(searchLabel, 0);
    headerLayout->addWidget(searchEditBox, 0);
    headerLayout->addStretch(1);
    
    QListWidget* imagesBox = new QListWidget();
    imagesBox->setSelectionMode(QAbstractItemView::SingleSelection);
    imagesBox->addItem("Project Parameters");
    imagesBox->setCurrentRow(0);
    for (ProjectImage* image : projectData.projectImageList()) {
        imagesList_.append(image);
        imagesBox->addItem(image->toString());
    }

    connect(&projectData, &ProjectData::imageAdded, [ = ](ProjectImage * image){
        imagesList_.append(image);
        imagesBox->addItem(image->toString());
    });
    
    connect(&projectData, &ProjectData::imageMoved, [ = ](ProjectImage* image) {
        int idx = imagesList_.indexOf(image) + 1;
        if(idx > 0) {
            imagesBox->item(idx)->setText(image->toString());
        }
    });
    
    connect(imagesBox, &QListWidget::currentRowChanged, [ = ](int id) {
        if(id == 0) parameters->resetConf(projectData.projectParameterData());
        else parameters->resetConf(imagesList_[id -1]->parameters());
    });
    
    BlockContainer* imagesBoxContainer = new BlockContainer("Select the scope of parameters", imagesBox);
    imagesBoxContainer->setMaximumWidth(300);

    mainLayout->addLayout(headerLayout, 0, 0, 1, 1);
    mainLayout->addWidget(parameters, 1, 0, 1, 1);
    mainLayout->addWidget(imagesBoxContainer, 0, 1, 2, 1);
    
    QWidget* mainWid = new QWidget;
    mainWid->setLayout(mainLayout);
    
    return mainWid;
}

void ProjectWindow::setProjectTitle(const QString& title) {
    projectNameLabel_->setText(title + " | " + projectData.projectMode().toString() + " Project");
}

void ProjectWindow::setProjectPath(const QString& path) {
    QString toBeDisplayed = path;
    if (path.length() > 100) toBeDisplayed.remove(0, path.length() - 100);
    projectPathLabel_->setText(toBeDisplayed);
}

void ProjectWindow::setImagesCount(int count) {
    imagesCountLabel_->setText(QString::number(count) + " images in project");
}

