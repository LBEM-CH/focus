#include <QtWidgets>

#include "ProjectWindow.h"
#include "ProjectImage.h"
#include "ParameterWidget.h"

ProjectWindow::ProjectWindow(QWidget* parent)
: QWidget(parent) {

    QVBoxLayout* mainLayout = new QVBoxLayout;
    mainLayout->setAlignment(Qt::AlignLeft);
    mainLayout->setSpacing(10);
    mainLayout->setMargin(0);
    mainLayout->addStretch(0);

    QGridLayout* titleLayout = new QGridLayout;
    titleLayout->setSpacing(0);
    titleLayout->setMargin(0);

    GraphicalButton* modeIcon = new GraphicalButton(projectData.projectMode().getIcon());
    modeIcon->setFixedSize(64, 64);
    titleLayout->addWidget(modeIcon, 0, 0, 2, 1);
    titleLayout->addLayout(setupTitleContainer(), 0, 1, 1, 1);
    titleLayout->addLayout(setupProjectActions(), 1, 1, 1, 1);

    mainLayout->addLayout(titleLayout);

    QFrame* hLine = new QFrame(this);
    hLine->setFrameStyle(QFrame::HLine | QFrame::Sunken);
    mainLayout->addWidget(hLine);

    ImageConfigChanger* configChanger = new ImageConfigChanger(this);
    ExecutionWindow* exeWindow = new ExecutionWindow(QDir(ApplicationData::scriptsDir().canonicalPath() + "/project"));
    
    QTabWidget* tabWidget = new QTabWidget();
    tabWidget->addTab(setupParametersWidget(), ApplicationData::icon("search"), "Search Parameters");
    tabWidget->addTab(configChanger, ApplicationData::icon("sync"), "Synchronize Image Parameters");
    tabWidget->addTab(exeWindow, ApplicationData::icon("utility"), "Utility Scripts");

    mainLayout->addWidget(tabWidget, 1);

    setLayout(mainLayout);

    connect(&projectData, &ProjectData::projectNameChanged, [ = ](const QString & name){
        setProjectTitle(name);
    });

}

QGridLayout* ProjectWindow::setupProjectActions() {
    QGridLayout* layout = new QGridLayout;
    layout->setAlignment(Qt::AlignLeft);
    layout->setSpacing(5);
    layout->setMargin(5);

    QPushButton* reindexButton = new QPushButton(ApplicationData::icon("refresh"), "Re-index Images");
    connect(reindexButton, &QPushButton::clicked, &projectData, &ProjectData::indexImages);
    layout->addWidget(reindexButton, 0, 0);

    QPushButton* changeNameButton = new QPushButton(ApplicationData::icon("rename"), "Rename Project");
    connect(changeNameButton, &QPushButton::clicked, &projectData, &ProjectData::changeProjectName);
    layout->addWidget(changeNameButton, 0, 1);

    layout->addItem(new QSpacerItem(30, 10), 0, 2);

    QPushButton* repiarLinksButton = new QPushButton(ApplicationData::icon("repair"), "Repair Project Links");
    connect(repiarLinksButton, &QPushButton::clicked, &projectData, &ProjectData::repairLinks);
    layout->addWidget(repiarLinksButton, 0, 3);

    QPushButton* resetImageConfigsButton = new QPushButton(ApplicationData::icon("reset"), "Reset Image Parameters");
    connect(resetImageConfigsButton, &QPushButton::clicked, &projectData, &ProjectData::resetImageConfigs);
    layout->addWidget(resetImageConfigsButton, 0, 4);

    layout->addItem(new QSpacerItem(30, 10), 0, 5);

    QPushButton* renumberButton = new QPushButton(ApplicationData::icon("renumber"), "Renumber Images");
    connect(renumberButton, &QPushButton::clicked, &projectData, &ProjectData::renumberImages);
    layout->addWidget(renumberButton, 0, 6);

    QPushButton* evenOddButton = new QPushButton(ApplicationData::icon("odd"), "Assign Even/Odd");
    connect(evenOddButton, &QPushButton::clicked, &projectData, &ProjectData::assignEvenOdd);
    layout->addWidget(evenOddButton, 0, 7);

    for (int i = 0; i < layout->columnCount(); ++i) layout->setColumnStretch(i, 0);

    return layout;
}

QHBoxLayout* ProjectWindow::setupTitleContainer() {
    QHBoxLayout* layout = new QHBoxLayout;
    layout->setMargin(5);
    layout->setSpacing(5);
    layout->addStretch(0);

    projectNameLabel_ = new QLabel;
    projectNameLabel_->setText(projectData.projectName());
    QFont nameFont = projectNameLabel_->font();
    nameFont.setPointSize(16);
    nameFont.setBold(true);
    projectNameLabel_->setFont(nameFont);
    layout->addWidget(projectNameLabel_, 0);

    QFrame* vLine = new QFrame(this);
    vLine->setFrameStyle(QFrame::VLine | QFrame::Plain);
    layout->addWidget(vLine, 0);

    QLabel* modeLabel = new QLabel(projectData.projectMode().toString() + " Project");
    QFont modeFont = modeLabel->font();
    modeFont.setPointSize(16);
    modeLabel->setFont(modeFont);
    layout->addWidget(modeLabel, 0);

    layout->addStretch(1);

    projectPathLabel_ = new QLabel;
    projectPathLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    setProjectPath(projectData.projectDir().canonicalPath());
    layout->addWidget(projectPathLabel_, 0);

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
    projectNameLabel_->setText(title);
}

void ProjectWindow::setProjectPath(const QString& path) {
    QString toBeDisplayed = path;
    if (path.length() > 100) toBeDisplayed.remove(0, path.length() - 100);
    projectPathLabel_->setText(toBeDisplayed);
}
