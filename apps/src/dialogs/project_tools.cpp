#include <iostream>

#include "project_tools.h"

ProjectTools::ProjectTools(confData* data, QWidget* parent)
: QDialog(parent) {
    
    setWindowTitle("Project Tools");
    setModal(true);
    
    mainData = data;
    
    blockContainer* logWindow = setupLogWindow();
    blockContainer* parameterContainer = setupParameterWindow();
    
    QSplitter* centralSplitter = new QSplitter(Qt::Vertical);
    centralSplitter->addWidget(parameterContainer);
    centralSplitter->addWidget(logWindow);
    centralSplitter->setStretchFactor(0, 1);
    centralSplitter->setStretchFactor(1, 1);
    centralSplitter->setSizes(QList<int>() << 1 << 1);

    toolsScriptModule = new scriptModule(data, mainData->getDir("projectToolScripts"), scriptModule::project);
    toolsScriptModule->setAttribute(Qt::WA_MacShowFocusRect, 0);
    toolsScriptModule->setMinimumWidth(250);
    toolsScriptModule->setMaximumWidth(300);
    
    connect(toolsScriptModule, &scriptModule::currentScriptChanged,
            [ = ](QModelIndex index){
        scriptChanged(index);
    });
    
    connect(toolsScriptModule, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
    connect(toolsScriptModule, SIGNAL(incrementProgress(int)), this, SLOT(increaseScriptProgress(int)));
    connect(toolsScriptModule, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
    connect(toolsScriptModule, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));
    connect(toolsScriptModule, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), toolsScriptModule, SLOT(setVerbosity(int)));
    
    //Setup Title Bar
    scriptLabel = new QLabel;
    QFont labelFont = scriptLabel->font();
    labelFont.setPointSize(20);
    scriptLabel->setFont(labelFont);
    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    runButton = new QPushButton;
    runButton->setIcon(*(mainData->getIcon("play")));
    runButton->setToolTip("Run/Stop script");
    runButton->setIconSize(QSize(18, 18));
    runButton->setCheckable(true);
    connect(runButton, SIGNAL(toggled(bool)), this, SLOT(execute(bool)));
    connect(toolsScriptModule, SIGNAL(scriptCompleted(QModelIndex)), this, SLOT(stopPlay()));

    manualButton = new QPushButton;
    manualButton->setIcon(*(mainData->getIcon("help")));
    manualButton->setToolTip("Show Help");
    manualButton->setIconSize(QSize(18, 18));
    manualButton->setCheckable(true);
    connect(manualButton, SIGNAL(toggled(bool)), this, SLOT(showSubTitle(bool)));

    QWidget* titleContainer = new QWidget;
    QHBoxLayout* titleLayout = new QHBoxLayout;
    titleLayout->setMargin(0);
    titleLayout->setSpacing(0);
    titleLayout->addWidget(scriptLabel);
    titleLayout->addWidget(spacer);
    titleLayout->addWidget(runButton);
    titleLayout->addWidget(manualButton);
    titleContainer->setLayout(titleLayout);

    //Setup Subtitle Container
    subTitleLabel = new QLabel;
    subTitleLabel->setWordWrap(true);
    QPalette subTitlePal(subTitleLabel->palette());
    subTitlePal.setColor(QPalette::WindowText, Qt::darkGray);
    subTitleLabel->setPalette(subTitlePal);
    subTitleLabel->hide();

    //Setup progress Bar
    progressBar = new QProgressBar(this);
    progressBar->setMaximum(100);
    progressBar->setFixedHeight(10);
    progressBar->setValue(0);
    progressBar->setTextVisible(false);

    QPalette p = palette();
    p.setColor(QPalette::Highlight, Qt::darkCyan);
    progressBar->setPalette(p);

    //Setup Header Container
    QWidget* headerContainer = new QWidget;
    QVBoxLayout* headerLayout = new QVBoxLayout;
    headerLayout->setMargin(5);
    headerLayout->setSpacing(5);
    headerLayout->addWidget(titleContainer);
    headerLayout->addWidget(subTitleLabel);
    headerContainer->setLayout(headerLayout);

    //Setup the layout and add widgets
    QGridLayout* layout = new QGridLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);

    layout->addWidget(toolsScriptModule, 0, 0, 3, 1);
    layout->addWidget(headerContainer, 0, 1, 1, 1);
    layout->addWidget(progressBar, 1, 1, 1, 1);
    layout->addWidget(centralSplitter, 2, 1, 1, 1);

    layout->setRowStretch(0, 0);
    layout->setRowStretch(1, 0);
    layout->setRowStretch(2, 1);
    
    setLayout(layout);
    resize(900, 500);

    verbosityControl->setCurrentIndex(1);
    toolsScriptModule->setVerbosity(1);
    toolsScriptModule->selectFirst();
}

blockContainer* ProjectTools::setupLogWindow() {
    //Setup Log Viewer
    logViewer = new LogViewer("Standard Output", NULL);

    //Setup Verbosity control combo box
    verbosityControl = new QComboBox(this);
    verbosityControl->addItems(QStringList() << "Silent" << "Low" << "Moderate" << "Highest");

    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), logViewer, SLOT(load(int)));

    QWidget* verbosityControlWidget = new QWidget();
    QGridLayout* verbosityControlLayout = new QGridLayout(verbosityControlWidget);
    verbosityControlLayout->setMargin(0);
    verbosityControlLayout->setSpacing(0);
    verbosityControlWidget->setLayout(verbosityControlLayout);

    verbosityControlLayout->addWidget(new QLabel("Verbosity Level: "), 0, 0);
    verbosityControlLayout->addWidget(verbosityControl, 0, 1, 1, 1, Qt::AlignVCenter);

    //Setup the window and add widgets
    blockContainer *logWindow = new blockContainer("Output (Double click for logbrowser)", this);
    logWindow->setMinimumWidth(400);
    logWindow->setMinimumHeight(200);
    logWindow->setMainWidget(logViewer);
    logWindow->setHeaderWidget(verbosityControlWidget);

    connect(logWindow, SIGNAL(doubleClicked()), this, SLOT(launchLogBrowser()));

    return logWindow;

}

blockContainer* ProjectTools::setupParameterWindow() {
    parameters = new ParametersWidget(mainData, this);

    //Setup Verbosity control combo box
    userLevelButtons = new QComboBox(this);
    userLevelButtons->addItems(QStringList() << "Simplified" << "Advanced");
    connect(userLevelButtons, SIGNAL(currentIndexChanged(int)), parameters, SLOT(setSelectionUserLevel(int)));

    QWidget* parameterLevelWidget = new QWidget();
    QGridLayout* parameterLevelLayout = new QGridLayout(parameterLevelWidget);
    parameterLevelLayout->setMargin(0);
    parameterLevelLayout->setSpacing(0);
    parameterLevelWidget->setLayout(parameterLevelLayout);

    parameterLevelLayout->addWidget(new QLabel("Level: "), 0, 0);
    parameterLevelLayout->addWidget(userLevelButtons, 0, 1, 1, 1, Qt::AlignVCenter);

    //Setup the window and add widgets
    blockContainer* parameterContainer = new blockContainer("Setup");
    parameterContainer->setMinimumWidth(400);
    parameterContainer->setMinimumHeight(200);
    parameterContainer->setMainWidget(parameters);
    parameterContainer->setHeaderWidget(parameterLevelWidget);

    return parameterContainer;
}


void ProjectTools::scriptChanged(QModelIndex index) {
    updateScriptLabel(toolsScriptModule->title(index));

    //  container->saveSplitterState(1);
    int uid = index.data(Qt::UserRole).toUInt();

    QString text;
    QStringList helpTextList = toolsScriptModule->getScriptManual(uid);
    for (int i = 0; i < helpTextList.size(); i++) text += helpTextList[i] + "\n";
    subTitleLabel->setText(text);

    parameters->changeParametersDisplayed(toolsScriptModule->displayedVariables(index));
    if (verbosityControl->currentIndex() != 0)
        logViewer->loadLogFile(toolsScriptModule->logFile(index));
    else
        logViewer->clear();
    
    setScriptProgress(toolsScriptModule->getScriptProgress(uid));
}

void ProjectTools::updateScriptLabel(const QString& label) {
    progressBar->update();
    scriptLabel->setText(label);
}

void ProjectTools::increaseScriptProgress(int increament) {
    if (progressBar->value() + increament <= progressBar->maximum())
        progressBar->setValue(progressBar->value() + increament);
    else
        progressBar->setValue(progressBar->maximum());
}

void ProjectTools::setScriptProgress(int progress) {
    progressBar->setValue(progress);
}

void ProjectTools::stopPlay() {
    runButton->setChecked(false);
}

void ProjectTools::execute(bool halt) {
    toolsScriptModule->execute(halt);
}

void ProjectTools::launchLogBrowser() {
    QProcess::startDetached(mainData->getApp("logBrowser") + " " + logViewer->getLogFile());
}

void ProjectTools::showSubTitle(bool s) {
    subTitleLabel->setVisible(s);
}