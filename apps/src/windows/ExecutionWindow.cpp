#include <QWhatsThis>
#include <QMessageBox>

#include "ApplicationData.h"
#include "ProjectData.h"
#include "UserPreferenceData.h"

#include "ExecutionWindow.h"
#include "StatusViewer.h"

ExecutionWindow::ExecutionWindow(const QDir& workDir, ResultsData *res, const QStringList& scriptDirs, ExecutionWindow::Type type, QWidget *parent)
: QWidget(parent) {
    workingDir = workDir;
    results = res;
    type_ = type;
    
    panelVisibilityToolBar = new QToolBar;
    panelVisibilityToolBar->addWidget(spacer());
    
    //Setup containers
    BlockContainer* parameterContainer = setupParameterWindow();
    BlockContainer* logWindow = setupLogWindow();
    QWidget* scriptsContainer = setupScriptsWidget(scriptDirs);

    centralSplitter = new QSplitter(Qt::Vertical);
    centralSplitter->addWidget(parameterContainer);
    centralSplitter->addWidget(logWindow);
    centralSplitter->setStretchFactor(0, 2);
    centralSplitter->setStretchFactor(1, 1);

    //Setup rightmost container
    resultsSplitter = setupResultsContainer();

    mainSplitter = new QSplitter(this);
    mainSplitter->setOrientation(Qt::Horizontal);
    mainSplitter->setHandleWidth(4);
    mainSplitter->addWidget(centralSplitter);
    mainSplitter->addWidget(resultsSplitter);
    mainSplitter->setStretchFactor(0, 3);
    mainSplitter->setStretchFactor(1, 1);

    //Setup progress Bar
    progressBar = new QProgressBar(this);
    progressBar->setMaximum(100);
    progressBar->setFixedHeight(10);
    progressBar->setValue(0);
    progressBar->setTextVisible(false);

    //Setup the containers layout
    QVBoxLayout* containersLayout = new QVBoxLayout;
    containersLayout->setMargin(0);
    containersLayout->setSpacing(0);
    containersLayout->addStretch(0);
    containersLayout->addWidget(setupTitleContainer(), 0);
    containersLayout->addWidget(progressBar, 0);
    containersLayout->addWidget(mainSplitter, 1);

    //For Image add the status container
    if (type_ == ExecutionWindow::Type::IMAGE) {
        BlockContainer* statusParserCont = new BlockContainer("Status");
        StatusViewer* statusParser = new StatusViewer(ApplicationData::configDir().absolutePath() + "/2dx_status.html");
        statusParser->setConf(getConf());
        statusParser->load();
        statusParserCont->setMainWidget(statusParser);
        addVisibilityButton("Status", statusParserCont, true);
        containersLayout->addWidget(statusParserCont, 0);
    }
    
    containersLayout->addWidget(panelVisibilityToolBar, 0);
    
    // Setup Main Layout
    QHBoxLayout* scriptsAndContainerLayout = new QHBoxLayout;
    scriptsAndContainerLayout->setMargin(0);
    scriptsAndContainerLayout->setSpacing(0);
    scriptsAndContainerLayout->addStretch(0);
    scriptsAndContainerLayout->addWidget(scriptsContainer, 0);
    scriptsAndContainerLayout->addLayout(containersLayout, 1);
    
    setLayout(scriptsAndContainerLayout);

    verbosityControl->setValue(1);
    defaultModule->selectFirst();

    scriptsWidget->setCurrentIndex(0);
    defaultButton->setChecked(true);
    
    connect(results, SIGNAL(saved(bool)), parameters, SLOT(load()));
}

QWidget* ExecutionWindow::setupScriptsWidget(const QStringList& scriptDirs) {
   
    //Create Toolbar
    QToolBar* scriptsToolBar = new QToolBar("Mode", this);
    scriptsToolBar->setOrientation(Qt::Vertical);
    scriptsToolBar->setIconSize(QSize(32, 32));
    
    scriptsWidget = new QStackedWidget(this);
    scriptsWidget->setAttribute(Qt::WA_MacShowFocusRect, 0);
    scriptsWidget->setMinimumWidth(250);
    scriptsWidget->setMaximumWidth(300);

    connect(scriptsWidget, &QStackedWidget::currentChanged,
            [ = ](){
        ScriptModule* module = static_cast<ScriptModule*> (scriptsWidget->currentWidget());
        module->select(module->getSelection()->selection());
    });

    QButtonGroup* showScriptsGroup = new QButtonGroup(this);
    showScriptsGroup->setExclusive(true);
    
    for (int i = 0; i < scriptDirs.size(); ++i) {
        ScriptModule* module = new ScriptModule(scriptDirs[i], workingDir);
        scriptsWidget->addWidget(module);
        module->resize(200, 20);
        module->setMinimumWidth(200);
        if (i == 0) defaultModule = module;

        connect(module, &ScriptModule::scriptCompleted,
                [ = ](QModelIndex index){
            scriptCompleted(module, index);
        });
        connect(module, &ScriptModule::currentScriptChanged,
                [ = ](QModelIndex index){
            scriptChanged(module, index);
        });
        connect(module, &ScriptModule::shouldResetParams,
                [ = ] (QModelIndex index){
            parameters->resetParameters(module->variablesToReset(index));
        });
        connect(module, SIGNAL(allScriptsCompleted()), this, SLOT(stopPlay()));
        connect(module, SIGNAL(reload()), this, SLOT(reload()));
        connect(module, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
        connect(module, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
        connect(module, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));
        connect(module, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
        connect(verbosityControl, SIGNAL(valueChanged(int)), module, SLOT(setVerbosity(int)));

        QToolButton* toolButton = new QToolButton(this);
        toolButton->setIcon(module->getModuleToolIcon());
        toolButton->setText(module->getModuleDescription());
        toolButton->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
        toolButton->setCheckable(true);
        toolButton->setFixedSize(64, 64);
        connect(toolButton, &QToolButton::toggled,
                [ = ] (){
            scriptsWidget->setCurrentWidget(module);
        });
        showScriptsGroup->addButton(toolButton);
        scriptsToolBar->addWidget(toolButton);
        if (i == 0) defaultButton = toolButton;
    }

    subscriptWidget = new QListView;
    subscriptWidget->setUniformItemSizes(true);
    subscriptWidget->setItemDelegate(new SpinBoxDelegate);
    connect(subscriptWidget, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(subscriptActivated(QModelIndex)));

    QSplitter* scriptsContainer = new QSplitter(Qt::Vertical);
    scriptsContainer->addWidget(scriptsWidget);
    scriptsContainer->addWidget(subscriptWidget);
    scriptsContainer->setStretchFactor(0, 3);
    scriptsContainer->setStretchFactor(1, 1);
    
    QWidget* widget = new QWidget;
    QHBoxLayout* layout = new QHBoxLayout;
    layout->setMargin(0);
    layout->setSpacing(0);
    layout->addStretch(0);
    layout->addWidget(scriptsToolBar, 0);
    layout->addWidget(scriptsContainer, 1);
    widget->setLayout(layout);
    
    return widget;
}

QWidget* ExecutionWindow::setupTitleContainer() {
    QWidget* titleContainer = new QWidget;
    QHBoxLayout* titleLayout = new QHBoxLayout;
    titleLayout->setMargin(0);
    titleLayout->setSpacing(0);
    
    scriptLabel = new QLabel;
    QFont labelFont = scriptLabel->font();
    labelFont.setPointSize(20);
    scriptLabel->setFont(labelFont);
    titleLayout->addWidget(scriptLabel);
    
    titleLayout->addWidget(spacer());
    
    runButton = new QPushButton;
    runButton->setIcon(ApplicationData::icon("play"));
    runButton->setToolTip("Run/Stop script");
    runButton->setIconSize(QSize(18, 18));
    runButton->setCheckable(true);
    connect(runButton, &QPushButton::toggled, this, &ExecutionWindow::executing);
    connect(runButton, SIGNAL(toggled(bool)), this, SLOT(execute(bool)));
    titleLayout->addWidget(runButton);

    QPushButton* refreshButton = new QPushButton;
    refreshButton->setIcon(ApplicationData::icon("refresh_colored"));
    refreshButton->setToolTip("Refresh Results");
    refreshButton->setIconSize(QSize(18, 18));
    refreshButton->setCheckable(false);
    connect(refreshButton, SIGNAL(clicked()), this, SLOT(reload()));
    titleLayout->addWidget(refreshButton);
    
    if(type_ == ExecutionWindow::Type::IMAGE) {
        QPushButton* saveCfgButton = new QPushButton;
        saveCfgButton->setIcon(ApplicationData::icon("save_project_default"));
        saveCfgButton->setToolTip("Save this configuration as project default");
        saveCfgButton->setIconSize(QSize(18, 18));
        saveCfgButton->setCheckable(false);
        connect(saveCfgButton, SIGNAL(clicked()), this, SLOT(saveAsProjectDefault()));
        titleLayout->addWidget(saveCfgButton);
    }

    QPushButton* manualButton = new QPushButton;
    manualButton->setIcon(ApplicationData::icon("information"));
    manualButton->setToolTip("Show Help");
    manualButton->setIconSize(QSize(18, 18));
    manualButton->setCheckable(false);
    connect(manualButton, SIGNAL(clicked()), this, SLOT(showScriptHelp()));
    titleLayout->addWidget(manualButton);

    titleContainer->setLayout(titleLayout);
    
    return titleContainer;
}

QSplitter* ExecutionWindow::setupResultsContainer() {
    BlockContainer *resultsContainer = new BlockContainer("Results");
    resultsView = new ResultsModule(workingDir.canonicalPath(), results, ResultsModule::results);
    resultsContainer->setMainWidget(resultsView);
    resultsContainer->setMinimumSize(QSize(235, 100));

    BlockContainer *imagesContainer = new BlockContainer("Images");
    imagesContainer->setMinimumSize(QSize(235, 100));

    connect(imagesContainer, SIGNAL(doubleClicked()), this, SLOT(launchFileBrowser()));
    ResultsModule *imagesView = new ResultsModule(workingDir.canonicalPath(), results, ResultsModule::images);

    QToolButton* importantSwitch = new QToolButton();
    importantSwitch->setIcon(ApplicationData::icon("important"));
    importantSwitch->setToolTip("Important images only");
    importantSwitch->setAutoRaise(false);
    importantSwitch->setCheckable(true);
    connect(importantSwitch, SIGNAL(toggled(bool)), imagesView, SLOT(setImportant(bool)));

    imagesContainer->setMainWidget(imagesView);
    imagesContainer->setHeaderWidget(importantSwitch);
    
    BlockContainer *previewContainer = new BlockContainer("File Preview");
    previewContainer->setMinimumSize(QSize(235, 100));
    
    QToolButton* infoSwitch = new QToolButton();
    infoSwitch->setIcon(ApplicationData::icon("info"));
    infoSwitch->setToolTip("Show File Information");
    infoSwitch->setAutoRaise(false);
    infoSwitch->setCheckable(true);
    infoSwitch->setChecked(false);
    
    viewer = new ImageViewer(workingDir.canonicalPath());
    connect(imagesView, &ResultsModule::resultChanged, [=] (const QString& file) {viewer->loadFile(file, infoSwitch->isChecked());});
    connect(infoSwitch, &QToolButton::toggled, [=] (bool check) {
        if(check) previewContainer->setHeaderTitle("File Information");
        else previewContainer->setHeaderTitle("File Preview");
        viewer->loadFile(imagesView->selectedImage(), check);
    });
    
    previewContainer->setMainWidget(viewer);
    previewContainer->setHeaderWidget(infoSwitch);
    
    QSplitter* splitter = new QSplitter(Qt::Vertical, this);
    splitter->setMaximumWidth(350);
    splitter->addWidget(resultsContainer);
    splitter->addWidget(imagesContainer);
    splitter->addWidget(previewContainer);
    
    showResultsButton = addVisibilityButton("Results", splitter, false);
    
    return splitter;
}


BlockContainer* ExecutionWindow::setupLogWindow() {
    //Setup Log Viewer
    logViewer = new LogViewer("Standard Output", NULL);

    verbosityControl = new QSlider;
    verbosityControl->setOrientation(Qt::Horizontal);
    verbosityControl->setFixedHeight(20);
    verbosityControl->setMinimum(0);
    verbosityControl->setMaximum(3);
    verbosityControl->setTickPosition(QSlider::TicksBothSides);
    verbosityControl->setTickInterval(1);
    verbosityControl->setSingleStep(1);
    connect(verbosityControl, SIGNAL(valueChanged(int)), logViewer, SLOT(load(int)));

    QWidget* verbosityControlWidget = new QWidget();
    QHBoxLayout* verbosityControlLayout = new QHBoxLayout(verbosityControlWidget);
    verbosityControlLayout->setMargin(0);
    verbosityControlLayout->setSpacing(3);
    verbosityControlWidget->setLayout(verbosityControlLayout);

    verbosityControlLayout->addWidget(new QLabel("Verbosity Level "), 0, Qt::AlignVCenter);
    verbosityControlLayout->addWidget(verbosityControl, 0, Qt::AlignVCenter);

    //Setup the window and add widgets
    BlockContainer *logWindow = new BlockContainer("Output (Double click for logbrowser)", this);
    logWindow->setMinimumWidth(400);
    logWindow->setMinimumHeight(200);
    logWindow->setMainWidget(logViewer);
    logWindow->setHeaderWidget(verbosityControlWidget);

    connect(logWindow, SIGNAL(doubleClicked()), this, SLOT(launchLogBrowser()));

    showOutputButton = addVisibilityButton("Output", logWindow, false);
    
    return logWindow;

}

BlockContainer* ExecutionWindow::setupParameterWindow() {
    parameters = new ParametersWidget(projectData.parameterData(workingDir), this);

    //Setup search box
    parameterSearchBox = new QLineEdit;
    parameterSearchBox->setPlaceholderText("Search Parameters");
    connect(parameterSearchBox, &QLineEdit::editingFinished,
            [=]() {
                parameters->searchParams(parameterSearchBox->text());
            });
    
    //Setup advanced level button
    QRadioButton* advancedLevelButton = new QRadioButton();
    advancedLevelButton->setFixedSize(20, 20);
    connect(advancedLevelButton, &GraphicalButton::toggled, 
            [=](bool val) {
                if(val) parameters->setSelectionUserLevel(1);
                else parameters->setSelectionUserLevel(0);
            });

    QWidget* parameterLevelWidget = new QWidget();
    QHBoxLayout* parameterLevelLayout = new QHBoxLayout(parameterLevelWidget);
    parameterLevelLayout->setMargin(0);
    parameterLevelLayout->setSpacing(0);
    parameterLevelWidget->setLayout(parameterLevelLayout);

    parameterLevelLayout->addWidget(advancedLevelButton, 0, Qt::AlignVCenter);
    parameterLevelLayout->addWidget(new QLabel("Show Advanced"), 0, Qt::AlignVCenter);
    parameterLevelLayout->addSpacing(10);
    parameterLevelLayout->addWidget(parameterSearchBox, 0, Qt::AlignVCenter);

    //Setup the window and add widgets
    BlockContainer* parameterContainer = new BlockContainer("Setup");
    parameterContainer->setMinimumWidth(400);
    parameterContainer->setMinimumHeight(200);
    parameterContainer->setMainWidget(parameters);
    parameterContainer->setHeaderWidget(parameterLevelWidget);
    
    addVisibilityButton("Parameters", parameterContainer, true);

    return parameterContainer;
}

QPushButton* ExecutionWindow::addVisibilityButton(QString title, QWidget* widgetToLink, bool initialState) {
    QPushButton* button = new QPushButton(title, panelVisibilityToolBar);
    button->setAutoDefault(false);
    button->setDefault(false);
    button->setCheckable(true);
    button->setChecked(initialState);
    //widgetToLink->setVisible(initialState);
    
    connect(button, &QPushButton::toggled, 
            [=](bool visible) { widgetToLink->setVisible(visible);});
            
    panelVisibilityToolBar->addWidget(button);
    return button;
}

void ExecutionWindow::execute(bool halt) {
    ScriptModule* module = (ScriptModule*) scriptsWidget->currentWidget();
    runningTabIndex = scriptsWidget->currentIndex();
    showOutputButton->setChecked(true);
    module->execute(halt);
}

void ExecutionWindow::stopPlay() {
    runningTabIndex = -1;
    runButton->setChecked(false);
    emit scriptCompletedSignal();
}

void ExecutionWindow::setScriptProgress(int progress) {
    progressBar->setValue(progress);
}

void ExecutionWindow::scriptChanged(ScriptModule *module, QModelIndex index) {
    progressBar->update();
    scriptLabel->setText(module->title(index));
    parameterSearchBox->setText("");
    
    //  container->saveSplitterState(1);
    int uid = index.data(Qt::UserRole).toUInt();

    QString text;
    QStringList helpTextList = module->getScriptManual(uid);
    for (int i = 0; i < helpTextList.size(); i++) text += "<p>" + helpTextList[i] + "</p>";
    scriptHelp = text;

    centralSplitter->setWhatsThis(text);
    
    QStandardItemModel* model = new QStandardItemModel;
    QStringList subScripts = module->getScriptDependents(uid);
    if (subScripts.size() == 0) {
        subscriptWidget->hide();
    } else {
        subscriptWidget->show();
        QStandardItem *titleItem = new QStandardItem("DEPENDENT SCRIPTS");
        QBrush b;
        b.setColor(Qt::darkGray);
        titleItem->setForeground(b);
        titleItem->setTextAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
        titleItem->setSizeHint(QSize(200, 40));
        titleItem->setSelectable(false);
        titleItem->setEditable(false);
        model->appendRow(titleItem);
        for (int i = 0; i < subScripts.size(); ++i) {
            QString subScriptTitle = subScripts[i];
            QStandardItem *subItem = new QStandardItem(subScriptTitle);
            subItem->setEditable(false);
            if (subScriptTitle.endsWith(".py") || subScriptTitle.endsWith(".python")) subItem->setIcon(ApplicationData::icon("script_py"));
            else subItem->setIcon(ApplicationData::icon("script_csh"));

            subItem->setData(ApplicationData::procScriptsDir().absolutePath() + "/" + subScriptTitle, Qt::UserRole + 5);
            if (!QFileInfo(ApplicationData::procScriptsDir().absolutePath() + "/" + subScriptTitle).exists())
                subItem->setForeground(QColor(255, 0, 0));

            model->appendRow(subItem);
        }
    }
    subscriptWidget->setModel(model);

    parameters->changeParametersDisplayed(module->displayedVariables(index));
    if (verbosityControl->value() != 0)
        logViewer->loadLogFile(module->logFile(index));
    else
        logViewer->clear();
    
    if(QFileInfo(module->logFile(index)).exists()) showOutputButton->setChecked(true);
    else showOutputButton->setChecked(false);
    
    reload(module->resultsFile(index));

    setScriptProgress(module->getScriptProgress(uid));
}

void ExecutionWindow::scriptCompleted(ScriptModule *module, QModelIndex index) {
    reload(module->resultsFile(index));
    results->save();
    resultsView->load();
}

void ExecutionWindow::subscriptActivated(QModelIndex item) {
    if (item.data(Qt::UserRole + 5).toString().isEmpty()) return;
    QProcess::startDetached(UserData::Instance().get("scriptEditor") + " " + item.data(Qt::UserRole + 5).toString());
}

void ExecutionWindow::reload(const QString& resultsFile) {
    if(resultsFile.isEmpty()) results->load();
    else results->load(resultsFile);
    
    if(results->results.isEmpty() && results->images.isEmpty()) showResultsButton->setChecked(false);
    else showResultsButton->setChecked(true);
}

void ExecutionWindow::launchFileBrowser() {
    QString path = QDir::toNativeSeparators(projectData.projectWorkingDir().canonicalPath());
    QDesktopServices::openUrl(QUrl("file:///" + path));
}

void ExecutionWindow::launchLogBrowser() {
    QProcess::startDetached(ApplicationData::logBrowserApp() + " " + logViewer->getLogFile());
}

void ExecutionWindow::showScriptHelp() {
    QWhatsThis::showText(mapToGlobal(QPoint(width()/2, 0)), scriptHelp);
}

void ExecutionWindow::runInitialization() {
    defaultModule->initialize();
}

bool ExecutionWindow::isRunningScript() {
    return runButton->isChecked();
}

void ExecutionWindow::save() {
    projectData.parameterData(workingDir)->save();
}

bool ExecutionWindow::modified() {
    return projectData.parameterData(workingDir)->isModified();
}

ParametersConfiguration* ExecutionWindow::getConf() {
    return projectData.parameterData(workingDir);
}

void ExecutionWindow::saveAsProjectDefault() {
    if (QMessageBox::question(this,
            tr("Save as default?"), "Saving as project default will change master config file and set default values for all other new imported images in this project. \n\n 2DX will QUIT after saving the file. \n\n Proceed?",
            tr("Yes"),
            tr("No"),
            QString(), 0, 1) == 0) {
        projectData.saveAsProjectDefault(workingDir);
    }
}

QWidget* ExecutionWindow::spacer() {
    QWidget* s = new QWidget();
    s->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    return s;
}