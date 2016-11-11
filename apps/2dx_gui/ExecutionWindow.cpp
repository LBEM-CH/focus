#include <QWhatsThis>
#include <QMessageBox>
#include <QDateTime>

#include "ApplicationData.h"
#include "ProjectData.h"
#include "UserPreferenceData.h"

#include "ExecutionWindow.h"
#include "StatusViewer.h"
#include "ScriptModuleProperties.h"
#include "UserPreferences.h"

QMutex ExecutionWindow::lock_;

ExecutionWindow::ExecutionWindow(const QDir& moduleDir, ProjectImage* image, QWidget *parent)
: QWidget(parent), image_(image) {
    
    QString typeStr = ScriptModuleProperties(moduleDir.canonicalPath()).level();
    if(typeStr == "image") type_ = ExecutionWindow::Type::IMAGE;
    else type_ = ExecutionWindow::Type::PROJECT;
    
    scriptHelpDialog = new ScriptHelp(this);
    results = new ResultsData(workingDirectory(), this);
    
    panelVisibilityToolBar = new QToolBar;
    panelVisibilityToolBar->addWidget(spacer());
    
    //Create Toolbar
    mainToolBar = new QToolBar("Navigation", this);
    mainToolBar->setOrientation(Qt::Vertical);
    mainToolBar->setIconSize(QSize(32, 32));
    
    executionWidget = new QWidget;
    
    mainWidget = new QStackedWidget;
    mainWidget->addWidget(executionWidget);
    
    mainToolBarButtonGroup = new QButtonGroup(this);
    mainToolBarButtonGroup->setExclusive(true);
    
    //Setup containers
    BlockContainer* parameterContainer = setupParameterWindow();
    BlockContainer* logWindow = setupLogWindow();
    QWidget* scriptsContainer = setupScriptsWidget(ScriptModuleProperties(moduleDir.canonicalPath()).subfolders());

    centralSplitter = new QSplitter(Qt::Vertical);
    centralSplitter->addWidget(parameterContainer);
    centralSplitter->addWidget(logWindow);
    centralSplitter->setStretchFactor(0, 2);
    centralSplitter->setStretchFactor(1, 1);
    
    if(type_ == Type::IMAGE) {
        centralSplitter->addWidget(setupHistoryWindow());
        centralSplitter->setStretchFactor(2, 1);
    }
    

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
        StatusViewer* statusParser = new StatusViewer(ApplicationData::configDir().absolutePath() + "/2dx_status.html");
        statusParser->setConf(getConf());
        statusParser->load();
        BlockContainer* statusParserCont = new BlockContainer("Status", statusParser);
        addVisibilityButton("Status", statusParserCont, true);
        containersLayout->addWidget(statusParserCont, 0);
    }
    
    containersLayout->addWidget(panelVisibilityToolBar, 0);
    
    // Setup Execution Layout
    QHBoxLayout* scriptsAndContainerLayout = new QHBoxLayout;
    scriptsAndContainerLayout->setMargin(0);
    scriptsAndContainerLayout->setSpacing(0);
    scriptsAndContainerLayout->addStretch(0);
    scriptsAndContainerLayout->addWidget(scriptsContainer, 0);
    scriptsAndContainerLayout->addLayout(containersLayout, 1);
    executionWidget->setLayout(scriptsAndContainerLayout);
    
    //Setup Main Layout
    QFrame* vLine = new QFrame(this);
    vLine->setFrameStyle(QFrame::VLine | QFrame::Sunken);
    
    QHBoxLayout* mainLayout = new QHBoxLayout;
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addStretch(0);
    mainLayout->addWidget(mainToolBar, 0);
    mainLayout->addWidget(vLine, 0);
    mainLayout->addWidget(mainWidget, 1);
    
    setLayout(mainLayout);
    
    //Set up values from settings
    outputVerbosityControl->setValue(UserPreferences().userLevel());
    defaultModule->selectFirst();

    scriptsWidget->setCurrentIndex(0);
    defaultButton->setChecked(true);
}

QWidget* ExecutionWindow::setupScriptsWidget(const QStringList& scriptDirs) {
    
    scriptsWidget = new QStackedWidget(this);
    scriptsWidget->setAttribute(Qt::WA_MacShowFocusRect, 0);
    scriptsWidget->setMinimumWidth(250);
    scriptsWidget->setMaximumWidth(300);

    connect(scriptsWidget, &QStackedWidget::currentChanged,
            [ = ](){
        ScriptModule* module = static_cast<ScriptModule*> (scriptsWidget->currentWidget());
        module->select(module->getSelection()->selection());
    });
    
    for (int i = 0; i < scriptDirs.size(); ++i) {
        ScriptModule* module = new ScriptModule(scriptDirs[i], workingDirectory());
        scriptsWidget->addWidget(module);
        module->resize(200, 20);
        module->setMinimumWidth(200);
        if (i == 0) defaultModule = module;
        if(module->isModuleDefaultActivated()) defaultModule = module;

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
        connect(module, &ScriptModule::reload, [=](){
            reloadAndSave();
        });
        connect(module, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
        connect(module, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
        connect(module, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));
        connect(module, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
        connect(outputVerbosityControl, SIGNAL(valueChanged(int)), module, SLOT(setVerbosity(int)));

        QToolButton* toolButton = getToolButton(module->getModuleToolIcon(), module->getModuleDescription(), true);
        connect(toolButton, &QToolButton::toggled, [ = ] (){
            mainWidget->setCurrentWidget(executionWidget);
            scriptsWidget->setCurrentWidget(module);
        });
        mainToolBarButtonGroup->addButton(toolButton);
        mainToolBar->addWidget(toolButton);
        if (i == 0) defaultButton = toolButton;
        if(module->isModuleDefaultActivated()) defaultButton = toolButton;
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
    layout->addWidget(scriptsContainer, 1);
    widget->setLayout(layout);
    
    return widget;
}

QWidget* ExecutionWindow::setupTitleContainer() {
    QWidget* titleContainer = new QWidget;
    QHBoxLayout* titleLayout = new QHBoxLayout;
    titleLayout->setMargin(5);
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
    refreshButton->setToolTip("Reload and Save results");
    refreshButton->setIconSize(QSize(18, 18));
    refreshButton->setCheckable(false);
    connect(refreshButton, &QPushButton::clicked, [=](){
        reloadAndSave();
    });
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
    
    resultsView = new ResultsModule(workingDirectory().canonicalPath(), results, ResultsModule::results);
    BlockContainer *resultsContainer = new BlockContainer("Results", resultsView);
    resultsContainer->setMinimumSize(QSize(235, 100));

    ResultsModule *imagesView = new ResultsModule(workingDirectory().canonicalPath(), results, ResultsModule::images);

    QToolButton* importantSwitch = new QToolButton();
    importantSwitch->setIcon(ApplicationData::icon("important"));
    importantSwitch->setToolTip("Important images only");
    importantSwitch->setAutoRaise(false);
    importantSwitch->setCheckable(true);
    connect(importantSwitch, SIGNAL(toggled(bool)), imagesView, SLOT(setImportant(bool)));
    
    BlockContainer *imagesContainer = new BlockContainer("Images", imagesView, importantSwitch);
    connect(imagesContainer, SIGNAL(doubleClicked()), this, SLOT(launchFileBrowser()));
    imagesContainer->setMinimumSize(QSize(235, 100));
    
    QToolButton* infoSwitch = new QToolButton();
    infoSwitch->setIcon(ApplicationData::icon("info"));
    infoSwitch->setToolTip("Show File Information");
    infoSwitch->setAutoRaise(false);
    infoSwitch->setCheckable(true);
    infoSwitch->setChecked(false);
    
    viewer = new ImageViewer(workingDirectory().canonicalPath());
    connect(imagesView, &ResultsModule::resultChanged, [=] (const QString& file, const QString& ext) {viewer->loadFile(file, ext, infoSwitch->isChecked());});
    
    BlockContainer* previewContainer = new BlockContainer("File Preview", viewer, infoSwitch);
    previewContainer->setMinimumSize(QSize(235, 100));
    
    connect(infoSwitch, &QToolButton::toggled, [=] (bool check) {
        if(check) previewContainer->setHeaderTitle("File Information");
        else previewContainer->setHeaderTitle("File Preview");
        viewer->loadFile(imagesView->selectedImagePath(), imagesView->selectedImageExtenstion(), check);
    });
    
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

    outputVerbosityControl = new QSlider;
    outputVerbosityControl->setOrientation(Qt::Horizontal);
    outputVerbosityControl->setFixedHeight(20);
    outputVerbosityControl->setMinimum(0);
    outputVerbosityControl->setMaximum(3);
    outputVerbosityControl->setTickPosition(QSlider::TicksBothSides);
    outputVerbosityControl->setTickInterval(1);
    outputVerbosityControl->setSingleStep(1);
    connect(outputVerbosityControl, SIGNAL(valueChanged(int)), logViewer, SLOT(load(int)));

    QWidget* verbosityControlWidget = new QWidget();
    QHBoxLayout* verbosityControlLayout = new QHBoxLayout(verbosityControlWidget);
    verbosityControlLayout->setMargin(0);
    verbosityControlLayout->setSpacing(3);
    verbosityControlWidget->setLayout(verbosityControlLayout);

    verbosityControlLayout->addWidget(new QLabel("Verbosity Level "), 0, Qt::AlignVCenter);
    verbosityControlLayout->addWidget(outputVerbosityControl, 0, Qt::AlignVCenter);

    //Setup the window and add widgets
    logWindow = new BlockContainer("Output (Double click for logbrowser)", logViewer, verbosityControlWidget, this);
    logWindow->setMinimumWidth(400);
    logWindow->setMinimumHeight(200);

    connect(logWindow, SIGNAL(doubleClicked()), this, SLOT(launchLogBrowser()));

    showOutputButton = addVisibilityButton("Output", logWindow, false);
    
    return logWindow;

}

BlockContainer* ExecutionWindow::setupHistoryWindow() {
    //Setup Log Viewer
    historyViewer = new LogViewer("Processing History", NULL);

    historyVerbosityControl = new QSlider;
    historyVerbosityControl->setOrientation(Qt::Horizontal);
    historyVerbosityControl->setFixedHeight(20);
    historyVerbosityControl->setMinimum(0);
    historyVerbosityControl->setMaximum(3);
    historyVerbosityControl->setTickPosition(QSlider::TicksBothSides);
    historyVerbosityControl->setTickInterval(1);
    historyVerbosityControl->setSingleStep(1);
    connect(historyVerbosityControl, SIGNAL(valueChanged(int)), historyViewer, SLOT(load(int)));
    historyVerbosityControl->setValue(UserPreferences().userLevel());

    QWidget* verbosityControlWidget = new QWidget();
    QHBoxLayout* verbosityControlLayout = new QHBoxLayout(verbosityControlWidget);
    verbosityControlLayout->setMargin(0);
    verbosityControlLayout->setSpacing(3);
    verbosityControlWidget->setLayout(verbosityControlLayout);

    verbosityControlLayout->addWidget(new QLabel("Verbosity Level "), 0, Qt::AlignVCenter);
    verbosityControlLayout->addWidget(historyVerbosityControl, 0, Qt::AlignVCenter);

    //Setup the window and add widgets
    BlockContainer *historyWindow = new BlockContainer("Processing History", historyViewer, verbosityControlWidget);
    historyWindow->setMinimumWidth(400);
    historyWindow->setMinimumHeight(200);

    historyWindow->setVisible(false);
    addVisibilityButton("History", historyWindow, false);
    
    return historyWindow;

}

BlockContainer* ExecutionWindow::setupParameterWindow() {
    parameters = new ParametersWidget(getConf(), this);
    
    if(UserPreferences().showAdvanced()) parameters->setSelectionUserLevel(1);

    //Setup search box
    parameterSearchBox = new QLineEdit;
    parameterSearchBox->setPlaceholderText("Search Parameters");
    parameterSearchBox->setFixedWidth(200);
    connect(parameterSearchBox, &QLineEdit::editingFinished,
            [=]() {
                parameters->searchParams(parameterSearchBox->text());
            });
    
    //Setup advanced level button
    QRadioButton* advancedLevelButton = new QRadioButton();
    advancedLevelButton->setFixedSize(20, 20);
    advancedLevelButton->setChecked(UserPreferences().showAdvanced());
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
    BlockContainer* parameterContainer = new BlockContainer("Setup", parameters, parameterLevelWidget);
    parameterContainer->setMinimumWidth(400);
    parameterContainer->setMinimumHeight(200);
    
    addVisibilityButton("Parameters", parameterContainer, true);

    return parameterContainer;
}

QToolButton* ExecutionWindow::addVisibilityButton(QString title, QWidget* widgetToLink, bool initialState) {
    QToolButton* button = new QToolButton(panelVisibilityToolBar);
    button->setText(title);
    button->setToolButtonStyle(Qt::ToolButtonTextOnly);
    button->setCheckable(true);
    button->setChecked(initialState);
    //widgetToLink->setVisible(initialState);
    
    connect(button, &QToolButton::toggled, 
            [=](bool visible) { widgetToLink->setVisible(visible);});
            
    panelVisibilityToolBar->addWidget(button);
    return button;
}

void ExecutionWindow::execute(bool halt) {
    ScriptModule* module = (ScriptModule*) scriptsWidget->currentWidget();
    runningTabIndex = scriptsWidget->currentIndex();
    showOutputButton->setChecked(true);
    logWindow->setVisible(true);
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
    
    //qDebug() << "Script selected: " << module->title(index);
    int uid = index.data(Qt::UserRole).toUInt();
    
    scriptHelpDialog->setTitle(module->title(index));
    scriptHelpDialog->setData(module->getScriptManual(uid), module->publicationList(index));
    
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
    //qDebug() << module->displayedVariables(index);
    if (outputVerbosityControl->value() != 0) logViewer->loadLogFile(module->logFile(index));
    else logViewer->clear();
    
    if(type_ == Type::IMAGE) {
        if (historyVerbosityControl->value() != 0) historyViewer->loadLogFile(workingDirectory().canonicalPath() + "/" + "History.dat");
        else historyViewer->clear();
    }
    
    if(QFileInfo(module->logFile(index)).exists()) {
        showOutputButton->setChecked(true);
        logWindow->setVisible(true);
    }
    else {
        showOutputButton->setChecked(false);
        logWindow->setVisible(false);
    }
    
    reloadAndSave(module->resultsFile(index), false);

    setScriptProgress(module->getScriptProgress(uid));
}

void ExecutionWindow::scriptCompleted(ScriptModule *module, QModelIndex index) {
    qDebug() << "Script: " << module->title(index) << " finished";
    reloadAndSave(module->resultsFile(index));
    if(results->newImagesImported()) projectData.indexImages();
    resultsView->load();
    setLastChangedInConfig();
}

void ExecutionWindow::subscriptActivated(QModelIndex item) {
    if (item.data(Qt::UserRole + 5).toString().isEmpty()) return;
    QProcess::startDetached(UserData::Instance().get("scriptEditor") + " " + item.data(Qt::UserRole + 5).toString());
}

void ExecutionWindow::reloadAndSave(const QString& resultsFile, bool save) {
    QMutexLocker locker(&ExecutionWindow::lock_);
    if(resultsFile.isEmpty()) results->load();
    else results->load(resultsFile);
    
    if(save) results->save();
    
    locker.unlock();
    
    if(results->results.isEmpty() && results->images.isEmpty())  {
        showResultsButton->setChecked(false);
        resultsSplitter->setVisible(false);
    }
    else  {
        showResultsButton->setChecked(true);
        resultsSplitter->setVisible(true);
    }
}

void ExecutionWindow::launchFileBrowser() {
    QString path = QDir::toNativeSeparators(workingDirectory().canonicalPath());
    QDesktopServices::openUrl(QUrl("file:///" + path));
}

void ExecutionWindow::launchLogBrowser() {
    QProcess::startDetached(ApplicationData::logBrowserApp() + " " + logViewer->getLogFile());
}

void ExecutionWindow::showScriptHelp() {
    scriptHelpDialog->show();
}

void ExecutionWindow::runInitialization() {
    defaultModule->initialize();
}

bool ExecutionWindow::isRunningScript() {
    return runButton->isChecked();
}

void ExecutionWindow::save() {
    getConf()->save();
}

bool ExecutionWindow::modified() {
    return getConf()->isModified();
}

ParametersConfiguration* ExecutionWindow::getConf() {
    if(image_) return image_->parameters();
    else return projectData.projectParameterData();
}

QDir ExecutionWindow::workingDirectory() {
    if(image_) return image_->workingDir();
    else return projectData.projectWorkingDir();
}

QToolBar* ExecutionWindow::getToolBar() {
    return mainToolBar;
}

QToolButton* ExecutionWindow::getToolButton(const QIcon& icon, const QString& text, bool checkable) {
    QToolButton* toolButton = new QToolButton();
    toolButton->setIcon(icon);
    toolButton->setText(text);
    toolButton->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolButton->setCheckable(checkable);
    toolButton->setFixedSize(80, 64);
    return toolButton;
}

void ExecutionWindow::saveAsProjectDefault() {
    if(image_) projectData.saveAsProjectDefault(image_);
}

QWidget* ExecutionWindow::spacer() {
    QWidget* s = new QWidget();
    s->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    return s;
}

void ExecutionWindow::setLastChangedInConfig() {
    ParametersConfiguration* conf = getConf();
    conf->set("last_processed", ApplicationData::currentDateTimeString());
}

void ExecutionWindow::addToMainToolBar(QWidget* associatedWidget, const QIcon& icon, const QString& text, bool startWithSeperator) {
    mainWidget->addWidget(associatedWidget);
    QToolButton* button = getToolButton(icon, text, true);
    button->setChecked(false);
    mainToolBarButtonGroup->addButton(button);
    connect(button, &QToolButton::toggled, [=] () {
        mainWidget->setCurrentWidget(associatedWidget);
    });
    if(startWithSeperator) mainToolBar->addSeparator();
    mainToolBar->addWidget(button);
}
