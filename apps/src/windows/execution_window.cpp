#include <QWhatsThis>

#include "execution_window.h"

using namespace std;

ExecutionWindow::ExecutionWindow(confData* data, resultsData *res, const QStringList& scriptDirs, QWidget *parent)
: QWidget(parent) {
    mainData = data;
    results = res;

    //Setup dependables
    blockContainer* logWindow = setupLogWindow();
    blockContainer* parameterContainer = setupParameterWindow();

    //Setup Leftmost container
    scriptsWidget = new QStackedWidget(this);
    scriptsWidget->setAttribute(Qt::WA_MacShowFocusRect, 0);
    scriptsWidget->setMinimumWidth(250);
    scriptsWidget->setMaximumWidth(300);

    connect(scriptsWidget, &QStackedWidget::currentChanged,
            [ = ](){
        scriptModule* module = static_cast<scriptModule*> (scriptsWidget->currentWidget());
        module->select(module->getSelection()->selection());
    });

    QActionGroup* showScriptsGroup = new QActionGroup(this);
    showScriptsGroup->setExclusive(true);
    QToolBar* scriptsToolBar = new QToolBar("Mode", this);
    scriptsToolBar->setOrientation(Qt::Vertical);
    scriptsToolBar->setIconSize(QSize(36, 36));

    for (int i = 0; i < scriptDirs.size(); ++i) {
        scriptModule* module = new scriptModule(mainData, scriptDirs[i]);
        scriptsWidget->addWidget(module);
        module->resize(200, 20);
        module->setMinimumWidth(200);
        if (i == 0) defaultModule = module;

        connect(module, &scriptModule::scriptCompleted,
                [ = ](QModelIndex index){
            scriptCompleted(module, index);
        });
        connect(module, &scriptModule::currentScriptChanged,
                [ = ](QModelIndex index){
            scriptChanged(module, index);
        });
        connect(module, &scriptModule::shouldResetParams,
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

        QAction* action = new QAction(module->getModuleToolIcon(), module->getModuleDescription(), this);
        action->setCheckable(true);
        connect(action, &QAction::triggered,
                [ = ] (){
            scriptsWidget->setCurrentWidget(module);
        });
        showScriptsGroup->addAction(action);
        scriptsToolBar->addAction(action);
        if (i == 0) defaultAction = action;
    }

    subscriptWidget = new QListView;
    subscriptWidget->setUniformItemSizes(true);
    subscriptWidget->setItemDelegate(new SpinBoxDelegate);
    connect(subscriptWidget, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(subscriptActivated(QModelIndex)));

    QSplitter* scriptsContainer = new QSplitter(Qt::Vertical);
    scriptsContainer->addWidget(scriptsWidget);
    scriptsContainer->addWidget(subscriptWidget);
    scriptsContainer->setStretchFactor(0, 2);
    scriptsContainer->setStretchFactor(1, 1);

    centralSplitter = new QSplitter(Qt::Vertical);
    centralSplitter->addWidget(parameterContainer);
    centralSplitter->addWidget(logWindow);
    centralSplitter->setStretchFactor(0, 1);
    centralSplitter->setStretchFactor(1, 1);

    //Setup rightmost container
    blockContainer *resultsContainer = new blockContainer("Results");
    resultsView = new resultsModule(mainData, results, resultsModule::results, mainData->getDir("project"));
    resultsContainer->setMainWidget(resultsView);
    resultsContainer->setMinimumSize(QSize(250, 100));
    resultsContainer->setMaximumWidth(400);

    blockContainer *imagesContainer = new blockContainer("Images");
    imagesContainer->setMinimumSize(QSize(250, 100));
    imagesContainer->setMaximumWidth(400);

    connect(imagesContainer, SIGNAL(doubleClicked()), this, SLOT(launchFileBrowser()));
    resultsModule *imagesView = new resultsModule(mainData, results, resultsModule::images, mainData->getDir("project"));

    QToolButton* importantSwitch = new QToolButton();
    importantSwitch->setIcon(*(mainData->getIcon("important")));
    importantSwitch->setToolTip("Important images only");
    importantSwitch->setAutoRaise(false);
    importantSwitch->setCheckable(true);
    connect(importantSwitch, SIGNAL(toggled(bool)), imagesView, SLOT(setImportant(bool)));

    imagesContainer->setMainWidget(imagesView);
    imagesContainer->setHeaderWidget(importantSwitch);


    resultsSplitter = new QSplitter(Qt::Vertical, this);
    resultsSplitter->addWidget(resultsContainer);
    resultsSplitter->addWidget(imagesContainer);

    centerRightSplitter = new QSplitter(this);
    centerRightSplitter->setOrientation(Qt::Horizontal);
    centerRightSplitter->setHandleWidth(4);
    centerRightSplitter->addWidget(centralSplitter);
    centerRightSplitter->addWidget(resultsSplitter);
    centerRightSplitter->setStretchFactor(0, 3);
    centerRightSplitter->setStretchFactor(1, 1);

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

    refreshButton = new QPushButton;
    refreshButton->setIcon(*(mainData->getIcon("refresh_colored")));
    refreshButton->setToolTip("Refresh Results");
    refreshButton->setIconSize(QSize(18, 18));
    refreshButton->setCheckable(false);
    connect(refreshButton, SIGNAL(clicked()), this, SLOT(reload()));

    manualButton = new QPushButton;
    manualButton->setIcon(*(mainData->getIcon("information")));
    manualButton->setToolTip("Show Help");
    manualButton->setIconSize(QSize(18, 18));
    manualButton->setCheckable(false);
    connect(manualButton, SIGNAL(clicked()), this, SLOT(showScriptHelp()));

    QWidget* titleContainer = new QWidget;
    QHBoxLayout* titleLayout = new QHBoxLayout;
    titleLayout->setMargin(0);
    titleLayout->setSpacing(0);
    titleLayout->addWidget(scriptLabel);
    titleLayout->addWidget(spacer);
    titleLayout->addWidget(runButton);
    titleLayout->addWidget(refreshButton);
    titleLayout->addWidget(manualButton);
    titleContainer->setLayout(titleLayout);

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
    headerContainer->setLayout(headerLayout);

    //Setup the layout and add widgets
    QGridLayout* layout = new QGridLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);
    setLayout(layout);

    layout->addWidget(scriptsToolBar, 0, 0, 3, 1);
    layout->addWidget(scriptsContainer, 0, 1, 3, 1);
    layout->addWidget(headerContainer, 0, 2, 1, 1);
    layout->addWidget(progressBar, 1, 2, 1, 1);
    layout->addWidget(centerRightSplitter, 2, 2, 1, 1);

    layout->setRowStretch(0, 0);
    layout->setRowStretch(1, 0);
    layout->setRowStretch(2, 1);

    verbosityControl->setValue(1);
    defaultModule->selectFirst();

    scriptsWidget->setCurrentIndex(0);
    defaultAction->setChecked(true);
    
    connect(results, SIGNAL(saved(bool)), parameters, SLOT(load()));

    //Just to get the correct stretches of log and parameter windows
    maximizeLogWindow(false);
    maximizeParameterWindow(false);
}

blockContainer* ExecutionWindow::setupLogWindow() {
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

    //Setup Maximize tool button
    QToolButton* maximizeLogWindow = new QToolButton();
    maximizeLogWindow->setFixedSize(QSize(20, 20));
    maximizeLogWindow->setIcon(*(mainData->getIcon("maximize")));
    maximizeLogWindow->setToolTip("Maximize output view");
    maximizeLogWindow->setAutoRaise(false);
    maximizeLogWindow->setCheckable(true);

    connect(maximizeLogWindow, SIGNAL(toggled(bool)), this, SLOT(maximizeLogWindow(bool)));

    QWidget* verbosityControlWidget = new QWidget();
    QHBoxLayout* verbosityControlLayout = new QHBoxLayout(verbosityControlWidget);
    verbosityControlLayout->setMargin(0);
    verbosityControlLayout->setSpacing(3);
    verbosityControlWidget->setLayout(verbosityControlLayout);

    verbosityControlLayout->addWidget(new QLabel("Verbosity Level "), 0, Qt::AlignVCenter);
    verbosityControlLayout->addWidget(verbosityControl, 0, Qt::AlignVCenter);
    verbosityControlLayout->addSpacing(3);
    verbosityControlLayout->addWidget(maximizeLogWindow, 0, Qt::AlignVCenter);

    //Setup the window and add widgets
    blockContainer *logWindow = new blockContainer("Output (Double click for logbrowser)", this);
    logWindow->setMinimumWidth(400);
    logWindow->setMinimumHeight(200);
    logWindow->setMainWidget(logViewer);
    logWindow->setHeaderWidget(verbosityControlWidget);

    connect(logWindow, SIGNAL(doubleClicked()), this, SLOT(launchLogBrowser()));

    return logWindow;

}

blockContainer* ExecutionWindow::setupParameterWindow() {
    parameters = new ParametersWidget(mainData, this);

    //Setup search box
    parameterSearchBox = new QLineEdit;
    connect(parameterSearchBox, &QLineEdit::editingFinished,
            [=]() {
                parameters->searchParams(parameterSearchBox->text());
            });
    
    //Setup advanced level button
    QToolButton* advancedLevelButton = new QToolButton();
    advancedLevelButton->setCheckable(true);
    advancedLevelButton->setIcon(*(mainData->getIcon("advanced")));
    advancedLevelButton->setToolTip("Show/Hide advanced parameters");
    advancedLevelButton->setFixedSize(20, 20);
    connect(advancedLevelButton, &graphicalButton::toggled, 
            [=](bool val) {
                if(val) parameters->setSelectionUserLevel(1);
                else parameters->setSelectionUserLevel(0);
            });

    //Setup Maximize tool button
    QToolButton* maximizeParameterWin = new QToolButton();
    maximizeParameterWin->setFixedSize(QSize(20, 20));
    maximizeParameterWin->setIcon(*(mainData->getIcon("maximize")));
    maximizeParameterWin->setToolTip("Maximize parameter view");
    maximizeParameterWin->setAutoRaise(false);
    maximizeParameterWin->setCheckable(true);

    connect(maximizeParameterWin, SIGNAL(toggled(bool)), this, SLOT(maximizeParameterWindow(bool)));

    QWidget* parameterLevelWidget = new QWidget();
    QHBoxLayout* parameterLevelLayout = new QHBoxLayout(parameterLevelWidget);
    parameterLevelLayout->setMargin(0);
    parameterLevelLayout->setSpacing(3);
    parameterLevelWidget->setLayout(parameterLevelLayout);

    parameterLevelLayout->addWidget(new QLabel("Search"), 0, Qt::AlignVCenter);
    parameterLevelLayout->addWidget(parameterSearchBox, 0, Qt::AlignVCenter);
    parameterLevelLayout->addSpacing(3);
    parameterLevelLayout->addWidget(advancedLevelButton, 0, Qt::AlignVCenter);
    parameterLevelLayout->addWidget(maximizeParameterWin, 0, Qt::AlignVCenter);

    //Setup the window and add widgets
    blockContainer* parameterContainer = new blockContainer("Setup");
    parameterContainer->setMinimumWidth(400);
    parameterContainer->setMinimumHeight(200);
    parameterContainer->setMainWidget(parameters);
    parameterContainer->setHeaderWidget(parameterLevelWidget);

    return parameterContainer;
}

void ExecutionWindow::execute(bool halt) {
    scriptModule* module = (scriptModule*) scriptsWidget->currentWidget();
    runningTabIndex = scriptsWidget->currentIndex();
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

void ExecutionWindow::scriptChanged(scriptModule *module, QModelIndex index) {
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
            if (subScriptTitle.endsWith(".py") || subScriptTitle.endsWith(".python")) subItem->setIcon(*mainData->getIcon("script_py"));
            else subItem->setIcon(*mainData->getIcon("script_csh"));

            subItem->setData(mainData->getDir("procDir") + "/" + subScriptTitle, Qt::UserRole + 5);
            if (!QFileInfo(mainData->getDir("procDir") + "/" + subScriptTitle).exists())
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
    results->load(module->resultsFile(index));

    setScriptProgress(module->getScriptProgress(uid));
    //  container->restoreSplitterState(1);
}

void ExecutionWindow::scriptCompleted(scriptModule *module, QModelIndex index) {
    //  cerr<<"Script completed"<<endl;
    results->load(module->resultsFile(index));
    results->save();
    resultsView->load();
}

void ExecutionWindow::subscriptActivated(QModelIndex item) {
    if (item.data(Qt::UserRole + 5).toString().isEmpty()) return;
    QProcess::startDetached(mainData->getApp("scriptEditor") + " " + item.data(Qt::UserRole + 5).toString());
}

void ExecutionWindow::reload() {
    results->load();
}

void ExecutionWindow::maximizeLogWindow(bool maximize) {
    if (maximize) {
        centralSplitter->setSizes(QList<int>() << 0 << 1);
        centerRightSplitter->setSizes(QList<int>() << 1 << 0);
    } else {
        centralSplitter->setSizes(QList<int>() << 1 << 1);
        centerRightSplitter->setSizes(QList<int>() << 3 << 1);
    }
}

void ExecutionWindow::maximizeParameterWindow(bool maximize) {
    if (maximize) {
        centralSplitter->setSizes(QList<int>() << 1 << 0);
        centerRightSplitter->setSizes(QList<int>() << 1 << 0);
    } else {
        centralSplitter->setSizes(QList<int>() << 1 << 1);
        centerRightSplitter->setSizes(QList<int>() << 3 << 1);
    }
}

void ExecutionWindow::launchFileBrowser() {
    QString path = QDir::toNativeSeparators(mainData->getDir("working"));
    QDesktopServices::openUrl(QUrl("file:///" + path));
}

void ExecutionWindow::launchLogBrowser() {
    QProcess::startDetached(mainData->getApp("logBrowser") + " " + logViewer->getLogFile());
}

void ExecutionWindow::showScriptHelp() {
    QWhatsThis::showText(mapToGlobal(QPoint(width()/2, 0)), scriptHelp);
}

void ExecutionWindow::runInitialization() {
    defaultModule->initialize();
}

void ExecutionWindow::hideResultsPanel() {
    resultsSplitter->hide();
}

bool ExecutionWindow::isRunningScript() {
    return runButton->isChecked();
}