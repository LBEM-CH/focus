#include "executionContainer.h"

using namespace std;

executionContainer::executionContainer(confData* data, resultsData *res, QWidget *parent)
                        : QWidget(parent) 
{
    mainData = data;
    results = res;
    
    //Setup Leftmost container
    scriptsWidget = new QStackedWidget(this);
    scriptsWidget->setMinimumWidth(250);
    scriptsWidget->setMaximumWidth(300);
    
    merge2DScripts = new scriptModule(mainData, mainData->getDir("merge2DScripts"), scriptModule::merge2D);
    connect(merge2DScripts, SIGNAL(scriptCompleted(QModelIndex)), this, SLOT(merge2DScriptCompleted(QModelIndex)));
    connect(merge2DScripts, SIGNAL(currentScriptChanged(QModelIndex)), this, SLOT(merge2DScriptChanged(QModelIndex)));
    connect(merge2DScripts, SIGNAL(reload()), this, SLOT(reload()));
    connect(merge2DScripts, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
    connect(merge2DScripts, SIGNAL(incrementProgress(int)), this, SLOT(increaseScriptProgress(int)));
    addToScriptsWidget(merge2DScripts);
    
    merge3DScripts = new scriptModule(mainData, mainData->getDir("merge3DScripts"), scriptModule::merge3D);
    connect(merge3DScripts, SIGNAL(scriptCompleted(QModelIndex)), this, SLOT(merge3DScriptCompleted(QModelIndex)));
    connect(merge3DScripts, SIGNAL(currentScriptChanged(QModelIndex)), this, SLOT(merge3DScriptChanged(QModelIndex)));
    connect(merge3DScripts, SIGNAL(reload()), this, SLOT(reload()));
    connect(merge3DScripts, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
    connect(merge3DScripts, SIGNAL(incrementProgress(int)), this, SLOT(increaseScriptProgress(int)));
    addToScriptsWidget(merge3DScripts);

    customScripts = new scriptModule(mainData, mainData->getDir("customScripts"), scriptModule::custom);
    connect(customScripts, SIGNAL(scriptCompleted(QModelIndex)), this, SLOT(customScriptCompleted(QModelIndex)));
    connect(customScripts, SIGNAL(currentScriptChanged(QModelIndex)), this, SLOT(customScriptChanged(QModelIndex)));
    connect(customScripts, SIGNAL(reload()), this, SLOT(reload()));
    connect(customScripts, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
    connect(customScripts, SIGNAL(incrementProgress(int)), this, SLOT(increaseScriptProgress(int)));
    addToScriptsWidget(customScripts);

    singleParticleScripts = new scriptModule(mainData, mainData->getDir("singleParticleScripts"), scriptModule::singleparticle);
    connect(singleParticleScripts, SIGNAL(scriptCompleted(QModelIndex)), this, SLOT(singleParticleScriptCompleted(QModelIndex)));
    connect(singleParticleScripts, SIGNAL(currentScriptChanged(QModelIndex)), this, SLOT(singleParticleScriptChanged(QModelIndex)));
    connect(singleParticleScripts, SIGNAL(reload()), this, SLOT(reload()));
    connect(singleParticleScripts, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
    connect(singleParticleScripts, SIGNAL(incrementProgress(int)), this, SLOT(increaseScriptProgress(int)));
    addToScriptsWidget(singleParticleScripts);
    
    manuals = new QStackedWidget();
    manuals->hide();
    
    QSplitter* scriptsContainer = new QSplitter(Qt::Vertical);
    scriptsContainer->addWidget(scriptsWidget);
    scriptsContainer->addWidget(manuals);
    scriptsContainer->setStretchFactor(0, 2);
    scriptsContainer->setStretchFactor(1, 1);

    //Setup center container
    blockContainer* logWindow = setupLogWindow();
    blockContainer* parameterContainer = setupParameterWindow();

    centralContainer = new QSplitter(Qt::Vertical);
    centralContainer->addWidget(parameterContainer);
    centralContainer->addWidget(logWindow);
    centralContainer->setStretchFactor(0, 1);
    centralContainer->setStretchFactor(1, 1);

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
    

    QSplitter *resultsSplitter = new QSplitter(Qt::Vertical, this);
    resultsSplitter->addWidget(resultsContainer);
    resultsSplitter->addWidget(imagesContainer);

    centerRightSplitter = new QSplitter(this);
    centerRightSplitter->setOrientation(Qt::Horizontal);
    centerRightSplitter->setHandleWidth(4);
    centerRightSplitter->addWidget(centralContainer);
    centerRightSplitter->addWidget(resultsSplitter);
    centerRightSplitter->setStretchFactor(0, 3);
    centerRightSplitter->setStretchFactor(1, 1);

    //Setup status Bar
    progressBar = new QProgressBar(this);
    progressBar->setMaximum(100);
    progressBar->setFixedWidth(300);
    progressBar->setFixedHeight(10);
    progressBar->setValue(0);
    progressBar->setTextVisible(false);
    
    statusBar = new QStatusBar(this);
    statusBar->addPermanentWidget(progressBar);
    
    
    //Setup the layout and add widgets
    QGridLayout* layout = new QGridLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);
    setLayout(layout);
    
    layout->addWidget(setupToolbar(), 0, 0, 2, 1);
    layout->addWidget(scriptsContainer, 0, 1, 1, 1);
    layout->addWidget(centerRightSplitter, 0, 2, 1, 1);
    layout->addWidget(statusBar, 1, 1, 1, 2);

    verbosityControl->setCurrentIndex(1);
    merge2DScripts->initialize();
    
    setMerge2DMode();
    
    //Just to get the correct stretches of log and parameter windows
    maximizeLogWindow(false);
    maximizeParameterWindow(false);
}

QToolBar* executionContainer::setupToolbar() 
{
    QToolBar* scriptsToolBar = new QToolBar("Choose Mode", this);
    scriptsToolBar->setOrientation(Qt::Vertical);
    
    showMerge2DScripts = new QToolButton(scriptsToolBar);
    showMerge2DScripts->setIcon(*(mainData->getIcon("merge2D")));
    showMerge2DScripts->setFixedSize(QSize(64,64));
    showMerge2DScripts->setText("Merge");
    showMerge2DScripts->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    showMerge2DScripts->setCheckable(true);
    connect(showMerge2DScripts, SIGNAL(clicked()), this, SLOT(setMerge2DMode()));
    
    showMerge3DScripts = new QToolButton(scriptsToolBar);
    showMerge3DScripts->setIcon(*(mainData->getIcon("merge3D")));
    showMerge3DScripts->setFixedSize(QSize(64,64));
    showMerge3DScripts->setText("Merge");
    showMerge3DScripts->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    showMerge3DScripts->setCheckable(true);
    connect(showMerge3DScripts, SIGNAL(clicked()), this, SLOT(setMerge3DMode()));
    
    showCustomScripts = new QToolButton(scriptsToolBar);
    showCustomScripts->setIcon(*(mainData->getIcon("custom")));
    showCustomScripts->setFixedSize(QSize(64,64));
    showCustomScripts->setText("Custom");
    showCustomScripts->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    showCustomScripts->setCheckable(true);
    connect(showCustomScripts, SIGNAL(clicked()), this, SLOT(setCustomMode()));
    
    showSPScripts = new QToolButton(scriptsToolBar);
    showSPScripts->setIcon(*(mainData->getIcon("single_particle")));
    showSPScripts->setFixedSize(QSize(64,64));
    showSPScripts->setText(tr("Single\nParticle"));
    showSPScripts->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    showSPScripts->setCheckable(true);
    connect(showSPScripts, SIGNAL(clicked()), this, SLOT(setSPMode()));
    
    scriptsToolBar->addWidget(showMerge2DScripts);
    scriptsToolBar->addWidget(showMerge3DScripts);
    scriptsToolBar->addWidget(showCustomScripts);
    scriptsToolBar->addWidget(showSPScripts);
    
    return scriptsToolBar;

}

blockContainer* executionContainer::setupLogWindow() 
{   
    //Setup Log Viewer
    logViewer = new LogViewer("Standard Output", NULL);

    connect(merge2DScripts, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
    connect(merge2DScripts, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));
    
    connect(merge3DScripts, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
    connect(merge3DScripts, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));

    connect(customScripts, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
    connect(customScripts, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));

    connect(singleParticleScripts, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
    connect(singleParticleScripts, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));

    connect(merge2DScripts, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
    connect(merge3DScripts, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
    connect(customScripts, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
    connect(singleParticleScripts, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
    
    //Setup Verbosity control combo box
    verbosityControl = new QComboBox(this);
    verbosityControl->addItems(QStringList() << "Silent" << "Low" << "Moderate" << "Highest");
    
    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), logViewer, SLOT(load(int)));
    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), merge2DScripts, SLOT(setVerbosity(int)));
    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), merge3DScripts, SLOT(setVerbosity(int)));
    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), customScripts, SLOT(setVerbosity(int)));
    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), singleParticleScripts, SLOT(setVerbosity(int)));
    
    //Setup Maximize tool button
    QToolButton* maximizeLogWindow = new QToolButton();
    maximizeLogWindow->setFixedSize(QSize(20,20));
    maximizeLogWindow->setIcon(*(mainData->getIcon("maximize")));
    maximizeLogWindow->setToolTip("Maximize output view");
    maximizeLogWindow->setAutoRaise(false);
    maximizeLogWindow->setCheckable(true);
    
    connect(maximizeLogWindow, SIGNAL(toggled(bool)), this, SLOT(maximizeLogWindow(bool)));
    
    QWidget* verbosityControlWidget = new QWidget();
    QGridLayout* verbosityControlLayout = new QGridLayout(verbosityControlWidget);
    verbosityControlLayout->setMargin(0);
    verbosityControlLayout->setSpacing(0);
    verbosityControlWidget->setLayout(verbosityControlLayout);
    
    verbosityControlLayout->addWidget(new QLabel("Verbosity Level: "), 0, 0);
    verbosityControlLayout->addWidget(verbosityControl, 0, 1, 1, 1 , Qt::AlignVCenter);
    verbosityControlLayout->addItem(new QSpacerItem(3,3), 0, 2);
    verbosityControlLayout->addWidget(maximizeLogWindow, 0, 3);
    
    //Setup the window and add widgets
    blockContainer *logWindow = new blockContainer("Output (Double click for logbrowser)", this);
    logWindow->setMinimumWidth(400);
    logWindow->setMinimumHeight(200);
    logWindow->setMainWidget(logViewer);
    logWindow->setHeaderWidget(verbosityControlWidget);
    
    connect(logWindow, SIGNAL(doubleClicked()), this, SLOT(launchLogBrowser()));
    
    return logWindow;

}

blockContainer* executionContainer::setupParameterWindow() 
{
    localParameters = new resizeableStackedWidget(this);
    
    parameters = new confInterface(mainData, "");
    connect(results, SIGNAL(saved(bool)), parameters, SLOT(load()));

    QWidget *parametersWidget = new QWidget();
    QVBoxLayout *parameterLayout = new QVBoxLayout();
    parametersWidget->setLayout(parameterLayout);
    parameterLayout->setMargin(0);
    parameterLayout->setSpacing(0);
    parameterLayout->addWidget(localParameters);
    parameterLayout->addWidget(parameters);
    parameterLayout->setStretchFactor(localParameters, 1);
    parameterLayout->setStretchFactor(parameters, 100);

    QScrollArea *window = new QScrollArea(this);
    window->setWidgetResizable(true);
    window->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    window->setWidget(parametersWidget);
    
    
    //Setup Verbosity control combo box
    userLevelButtons = new QComboBox(this);
    userLevelButtons->addItems(QStringList() << "Simplified" << "Advanced");
    connect(userLevelButtons, SIGNAL(currentIndexChanged(int)), parameters, SLOT(setSelectionUserLevel(int)));
    
    //Setup Maximize tool button
    QToolButton* maximizeParameterWin = new QToolButton();
    maximizeParameterWin->setFixedSize(QSize(20,20));
    maximizeParameterWin->setIcon(*(mainData->getIcon("maximize")));
    maximizeParameterWin->setToolTip("Maximize parameter view");
    maximizeParameterWin->setAutoRaise(false);
    maximizeParameterWin->setCheckable(true);
    
    connect(maximizeParameterWin, SIGNAL(toggled(bool)), this, SLOT(maximizeParameterWindow(bool)));
    
    QWidget* parameterLevelWidget = new QWidget();
    QGridLayout* parameterLevelLayout = new QGridLayout(parameterLevelWidget);
    parameterLevelLayout->setMargin(0);
    parameterLevelLayout->setSpacing(0);
    parameterLevelWidget->setLayout(parameterLevelLayout);
    
    parameterLevelLayout->addWidget(new QLabel("Level: "), 0, 0);
    parameterLevelLayout->addWidget(userLevelButtons, 0, 1, 1, 1 , Qt::AlignVCenter);
    parameterLevelLayout->addItem(new QSpacerItem(3,3), 0, 2);
    parameterLevelLayout->addWidget(maximizeParameterWin, 0, 3);
    
    //Setup the window and add widgets
    blockContainer* parameterContainer = new blockContainer("Setup");
    parameterContainer->setMinimumWidth(400);
    parameterContainer->setMinimumHeight(200);
    parameterContainer->setMainWidget(window);
    parameterContainer->setHeaderWidget(parameterLevelWidget);
    
    return parameterContainer;
}



void executionContainer::addToScriptsWidget(QWidget *widget) 
{
    scriptsWidget->addWidget(widget);
    widget->resize(200, 20);
    widget->setMinimumWidth(200);

}

void executionContainer::execute(bool halt) {
    scriptModule* module = (scriptModule*) scriptsWidget->currentWidget();
    if (module->type() == scriptModule::merge2D) {
        merge2DScripts->execute(halt);
    }
    if (module->type() == scriptModule::merge3D) {
        merge3DScripts->execute(halt);
    }
    if (module->type() == scriptModule::custom) {
        customScripts->execute(halt);
    }
    if (module->type() == scriptModule::singleparticle) {
        singleParticleScripts->execute(halt);
    }

}

void executionContainer::updateStatusMessage(const QString& message) {
    progressBar->update();
    statusBar->showMessage(message);
}

void executionContainer::increaseScriptProgress(int increament) {
    if (progressBar->value() + increament <= progressBar->maximum())
        progressBar->setValue(progressBar->value() + increament);
    else
        progressBar->setValue(progressBar->maximum());
}

void executionContainer::setScriptProgress(int progress) {
    progressBar->setValue(progress);
}

void executionContainer::setMerge2DMode() {
    showMerge2DScripts->setChecked(true);
    showMerge3DScripts->setChecked(false);
    showCustomScripts->setChecked(false);
    showSPScripts->setChecked(false);
    scriptsWidget->setCurrentWidget(merge2DScripts);
    merge2DScripts->focusWidget();
}

void executionContainer::setMerge3DMode() {
    showMerge2DScripts->setChecked(false);
    showMerge3DScripts->setChecked(true);
    showCustomScripts->setChecked(false);
    showSPScripts->setChecked(false);
    scriptsWidget->setCurrentWidget(merge3DScripts);
    merge3DScripts->focusWidget();
}

void executionContainer::setCustomMode() {
    showMerge2DScripts->setChecked(false);
    showMerge3DScripts->setChecked(false);
    showCustomScripts->setChecked(true);
    showSPScripts->setChecked(false);
    scriptsWidget->setCurrentWidget(customScripts);
    customScripts->focusWidget();
}

void executionContainer::setSPMode() {
    showMerge2DScripts->setChecked(false);
    showMerge3DScripts->setChecked(false);
    showCustomScripts->setChecked(false);
    showSPScripts->setChecked(true);
    scriptsWidget->setCurrentWidget(singleParticleScripts);
    singleParticleScripts->focusWidget();
}


void executionContainer::scriptChanged(scriptModule *module, QModelIndex index) {
    updateStatusMessage(module->title(index));
    
    //  container->saveSplitterState(1);
    int uid = index.data(Qt::UserRole).toUInt();

    if (localIndex[uid] == 0 && module->conf(index)->size() != 0) {
        confInterface *local = new confInterface(module->conf(index), "");
        localIndex[uid] = localParameters->addWidget(local) + 1;
        if (localParameters->widget(localIndex[uid] - 1) == NULL) cerr << "Something's very wrong here." << endl;
        //    connect(userLevelButtons,SIGNAL(levelChanged(int)),local,SLOT(setSelectionUserLevel(int)));
    }

    if (manualIndex[uid] == 0 && !module->conf(index)->manual().isEmpty())
        manualIndex[uid] = manuals->addWidget(new confManual(mainData, module->conf(index))) + 1;

    if (localIndex[uid] - 1 < 0)
        localParameters->hide();
    else {
        localParameters->show();
        localParameters->setCurrentIndex(localIndex[uid] - 1);
    }

    manuals->setCurrentIndex(manualIndex[uid] - 1);

    parameters->select(module->displayedVariables(index));
    if (verbosityControl->currentIndex() != 0)
        logViewer->loadLogFile(module->logFile(index));
    else
        logViewer->clear();
    results->load(module->resultsFile(index));
    //  container->restoreSplitterState(1);
}

void executionContainer::merge2DScriptChanged(QModelIndex index) {
    scriptChanged(merge2DScripts, index);
}

void executionContainer::merge3DScriptChanged(QModelIndex index) {
    scriptChanged(merge3DScripts, index);
}

void executionContainer::customScriptChanged(QModelIndex index) {
    scriptChanged(customScripts, index);
}

void executionContainer::singleParticleScriptChanged(QModelIndex index) {
    scriptChanged(singleParticleScripts, index);
}

void executionContainer::scriptCompleted(scriptModule *module, QModelIndex index) {
    //  cerr<<"Script completed"<<endl;
    results->load(module->resultsFile(index));
    results->save();
    resultsView->load();
    
    emit scriptCompletedSignal();
}

void executionContainer::reload() {
    results->load();
}

void executionContainer::merge2DScriptCompleted(QModelIndex index) {
    //  cerr<<"Standard ";
    scriptCompleted(merge2DScripts, index);
}

void executionContainer::merge3DScriptCompleted(QModelIndex index) {
    //  cerr<<"Standard ";
    scriptCompleted(merge3DScripts, index);
}

void executionContainer::customScriptCompleted(QModelIndex index) {
    //  cerr<<"Custom ";
    scriptCompleted(customScripts, index);
}

void executionContainer::singleParticleScriptCompleted(QModelIndex index) {
    //  cerr<<"Single Particle ";
    scriptCompleted(singleParticleScripts, index);
}

void executionContainer::maximizeLogWindow(bool maximize) 
{
    if(maximize) 
    {
        centralContainer->setSizes(QList<int>() << 0 << 1);
        centerRightSplitter->setSizes(QList<int>() << 1 << 0);
    }
    else
    {
        centralContainer->setSizes(QList<int>() << 1 << 1);
        centerRightSplitter->setSizes(QList<int>() << 5 << 2);
    }
}

void executionContainer::maximizeParameterWindow(bool maximize) 
{
    if(maximize) 
    {
        centralContainer->setSizes(QList<int>() << 1 << 0);
        centerRightSplitter->setSizes(QList<int>() << 1 << 0);
    }
    else
    {
        centralContainer->setSizes(QList<int>() << 1 << 1);
        centerRightSplitter->setSizes(QList<int>() << 5 << 2);
    }
}

void executionContainer::launchFileBrowser() {
    QString path = QDir::toNativeSeparators(mainData->getDir("working"));
    QDesktopServices::openUrl(QUrl("file:///" + path));
}

void executionContainer::launchLogBrowser()
{
    QProcess::startDetached(mainData->getApp("logBrowser") + " " +logViewer->getLogFile());
}

void executionContainer::showManual(bool show) {
    if (show) {
        manuals->show();
    } else {
        manuals->hide();
    }
}

void executionContainer::updateFontInfo() 
{
    parameters->updateFontInfo();
    logViewer->updateFontInfo();
}