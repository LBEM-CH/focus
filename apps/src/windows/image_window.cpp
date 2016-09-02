/***************************************************************************
 *   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
 *   HStahlberg@ucdavis.edu                                                *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include "image_window.h"
#include  <QDesktopServices>
#include <iostream>
using namespace std;

ImageWindow::ImageWindow(confData *conf, QWidget *parent)
: QWidget(parent) {
    data = conf;

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

    standardScripts = new scriptModule(data, data->getDir("standardScripts"));
    scriptsWidget->addWidget(standardScripts);
    connect(standardScripts, SIGNAL(initialized()), data, SLOT(save()));
    connect(standardScripts, SIGNAL(currentScriptChanged(QModelIndex)), this, SLOT(standardScriptChanged(QModelIndex)));
    connect(standardScripts, SIGNAL(scriptCompleted(QModelIndex)), this, SLOT(standardScriptCompleted(QModelIndex)));
    connect(standardScripts, SIGNAL(allScriptsCompleted()), this, SLOT(stopPlay()));
    connect(standardScripts, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
    connect(standardScripts, SIGNAL(reload()), this, SLOT(reload()));
    connect(standardScripts, &scriptModule::shouldResetParams,
            [ = ] (QModelIndex index){
        parameters->resetParameters(standardScripts->variablesToReset(index));
    });

    customScripts = new scriptModule(data, data->getDir("customScripts"));
    scriptsWidget->addWidget(customScripts);
    connect(customScripts, SIGNAL(currentScriptChanged(QModelIndex)), this, SLOT(customScriptChanged(QModelIndex)));
    connect(customScripts, SIGNAL(scriptCompleted(QModelIndex)), this, SLOT(customScriptCompleted(QModelIndex)));
    connect(customScripts, SIGNAL(allScriptsCompleted()), this, SLOT(stopPlay()));
    connect(customScripts, SIGNAL(progress(int)), this, SLOT(setScriptProgress(int)));
    connect(customScripts, SIGNAL(reload()), this, SLOT(reload()));
    connect(customScripts, &scriptModule::shouldResetParams,
            [ = ] (QModelIndex index){
        parameters->resetParameters(customScripts->variablesToReset(index));
    });

    subscriptWidget = new QListView;
    subscriptWidget->setUniformItemSizes(true);
    subscriptWidget->setItemDelegate(new SpinBoxDelegate);
    connect(subscriptWidget, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(subscriptActivated(QModelIndex)));

    QSplitter* scriptsContainer = new QSplitter(Qt::Vertical);
    scriptsContainer->addWidget(scriptsWidget);
    scriptsContainer->addWidget(subscriptWidget);
    scriptsContainer->setStretchFactor(0, 2);
    scriptsContainer->setStretchFactor(1, 1);


    parameterContainer = setupParameterWindow();
    logWindow = setupLogWindow();

    centralSplitter = new QSplitter(this);
    centralSplitter->setOrientation(Qt::Vertical);

    centralSplitter->addWidget(parameterContainer);
    centralSplitter->addWidget(logWindow);
    centralSplitter->setStretchFactor(0, 1);
    centralSplitter->setStretchFactor(1, 1);

    /*           Results View Information               */

    QSplitter *resultsSplitter = new QSplitter(Qt::Vertical);

    blockContainer *resultsContainer = new blockContainer("Results");
    results = new resultsParser(data, QStringList() << "", resultsParser::results);
    results->setContextMenuPolicy(Qt::ActionsContextMenu);

    QAction *resultsLoadAction = new QAction("Re-Evaluate Results", results);
    results->addAction(resultsLoadAction);
    connect(resultsLoadAction, SIGNAL(triggered()), this, SLOT(reload()));

    resultsContainer->setMainWidget(results);

    resultsSplitter->addWidget(resultsContainer);
    resultsSplitter->setMinimumWidth(235);

    blockContainer *imagesContainer = new blockContainer("Images");
    connect(imagesContainer, SIGNAL(doubleClicked()), this, SLOT(launchFileBrowser()));

    blockContainer *previewContainer = new blockContainer("Preview");
    previewContainer->setFixedWidth(235);
    preview = new imagePreview(data, "", false, previewContainer);
    connect(preview, SIGNAL(load()), this, SLOT(refresh()));
    previewContainer->setMainWidget(preview);


    imageParser = new resultsParser(data, QStringList() << "", resultsParser::images);
    connect(imageParser, SIGNAL(imageSelected(const QString &)), preview, SLOT(setImage(const QString&)));
    connect(imageParser, SIGNAL(cellActivated(int, int)), preview, SLOT(launchNavigator()));


    imagesContainer->setMainWidget(imageParser);

    QToolButton* importantSwitch = new QToolButton();
    importantSwitch->setIcon(*(data->getIcon("important")));
    importantSwitch->setToolTip("Important images only");
    importantSwitch->setAutoRaise(false);
    importantSwitch->setCheckable(true);
    connect(importantSwitch, SIGNAL(toggled(bool)), imageParser, SLOT(setImportant(bool)));
    resultsSplitter->addWidget(imagesContainer);

    imagesContainer->setHeaderWidget(importantSwitch);

    QToolButton* showHeaderButton = new QToolButton();
    showHeaderButton->setIcon(*(data->getIcon("info")));
    showHeaderButton->setToolTip("Show Image Header");
    showHeaderButton->setAutoRaise(false);
    showHeaderButton->setCheckable(true);
    showHeaderButton->setChecked(false);
    connect(showHeaderButton, SIGNAL(toggled(bool)), preview, SLOT(showImageHeader(bool)));
    previewContainer->setHeaderWidget(showHeaderButton);

    QWidget *rightContainer = new QWidget;

    QVBoxLayout *rightLayout = new QVBoxLayout;
    rightLayout->setMargin(0);
    rightLayout->setSpacing(0);
    rightContainer->setLayout(rightLayout);
    rightLayout->addWidget(resultsSplitter);

    centerRightSplitter = new QSplitter(this);
    centerRightSplitter->setOrientation(Qt::Horizontal);
    centerRightSplitter->setHandleWidth(4);
    centerRightSplitter->addWidget(centralSplitter);
    centerRightSplitter->addWidget(rightContainer);
    centerRightSplitter->setStretchFactor(0, 5);
    centerRightSplitter->setStretchFactor(1, 2);

    //Setup Title Bar
    scriptLabel = new QLabel;
    QFont labelFont = scriptLabel->font();
    labelFont.setPointSize(20);
    scriptLabel->setFont(labelFont);
    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    runButton = new QPushButton;
    runButton->setIcon(*(data->getIcon("play")));
    runButton->setToolTip("Run/Stop script");
    runButton->setIconSize(QSize(18, 18));
    runButton->setCheckable(true);
    connect(runButton, &QPushButton::toggled, this, &ImageWindow::executing);
    connect(runButton, SIGNAL(toggled(bool)), this, SLOT(execute(bool)));

    refreshButton = new QPushButton;
    refreshButton->setIcon(*(data->getIcon("refresh_colored")));
    refreshButton->setToolTip("Refresh Results");
    refreshButton->setIconSize(QSize(18, 18));
    refreshButton->setCheckable(false);
    connect(refreshButton, SIGNAL(clicked()), this, SLOT(reload()));

    saveCfgButton = new QPushButton;
    saveCfgButton->setIcon(*(data->getIcon("save_project_default")));
    saveCfgButton->setToolTip("Save this configuration as project default");
    saveCfgButton->setIconSize(QSize(18, 18));
    saveCfgButton->setCheckable(false);
    connect(saveCfgButton, SIGNAL(clicked()), this, SLOT(saveAsProjectDefault()));

    manualButton = new QPushButton;
    manualButton->setIcon(*(data->getIcon("help")));
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
    titleLayout->addWidget(refreshButton);
    titleLayout->addWidget(saveCfgButton);
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

    blockContainer* statusParserCont = new blockContainer("Status");
    statusParser = new statusViewer(data->getDir("config") + "/2dx_status.html");
    statusParser->setConf(data);
    statusParser->load();
    statusParserCont->setMainWidget(statusParser);

    QWidget *statusContainer = new QWidget();
    statusContainer->setFixedHeight(235);

    QHBoxLayout *statusLayout = new QHBoxLayout();
    statusLayout->setMargin(0);
    statusLayout->setSpacing(0);
    statusLayout->addWidget(statusParserCont, 1);
    statusLayout->addWidget(previewContainer, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    statusContainer->setLayout(statusLayout);

    //Setup the layout and add widgets
    QGridLayout* layout = new QGridLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);
    setLayout(layout);

    /*LAYOUT
     * -----------------------------------------------------------
     * | TOOL   | SCRIPTS       | HEADER (0, 2, 1, 1)
     * | BAR    | WIDGET        |--------------------------------
     * |(0,0,   | (0,1,4,1)     | PROGRESSBAR (1, 2, 1, 1)
     * |   4,1) |               | -------------------------------
     * |        |               | CENTRAL RIGHT SPLITTER
     * |        |               | (2, 2, 1 , 1)
     * |        |               |
     * |        |               |
     * |        |               |
     * |        |               |__________________________________
     * |        |               | STATUS PARSER (3, 2, 1, 1)
     * -----------------------------------------------------------
     */

    layout->addWidget(setupToolbar(), 0, 0, 4, 1);
    layout->addWidget(scriptsContainer, 0, 1, 4, 1);
    layout->addWidget(headerContainer, 0, 2, 1, 1);
    layout->addWidget(progressBar, 1, 2, 1, 1);
    layout->addWidget(centerRightSplitter, 2, 2, 1, 1);
    layout->addWidget(statusContainer, 3, 2, 1, 1);

    layout->setRowStretch(0, 0);
    layout->setRowStretch(1, 0);
    layout->setRowStretch(2, 1);
    layout->setRowStretch(3, 0);

    verbosityControl->setCurrentIndex(1);

    //Just to set correct siezs
    maximizeLogWindow(false);
    maximizeParameterWindow(false);

    setStandardMode();
    standardScripts->initialize();
}

void ImageWindow::bridgeScriptLogConnection(bool bridge) {
    if (bridge) {
        connect(standardScripts, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
        connect(standardScripts, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));
        connect(customScripts, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
        connect(customScripts, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));
        connect(standardScripts, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
        connect(customScripts, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
    } else {
        disconnect(standardScripts, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
        disconnect(standardScripts, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));
        disconnect(customScripts, SIGNAL(standardOut(const QStringList &)), logViewer, SLOT(insertText(const QStringList &)));
        disconnect(customScripts, SIGNAL(standardError(const QByteArray &)), logViewer, SLOT(insertError(const QByteArray &)));
        disconnect(standardScripts, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
        disconnect(customScripts, SIGNAL(scriptLaunched()), logViewer, SLOT(clear()));
    }
}

blockContainer* ImageWindow::setupLogWindow() {
    blockContainer *logWindow = new blockContainer("Output (Double click for logbrowser)", this);
    logWindow->setMinimumWidth(400);
    logWindow->setMinimumHeight(200);

    //Setup Log Viewer
    logViewer = new LogViewer("Standard Output", NULL);
    bridgeScriptLogConnection(true);

    //Setup Verbosity control combo box
    verbosityControl = new QComboBox(this);
    verbosityControl->addItems(QStringList() << "Silent" << "Low" << "Moderate" << "Highest");

    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), logViewer, SLOT(load(int)));
    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), standardScripts, SLOT(setVerbosity(int)));
    connect(verbosityControl, SIGNAL(currentIndexChanged(int)), customScripts, SLOT(setVerbosity(int)));

    //Setup History View
    QToolButton* historyButton = new QToolButton();
    historyButton->setFixedSize(QSize(20, 20));
    historyButton->setIcon(*(data->getIcon("info")));
    historyButton->setToolTip("Processing history toggle");
    historyButton->setAutoRaise(false);
    historyButton->setCheckable(true);
    connect(historyButton, SIGNAL(toggled(bool)), this, SLOT(toggleHistoryView(bool)));

    //Setup Maximize tool button
    QToolButton* maximizeLogWindow = new QToolButton();
    maximizeLogWindow->setFixedSize(QSize(20, 20));
    maximizeLogWindow->setIcon(*(data->getIcon("maximize")));
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
    verbosityControlLayout->addWidget(verbosityControl, 0, 1, 1, 1, Qt::AlignVCenter);
    verbosityControlLayout->addItem(new QSpacerItem(3, 3), 0, 2);
    verbosityControlLayout->addWidget(historyButton, 0, 3);
    verbosityControlLayout->addItem(new QSpacerItem(3, 3), 0, 4);
    verbosityControlLayout->addWidget(maximizeLogWindow, 0, 5);

    //Setup the window and add widgets

    logWindow->setMainWidget(logViewer);
    logWindow->setHeaderWidget(verbosityControlWidget);

    connect(logWindow, SIGNAL(doubleClicked()), this, SLOT(launchLogBrowser()));

    return logWindow;

}

blockContainer* ImageWindow::setupParameterWindow() {
    parameters = new ParametersWidget(data, this);

    //Setup Verbosity control combo box
    userLevelButtons = new QComboBox(this);
    userLevelButtons->addItems(QStringList() << "Simplified" << "Advanced");
    connect(userLevelButtons, SIGNAL(currentIndexChanged(int)), parameters, SLOT(setSelectionUserLevel(int)));

    //Setup Maximize tool button
    QToolButton* maximizeParameterWin = new QToolButton();
    maximizeParameterWin->setFixedSize(QSize(20, 20));
    maximizeParameterWin->setIcon(*(data->getIcon("maximize")));
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
    parameterLevelLayout->addWidget(userLevelButtons, 0, 1, 1, 1, Qt::AlignVCenter);
    parameterLevelLayout->addItem(new QSpacerItem(3, 3), 0, 2);
    parameterLevelLayout->addWidget(maximizeParameterWin, 0, 3);

    //Setup the window and add widgets
    blockContainer* parameterContainer = new blockContainer("Setup");
    parameterContainer->setMinimumWidth(400);
    parameterContainer->setMinimumHeight(200);
    parameterContainer->setMainWidget(parameters);
    parameterContainer->setHeaderWidget(parameterLevelWidget);

    return parameterContainer;
}

QToolBar* ImageWindow::setupToolbar() {
    QToolBar* scriptsToolBar = new QToolBar("Choose Mode", this);
    scriptsToolBar->setOrientation(Qt::Vertical);
    scriptsToolBar->setIconSize(QSize(36, 36));

    showStandardScripts = new QToolButton(scriptsToolBar);
    showStandardScripts->setIcon(*(data->getIcon("standard")));
    showStandardScripts->setToolTip("Standard");
    showStandardScripts->setCheckable(true);
    connect(showStandardScripts, SIGNAL(clicked()), this, SLOT(setStandardMode()));

    showCustomScripts = new QToolButton(scriptsToolBar);
    showCustomScripts->setIcon(*(data->getIcon("custom")));
    showCustomScripts->setToolTip("Custom");
    showCustomScripts->setCheckable(true);
    connect(showCustomScripts, SIGNAL(clicked()), this, SLOT(setCustomMode()));

    scriptsToolBar->addWidget(showStandardScripts);
    scriptsToolBar->addWidget(showCustomScripts);

    return scriptsToolBar;

}

void ImageWindow::scriptChanged(scriptModule *module, QModelIndex index) {
    updateScriptLabel(module->title(index));

    int uid = index.data(Qt::UserRole).toUInt();

    QString text;
    QStringList helpTextList = module->getScriptManual(uid);
    for (int i = 0; i < helpTextList.size(); i++) text += helpTextList[i] + "\n";
    subTitleLabel->setText(text);

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
            if (subScriptTitle.endsWith(".py") || subScriptTitle.endsWith(".python")) subItem->setIcon(*data->getIcon("script_py"));
            else subItem->setIcon(*data->getIcon("script_csh"));

            subItem->setData(data->getDir("procDir") + "/" + subScriptTitle, Qt::UserRole + 5);
            if (!QFileInfo(data->getDir("procDir") + "/" + subScriptTitle).exists())
                subItem->setForeground(QColor(255, 0, 0));

            model->appendRow(subItem);
        }
    }
    subscriptWidget->setModel(model);

    currentResults = module->resultsFile(index);

    setScriptProgress(module->getScriptProgress(uid));

    parameters->changeParametersDisplayed(module->displayedVariables(index));
    currentLog = module->logFile(index);
    if (!visible["historyview"]) logViewer->loadLogFile(currentLog);
    results->setResult(module->resultsFile(index));
    imageParser->setResult(module->resultsFile(index));
    //warningWindow->load(module->resultsFile(index));
    parameters->update();
}

void ImageWindow::standardScriptChanged(QModelIndex index) {
    scriptChanged(standardScripts, index);
}

void ImageWindow::customScriptChanged(QModelIndex index) {
    scriptChanged(customScripts, index);
}

bool ImageWindow::parseResults(confData *conf, const QString &results) {
    currentResults = results;
    return scriptParser::parseResults(conf, results);
}

bool ImageWindow::parseResults() {
    return parseResults(data, currentResults);
}

void ImageWindow::scriptCompleted(scriptModule *module, QModelIndex index) {
    if (!module->isRunning())
        parseResults(data, module->resultsFile(index));
    //  results->setResult(module->resultsFile(index));
    imageParser->setResult(module->resultsFile(index));
    parameters->load();
    //statusParser->load();
}

void ImageWindow::standardScriptCompleted(QModelIndex index) {
    scriptCompleted(standardScripts, index);
}

void ImageWindow::customScriptCompleted(QModelIndex index) {
    scriptCompleted(customScripts, index);
}

void ImageWindow::setStandardMode() {
    showStandardScripts->setChecked(true);
    showCustomScripts->setChecked(false);
    scriptsWidget->setCurrentWidget(standardScripts);
    standardScripts->focusWidget();
}

void ImageWindow::setCustomMode() {
    showStandardScripts->setChecked(false);
    showCustomScripts->setChecked(true);
    scriptsWidget->setCurrentWidget(customScripts);
    customScripts->focusWidget();
}

void ImageWindow::maximizeLogWindow(bool maximize) {
    if (maximize) {
        centralSplitter->setSizes(QList<int>() << 0 << 1);
        centerRightSplitter->setSizes(QList<int>() << 1 << 0);
    } else {
        centralSplitter->setSizes(QList<int>() << 1 << 1);
        centerRightSplitter->setSizes(QList<int>() << 5 << 2);
    }
}

void ImageWindow::maximizeParameterWindow(bool maximize) {

    if (maximize) {
        centralSplitter->setSizes(QList<int>() << 1 << 0);
        centerRightSplitter->setSizes(QList<int>() << 1 << 0);
    } else {
        centralSplitter->setSizes(QList<int>() << 1 << 1);

        centerRightSplitter->setSizes(QList<int>() << 5 << 2);
    }
}

void ImageWindow::execute(bool run) {
    scriptModule* module = (scriptModule*) scriptsWidget->currentWidget();
    runningTabIndex = scriptsWidget->currentIndex();
    module->execute(run);
}

void ImageWindow::save() {
    data->save();
}

bool ImageWindow::modified() {
    return data->isModified();
}

void ImageWindow::reload() {
    parseResults();
    refresh();
}

void ImageWindow::stopPlay() {
    runningTabIndex = -1;
    runButton->setChecked(false);
    emit scriptCompletedSignal();
}

void ImageWindow::refresh() {
    parameters->load();
    //statusParser->load();
    results->load();
    imageParser->load();
    //statusParser->load();
}

void ImageWindow::updateScriptLabel(const QString& label) {
    progressBar->update();
    scriptLabel->setText(label);
}

void ImageWindow::setScriptProgress(int progress) {
    if (scriptsWidget->currentIndex() == runningTabIndex) progressBar->setValue(progress);
}

void ImageWindow::revert() {
    data->reload();
    parameters->load();
    //statusParser->load();
}

void ImageWindow::showSubTitle(bool s) {
    subTitleLabel->setVisible(s);
}

void ImageWindow::launchLogBrowser() {
    QProcess::startDetached(data->getApp("logBrowser") + " " + logViewer->getLogFile());
}

void ImageWindow::launchFileBrowser() {
    QString path = QDir::toNativeSeparators(data->getDir("working"));
    QDesktopServices::openUrl(QUrl("file:///" + path));
}

void ImageWindow::toggleHistoryView(bool show) {
    visible["historyview"] = show;
    if (show) {
        bridgeScriptLogConnection(false);
        logViewer->loadLogFile(data->getDir("working") + "/" + "History.dat");
        logWindow->setHeaderTitle("Processing history");
    } else {
        logViewer->loadLogFile(currentLog);
        bridgeScriptLogConnection(true);
        logWindow->setHeaderTitle("Output (Double click for logbrowser)");
    }
    verbosityControl->setCurrentIndex(verbosityControl->currentIndex());
}

void ImageWindow::useNewViewer(bool enable) {
    preview->enableNewViewer(enable);
}

void ImageWindow::subscriptActivated(QModelIndex item) {
    if (item.data(Qt::UserRole + 5).toString().isEmpty()) return;
    QProcess::startDetached(data->getApp("scriptEditor") + " " + item.data(Qt::UserRole + 5).toString());
}

void ImageWindow::saveAsProjectDefault() {
    if (QMessageBox::question(this,
            tr("Save as default?"), "Saving as project default will change master config file and set default values for all other new imported images in this project. \n\n 2DX will QUIT after saving the file. \n\n Proceed?",
            tr("Yes"),
            tr("No"),
            QString(), 0, 1) == 0) {
        emit saveAsProjectDefaultRequested(data);
    }
}

confData* ImageWindow::getConf() {
    return data;
}

bool ImageWindow::isRunningScript() {
    return runButton->isChecked();
}