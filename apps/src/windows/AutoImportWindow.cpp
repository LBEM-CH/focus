#include <QtWidgets>

#include "ImportFolderSettings.h"
#include "AutoImportWindow.h"
#include "ScriptModuleProperties.h"
#include "ScriptData.h"
#include "UserPreferences.h"
#include "GroupContainer.h"
#include "ScriptParser.h"
#include "ResultsData.h"

AutoImportWindow::AutoImportWindow(QWidget* parent)
: QWidget(parent) {
    resultsTable_ = setupFilesTable();

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    QFont font = statusLabel_->font();
    font.setBold(true);
    statusLabel_->setFont(font);

    importButton_ = new QPushButton(ApplicationData::icon("play"), tr("Start Import"));
    importButton_->setCheckable(true);
    importButton_->setChecked(false);
    connect(importButton_, &QAbstractButton::clicked, this, &AutoImportWindow::executeImport);
    
    refreshButton_ = new QPushButton(ApplicationData::icon("refresh"), tr("Rescan Import Folder"));
    connect(refreshButton_, &QAbstractButton::clicked, this, &AutoImportWindow::analyzeImport);
    
    inputContiner_ = setupInputContainer();
    inputContiner_->setMaximumWidth(550);

    QSplitter* mainSplitter = new QSplitter(Qt::Horizontal);
    mainSplitter->setHandleWidth(4);
    mainSplitter->addWidget(inputContiner_);
    mainSplitter->addWidget(setupStatusContinaer());

    mainSplitter->setStretchFactor(0, 1);
    mainSplitter->setStretchFactor(1, 1);

    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->addWidget(mainSplitter);
    setLayout(mainLayout);

    analyzeImport();
    if(ProjectPreferences(projectData.projectDir()).importRestartCheck()) executeImport(true);
    
    connect(&process_, static_cast<void(QProcess::*)(int)>(&QProcess::finished), this, &AutoImportWindow::continueExecution);
    connect(&watcher_, &QFileSystemWatcher::directoryChanged, [=] {
        analyzeImport();
        if(ProjectPreferences(projectData.projectDir()).importContinuousCheck()) {
            executeImport(true);
        }
    });
    
    connect(&timer_, &QTimer::timeout, [=] {
        analyzeImport();
        if(ProjectPreferences(projectData.projectDir()).importContinuousCheck()) {
            executeImport(true);
        }
    });
    
}

QTableWidget* AutoImportWindow::setupFilesTable() {
    QTableWidget* filesTable = new QTableWidget(0, 5);
    filesTable->setSelectionBehavior(QAbstractItemView::SelectRows);
    filesTable->setAttribute(Qt::WA_MacShowFocusRect, 0);

    QStringList labels;
    labels << "" << tr("Directory") << tr("Averaged Stack Name") << tr("Aligned") << tr("Raw");
    filesTable->setHorizontalHeaderLabels(labels);
    filesTable->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    filesTable->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    filesTable->verticalHeader()->hide();
    filesTable->setShowGrid(false);
    filesTable->setAlternatingRowColors(true);

    return filesTable;
}

QWidget* AutoImportWindow::setupInputContainer() {
    QWidget* mainContainer = new QWidget;
    QGridLayout* mainLayout = new QGridLayout();
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);

    mainLayout->addWidget(setupInputFolderContainer(), 0, 0);
    mainLayout->addWidget(setupOptionsContainter(), 1, 0);
    mainLayout->addWidget(setupScriptsContainer(), 2, 0);

    mainContainer->setMaximumWidth(550);
    mainContainer->setLayout(mainLayout);

    return mainContainer;
}

QWidget* AutoImportWindow::setupInputFolderContainer() {
    GroupContainer* container = new GroupContainer();
    container->setTitle("Import Folder");

    QFormLayout* layout = new QFormLayout;
    layout->setHorizontalSpacing(10);
    layout->setVerticalSpacing(2);
    layout->setRowWrapPolicy(QFormLayout::DontWrapRows);
    layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    layout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    layout->setLabelAlignment(Qt::AlignLeft);

    QDir projectPath = projectData.projectDir();

    BrowserWidget* filesDirBrowser_ = new BrowserWidget(BrowserWidget::BrowseType::DIRECTORY);
    filesDirBrowser_->setPath(ProjectPreferences(projectPath).importImageDir());
    QString currDir = ProjectPreferences(projectPath).importImageDir() + '/' + ProjectPreferences(projectPath).importAveragedFolder();
    if(QFileInfo(currDir).isDir()) watcher_.addPath(currDir);
    connect(filesDirBrowser_, &BrowserWidget::pathChanged, [ = ] (const QString & value){
        ProjectPreferences(projectPath).setImportImageDir(value);
        watcher_.removePaths(watcher_.directories());
        QString newDir = ProjectPreferences(projectPath).importImageDir() + '/' + ProjectPreferences(projectPath).importAveragedFolder();
        if(QFileInfo(newDir).isDir()) watcher_.addPath(newDir);
        analyzeImport();
    });
    layout->addRow(filesDirBrowser_);

    QLabel* introLabel = new QLabel(introText());
    introLabel->setWordWrap(true);
    QPalette pal = introLabel->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    introLabel->setPalette(pal);
    layout->addRow(introLabel);

    QCheckBox* restartCheck = new QCheckBox("Import new images in the import folder on start");
    restartCheck->setChecked(ProjectPreferences(projectPath).importRestartCheck());
    connect(restartCheck, &QCheckBox::toggled, [ = ] (bool check){
        ProjectPreferences(projectPath).setImportRestartCheck(check);
    });
    layout->addRow(restartCheck);

    QCheckBox* continuous = new QCheckBox("Continuously import new images in the import folder");
    continuous->setChecked(ProjectPreferences(projectPath).importContinuousCheck());
    connect(continuous, &QCheckBox::toggled, [ = ] (bool check){
        ProjectPreferences(projectPath).setImportContinuousCheck(check);
    });
    layout->addRow(continuous);
    
    container->setContainerLayout(layout);

    return container;
}

QWidget* AutoImportWindow::setupOptionsContainter() {
    QDir projectPath = projectData.projectDir();

    LineEditSet* averagedDir = new LineEditSet;
    averagedDir->setValue(ProjectPreferences(projectPath).importAveragedFolder());
    connect(averagedDir, &LineEditSet::valueChanged, [ = ] (const QString & value){
        ProjectPreferences(projectPath).setImportAveragedFolder(value);
        watcher_.removePaths(watcher_.directories());
        QString newDir = ProjectPreferences(projectPath).importImageDir() + '/' + ProjectPreferences(projectPath).importAveragedFolder();
        if(QFileInfo(newDir).isDir()) watcher_.addPath(newDir);
        analyzeImport();
    });

    LineEditSet* moviesDir = new LineEditSet;
    moviesDir->setValue(ProjectPreferences(projectPath).importAlignedFolder());
    connect(moviesDir, &LineEditSet::valueChanged, [ = ] (const QString & value){
        ProjectPreferences(projectPath).setImportAlignedFolder(value);
        analyzeImport();
    });

    LineEditSet* rawDir = new LineEditSet;
    rawDir->setValue(ProjectPreferences(projectPath).importRawFolder());
    connect(rawDir, &LineEditSet::valueChanged, [ = ] (const QString & value){
        ProjectPreferences(projectPath).setImportRawFolder(value);
        analyzeImport();
    });

    LineEditSet* ignoreImagePattern = new LineEditSet;
    ignoreImagePattern->setValue(ProjectPreferences(projectPath).importIgnorePattern());
    connect(ignoreImagePattern, &LineEditSet::valueChanged, [ = ] (const QString & value){
        ProjectPreferences(projectPath).setImportIgnorePattern(value);
        analyzeImport();
    });

    NoScrollComboBox* addToGroup_ = new NoScrollComboBox();
    addToGroup_->setEditable(true);
    addToGroup_->addItems(imageGroups());
    addToGroup_->setCurrentText(ProjectPreferences(projectPath).importGroup());
    addToGroup_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    connect(addToGroup_, static_cast<void(QComboBox::*)(const QString &)> (&QComboBox::currentTextChanged), [ = ] (const QString & value){
        ProjectPreferences(projectPath).setImportGroup(value);
        analyzeImport();
    });

    IntLineEditSet* numberLength_ = new IntLineEditSet();
    numberLength_->setAllRanges(5, 10);
    int pastNumberLength = ProjectPreferences(projectPath).importImageLength();
    if (pastNumberLength >= 5 && pastNumberLength <= 10) numberLength_->setValue(QString::number(pastNumberLength));
    else {
        numberLength_->setValue("5");
        ProjectPreferences(projectPath).setImportImageLength(5);
    }
    connect(numberLength_, &LineEditSet::valueChanged, [ = ] (const QString & value){
        ProjectPreferences(projectPath).setImportImageLength(value.toInt());
        analyzeImport();
    });

    QFormLayout* layout = new QFormLayout;
    layout->setHorizontalSpacing(10);
    layout->setVerticalSpacing(2);
    layout->setRowWrapPolicy(QFormLayout::DontWrapRows);
    layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    layout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    layout->setLabelAlignment(Qt::AlignLeft);
    layout->addRow("Subdirectory containing averaged stack", averagedDir);
    layout->addRow("Subdirectory containing aligned stack", moviesDir);
    layout->addRow("Subdirectory containing raw stack", rawDir);
    layout->addRow("String to be ignored at the end of image file names", ignoreImagePattern);
    layout->addRow("Add the imported images to group", addToGroup_);
    layout->addRow("Length of the target image number string (>=5 and <=10)", numberLength_);


    //Setup the window and add widgets
    GroupContainer* parameterContainer = new GroupContainer;
    parameterContainer->setTitle("Import Options");
    parameterContainer->setMinimumWidth(200);
    parameterContainer->setMinimumHeight(100);
    parameterContainer->setContainerLayout(layout);

    return parameterContainer;
}

QWidget* AutoImportWindow::setupScriptsContainer() {

    GroupContainer* scriptsContainer = new GroupContainer;
    scriptsContainer->setTitle("Prepare by running following selection of scripts");
    QHBoxLayout* scriptContLayout = new QHBoxLayout();

    QStringList scriptsAvailable;
    
    

    QStringList scriptFolders = ScriptModuleProperties(ApplicationData::scriptsDir().absolutePath() + "/image/").subfolders();
    QList<QListWidget*> availableScriptConts;
    QToolBox* availaleScriptsBox = new QToolBox;
    for (QString scriptFolder : scriptFolders) {

        QListWidget* availableScriptCont = new QListWidget();
        availableScriptCont->setSelectionMode(QAbstractItemView::ExtendedSelection);
        availableScriptCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
        
        QDir scriptDir = QDir(scriptFolder);
        ScriptModuleProperties scriptProps(scriptDir.absolutePath());

        QMap<int, QListWidgetItem*> map;
        quint32 sortOrder, uid;

        for (QString entry : scriptDir.entryList(QStringList() << "*.script", QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted)) {
            QStringList titleList;
            ScriptData scriptFileData(scriptDir.canonicalPath() + "/" + entry);
            sortOrder = scriptFileData.getProperty("sortOrder").toUInt();

            uid = qHash(scriptDir.canonicalPath() + "/" + entry)^qHash(sortOrder);

            titleList << QString(scriptFileData.getProperty("title")).simplified();

            QListWidgetItem* item = new QListWidgetItem(titleList.first());
            item->setToolTip(entry);
            item->setData(Qt::UserRole + 5, scriptDir.canonicalPath() + "/" + entry);
            item->setData(Qt::UserRole, uid);
            item->setTextAlignment(Qt::AlignVCenter);
            item->setIcon(ApplicationData::icon(scriptProps.scriptIcon()));
            item->setSizeHint(QSize(200, 30));

            map.insert(sortOrder, item);
        }

        QMapIterator<int, QListWidgetItem*> it(map);
        while (it.hasNext()) {
            it.next();
            availableScriptCont->addItem(it.value());
            scriptsAvailable.append(it.value()->text());
        }
        
        availableScriptConts.append(availableScriptCont);
        availaleScriptsBox->addItem(availableScriptCont, ApplicationData::icon(scriptProps.icon()), scriptProps.title());
    }

    selectedScriptsCont = new QListWidget;
    selectedScriptsCont->setSelectionMode(QAbstractItemView::ExtendedSelection);
    selectedScriptsCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
    resetSelectedScriptsContainer(availableScriptConts, scriptsAvailable);

    GraphicalButton* moveButton = new GraphicalButton(ApplicationData::icon("move_selected"));
    moveButton->setFixedSize(32, 32);
    connect(moveButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedScripts;
        for (int row = 0; row < selectedScriptsCont->count(); row++) {
            selectedScripts.append(selectedScriptsCont->item(row)->text());
        }

        for(QListWidget* availableScriptCont : availableScriptConts) {
            for (QModelIndex index : availableScriptCont->selectionModel()->selectedIndexes()) {
                if (!selectedScripts.contains(index.data(Qt::DisplayRole).toString())) selectedScripts.append(index.data(Qt::DisplayRole).toString());
            }
        }

        ProjectPreferences(projectData.projectDir()).setImportScripts(selectedScripts);
        resetSelectedScriptsContainer(availableScriptConts, scriptsAvailable);
    });

    GraphicalButton* deleteButton = new GraphicalButton(ApplicationData::icon("delete_selected"));
    deleteButton->setFixedSize(32, 32);
    connect(deleteButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedScripts = ProjectPreferences(projectData.projectDir()).importScripts();
        for (QModelIndex index : selectedScriptsCont->selectionModel()->selectedIndexes()) {
            selectedScripts.removeAll(index.data(Qt::DisplayRole).toString());
        }
        ProjectPreferences(projectData.projectDir()).setImportScripts(selectedScripts);
        resetSelectedScriptsContainer(availableScriptConts, scriptsAvailable);
    });

    QVBoxLayout *buttonsLayout = new QVBoxLayout;
    buttonsLayout->setMargin(0);
    buttonsLayout->setSpacing(4);
    buttonsLayout->addStretch(0);
    buttonsLayout->addWidget(moveButton, 0);
    buttonsLayout->addWidget(deleteButton, 0);
    buttonsLayout->addStretch(0);

    scriptContLayout->addWidget(new BlockContainer("Available Scripts", availaleScriptsBox));
    scriptContLayout->addLayout(buttonsLayout, 0);
    scriptContLayout->addWidget(new BlockContainer("Selected Scripts", selectedScriptsCont));
    scriptsContainer->setContainerLayout(scriptContLayout);
    return scriptsContainer;
}

QWidget* AutoImportWindow::setupStatusContinaer() {
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch(0);
    buttonLayout->addWidget(importButton_, 0);
    buttonLayout->addWidget(refreshButton_, 0);
    buttonLayout->addStretch(1);

    progressBar_ = new QProgressBar;
    progressBar_->setMaximum(100);
    progressBar_->setFixedHeight(10);
    progressBar_->setValue(0);
    progressBar_->setTextVisible(false);
    progressBar_->hide();

    statusEntryList_ = new QListWidget;
    statusEntryList_->setMaximumHeight(150);
    statusEntryList_->hide();

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->setMargin(10);
    mainLayout->setSpacing(10);
    mainLayout->addWidget(statusLabel_);
    mainLayout->addLayout(buttonLayout);
    mainLayout->addWidget(progressBar_);
    mainLayout->addWidget(statusEntryList_);
    mainLayout->addWidget(resultsTable_);

    GroupContainer* container = new GroupContainer;
    container->setTitle("Current Status");
    container->setContainerLayout(mainLayout);

    return container;
}

void AutoImportWindow::analyzeImport() {
    
    if(currentlyExecuting_) {
        qDebug()<< "The import is already running, not analyzing the import folder for now!";
        return;
    }
    
    dirToRowNumber_.clear();
    toBeImported_.clear();
    resultsTable_->setRowCount(0);
    importButton_->setEnabled(true);

    ProjectPreferences projectConfig(projectData.projectDir());
    QString importImagesPath = projectConfig.importImageDir();
    QString importAveragedFolder = projectConfig.importAveragedFolder();
    QString importAlignedFolder = projectConfig.importAlignedFolder();
    QString importRawFolder = projectConfig.importRawFolder();
    QString importGroup = projectConfig.importGroup();
    QString ignoreImagePattern = projectConfig.importIgnorePattern();
    int imageNumberLength = projectConfig.importImageLength();

    if (projectConfig.importImageDir().isEmpty() || !QFileInfo(importImagesPath).exists()) {
        std::cerr << "The import image path does not exist\n";
        return;
    }

    int uid = projectData.projectParameterData()->get("import_imagenumber")->value().toInt();

    bool addingAFile = false;
    
    ImportFolderSettings folderPreferences(QDir(projectConfig.importImageDir()));
    for (QString image : QDir(importImagesPath + "/" + importAveragedFolder).entryList(QStringList("*.mrc"), QDir::Files | QDir::NoSymLinks)) {
                
        bool copying = false;
        bool processed = false;
        bool hasAligned = false;
        bool hasRaw = false;
        QString dirName;
        if(!isSafeToCopy(importImagesPath + "/" + importAveragedFolder + "/" + image)) {
            copying = true;
            dirName = "File being edited";
            addingAFile = true;
        }
        else if(ImportFolderSettings(QDir(importImagesPath)).importedNames().contains(image)) {
            processed = true;
            dirName = folderPreferences.linkedDirectory(image);
            hasAligned = folderPreferences.hadAligned(image);
            hasRaw = folderPreferences.hadRaw(image);
        }
        else {
            QString imageNumber = ProjectData::commitIntToStringLength(++uid, imageNumberLength);

            while (QDir(projectData.projectDir().canonicalPath() + "/" + importGroup + "/" + imageNumber).exists()) {
                imageNumber = ProjectData::commitIntToStringLength(++uid, imageNumberLength);
            }
            
            dirToRowNumber_.insert(imageNumber, resultsTable_->rowCount());
            dirName = importGroup + "/" + imageNumber;
            toBeImported_.insert(imageNumber, QStringList() << importImagesPath + "/" + importAveragedFolder + "/" + image);

            //Check for movie file
            QString movieFile;
            if (QDir(importImagesPath + "/" + importAlignedFolder).exists()) {
                QString baseName = QFileInfo(importImagesPath + "/" + image).completeBaseName();
                if (!ignoreImagePattern.isEmpty()) {
                    baseName.contains(ignoreImagePattern, Qt::CaseInsensitive);
                    baseName.remove(ignoreImagePattern, Qt::CaseInsensitive);
                }

                QStringList possibleMovieFiles = QDir(importImagesPath + "/" + importAlignedFolder).entryList(QStringList(baseName + "*.mrc*"), QDir::Files | QDir::NoSymLinks);
                if (!possibleMovieFiles.isEmpty()) {
                    hasAligned = true;
                    movieFile = importImagesPath + "/" + importAlignedFolder + "/" + possibleMovieFiles.first();
                }
            }
            toBeImported_[imageNumber].append(movieFile);
            
            //Check for raw file
            QString rawFile;
            if (QDir(importImagesPath + "/" + importRawFolder).exists()) {
                QString baseName = QFileInfo(importImagesPath + "/" + image).completeBaseName();
                if (!ignoreImagePattern.isEmpty()) {
                    baseName.contains(ignoreImagePattern, Qt::CaseInsensitive);
                    baseName.remove(ignoreImagePattern, Qt::CaseInsensitive);
                }

                QStringList possibleMovieFiles = QDir(importImagesPath + "/" + importRawFolder).entryList(QStringList(baseName + "*.mrc*"), QDir::Files | QDir::NoSymLinks);
                if (!possibleMovieFiles.isEmpty()) {
                    hasRaw = true;
                    rawFile = importImagesPath + "/" + importRawFolder + "/" + possibleMovieFiles.first();
                }
            }
            toBeImported_[imageNumber].append(rawFile);
        }

        QTableWidgetItem *statusItem = new QTableWidgetItem();
        statusItem->setFlags(statusItem->flags() ^ Qt::ItemIsEditable);
        if(copying) statusItem->setIcon(ApplicationData::icon("import_copying"));
        else if (!processed) statusItem->setIcon(ApplicationData::icon("import_next"));
        else statusItem->setIcon(ApplicationData::icon("import_done"));
        
        QTableWidgetItem* imageItem = new QTableWidgetItem(image);
        imageItem->setFlags(imageItem->flags() ^ Qt::ItemIsEditable);
        
        QTableWidgetItem* numberItem = new QTableWidgetItem(dirName);
        numberItem->setFlags(numberItem->flags() ^ Qt::ItemIsEditable);

        QTableWidgetItem *movieItem = new QTableWidgetItem();
        movieItem->setFlags(movieItem->flags() ^ Qt::ItemIsEditable);
        if (hasAligned) movieItem->setIcon(ApplicationData::icon("tick"));
        else movieItem->setIcon(ApplicationData::icon("cross"));
        
        QTableWidgetItem *rawItem = new QTableWidgetItem();
        rawItem->setFlags(rawItem->flags() ^ Qt::ItemIsEditable);
        if (hasRaw) rawItem->setIcon(ApplicationData::icon("tick"));
        else rawItem->setIcon(ApplicationData::icon("cross"));

        int row = resultsTable_->rowCount();
        resultsTable_->insertRow(row);
        resultsTable_->setItem(row, 0, statusItem);
        resultsTable_->setItem(row, 1, numberItem);
        resultsTable_->setItem(row, 2, imageItem);
        resultsTable_->setItem(row, 3, movieItem);
        resultsTable_->setItem(row, 4, rawItem);

    }

    if(addingAFile) timer_.start(30000);
    else timer_.stop();
    
    for (int i = 0; i < resultsTable_->columnCount(); ++i) resultsTable_->resizeColumnToContents(i);

    if(resultsTable_->rowCount()!=0) statusLabel_->setText(tr("%1 image(s) found in folder of which %2 image(s) are to be imported").arg(resultsTable_->rowCount()).arg(toBeImported_.keys().size()));
    else statusLabel_->setText("No such files could be found. (if not intended, please check the options again.)");
    
    if(toBeImported_.isEmpty()) importButton_->setDisabled(true);
    
    resultsTable_->scrollToBottom();
}

void AutoImportWindow::resetSelectedScriptsContainer(QList<QListWidget*> availConts, QStringList availScripts) {
    selectedScriptsCont->clear();
    QStringList selectedScripts = ProjectPreferences(projectData.projectDir()).importScripts();
    for (QString script : availScripts) {
        if (selectedScripts.contains(script)) {
            QList<QListWidgetItem*> foundItems;
            for(QListWidget* availCont : availConts) foundItems.append(availCont->findItems(script, Qt::MatchExactly));
            if (foundItems.size() > 0) {
                QListWidgetItem* foundItem = foundItems.first();
                QListWidgetItem* item = new QListWidgetItem(foundItem->text());
                item->setToolTip(foundItem->toolTip());
                item->setData(Qt::UserRole + 5, foundItem->data(Qt::UserRole + 5));
                item->setData(Qt::UserRole, foundItem->data(Qt::UserRole));
                item->setTextAlignment(Qt::AlignVCenter);
                item->setIcon(foundItem->icon());
                item->setSizeHint(QSize(150, 30));

                selectedScriptsCont->addItem(item);
            }
        }
    }
}

void AutoImportWindow::executeImport(bool execute) {
    if (!execute || toBeImported_.isEmpty()) {
        importButton_->setChecked(false);
        importButton_->setText("Start Import");
        progressBar_->hide();
        inputContiner_->setEnabled(true);
        refreshButton_->setEnabled(true);
        scriptsToBeExecuted_.clear();
        
        if(!numberExecuting_.isEmpty()) projectData.indexImages();
        
        scriptExecuting_ = "";
        numberExecuting_ = "";
        
        if (process_.state() == QProcess::Running) {
            disconnect(&process_, static_cast<void(QProcess::*)(int)>(&QProcess::finished), this, &AutoImportWindow::continueExecution);
            process_.kill();
            while (!process_.waitForFinished()) process_.kill();
            connect(&process_, static_cast<void(QProcess::*)(int)>(&QProcess::finished), this, &AutoImportWindow::continueExecution);
        }
        currentlyExecuting_ =false;
        analyzeImport();
        
    } else if(currentlyExecuting_) {
        qDebug() << "Currently importing, skipping this import";
        return;
    } else {
        
        timer_.stop();
        
        currentlyExecuting_ = true;
        importButton_->setChecked(true);
        importButton_->setText("Stop Import");
        
        QStringList numbers = toBeImported_.keys();

        progressBar_->setMaximum(numbers.size());
        progressBar_->setValue(0);
        progressBar_->show();

        statusEntryList_->clear();
        statusEntryList_->show();
        
        inputContiner_->setDisabled(true);
        refreshButton_->setDisabled(true);

        QString importGroup_ = ProjectPreferences(projectData.projectDir()).importGroup();

        projectData.projectDir().mkpath(importGroup_);
        QFile(projectData.projectDir().absolutePath() + "/merge").link("../2dx_master.cfg", projectData.projectDir().absolutePath() + "/" + importGroup_ + "/2dx_master.cfg");
        importImage();
    }
}

void AutoImportWindow::importImage() {
    
    if(toBeImported_.isEmpty()) {
        executeImport(false);
        return;
    }
    
    if(!numberExecuting_.isEmpty()) {
        if(dirToRowNumber_.keys().contains(numberExecuting_)) {
            if(dirToRowNumber_[numberExecuting_] < resultsTable_->rowCount()) {
                if(resultsTable_->item(dirToRowNumber_[numberExecuting_], 0)) {
                    resultsTable_->item(dirToRowNumber_[numberExecuting_], 0)->setIcon(ApplicationData::icon("import_done"));
                }
            }
        }
    }
    
    QString importGroup_ = ProjectPreferences(projectData.projectDir()).importGroup();
    QString number = toBeImported_.keys().first();
    numberExecuting_ = number;
    QStringList files = toBeImported_[number];
    toBeImported_.remove(number);

    if (dirToRowNumber_.keys().contains(numberExecuting_)) {
        if (dirToRowNumber_[numberExecuting_] < resultsTable_->rowCount()) {
            if(resultsTable_->item(dirToRowNumber_[numberExecuting_], 0)) {
                resultsTable_->item(dirToRowNumber_[numberExecuting_], 0)->setIcon(ApplicationData::icon("import_working"));
            }
        }
    }

    progressBar_->setValue(progressBar_->value() + 1);
    statusLabel_->setText("Importing: " + number);

    addStatusToList("=========================================; Importing: " + number);
    
    projectData.projectParameterData()->set("import_imagenumber", number);
    
    scriptExecuting_ = "";
    scriptsToBeExecuted_ = selectedScriptPaths();

    //Create dir
    QDir workingDir = QDir(projectData.projectDir().canonicalPath() + "/" + importGroup_ + "/" + number);
    projectData.projectDir().mkpath(importGroup_ + "/" + number);
    addStatusToList("Created Directory: " + workingDir.canonicalPath());
    workingDir.mkpath("proc");
    workingDir.mkpath("LOGS");

    //Copy Config File
    projectData.projectParameterData()->saveAs(workingDir.canonicalPath() + "/2dx_image.cfg", true);
    addStatusToList("Created Configuration File.");

    ParametersConfiguration* conf = projectData.parameterData(workingDir);
    conf->set("imagename", number, false);
    conf->set("nonmaskimagename", number, false);
    conf->set("imagenumber", number, false);
    conf->set("imagename_original", files.first(), false);
    conf->set("import_gaincorrectedstack_2d", number + "_raw", false);

    scriptsToBeExecuted_.insert(0, "cp -f " + files.first() + " " + workingDir.canonicalPath() + "/" + number + "_raw.mrc");

    bool hasAligned = false;
    bool hasRaw = false;
    //Check for movie file
    if (files.size() > 1 && !files[1].isEmpty()) {
        conf->set("movie_stackname_raw", number + "_stack", false);
        scriptsToBeExecuted_.insert(0, "cp -f " + files[1] + " " + workingDir.canonicalPath() + "/" + number + "_stack.mrc");
        hasAligned = true;
    }

    //Check for raw file
    if (files.size() > 2 && !files[2].isEmpty()) {
        conf->set("import_rawstack_original", files[2], false);
        conf->set("import_rawstack", QFileInfo(files[2]).fileName(), false);
        scriptsToBeExecuted_.insert(0, "cp -f " + files[2] + " " + workingDir.canonicalPath() + "/" + QFileInfo(files[2]).fileName());
        hasRaw = true;
    }

    //ToDo: CHEN:
    //Ckech for gain reference file
    // 
    // 
    // 
    // 
    // 
    //

    conf->setModified(true);
    
    //Prepare for script execution
    currentWorkingDir_ = workingDir;
    

    //register that this image was imported
    ImportFolderSettings(QDir(ProjectPreferences(projectData.projectDir()).importImageDir()))
            .addImportedImage(QFileInfo(files.first()).fileName(), importGroup_ + "/" + number, hasAligned, hasRaw);
    
    //Copy Files
    addStatusToList("Copying Files...");
    continueExecution(0);
}

void AutoImportWindow::continueExecution(int exitCode) {
    if(exitCode != 0) {
        if(scriptExecuting_.isEmpty()) addStatusToList("Error in copying files!", true);
        else addStatusToList("Error in running script: " + scriptExecuting_, true);
    }
    
    if(!scriptExecuting_.isEmpty()) {
        addStatusToList("Saving results from script: " + scriptExecuting_ + " to " + numberExecuting_);
        ResultsData resultsData(currentWorkingDir_);
        resultsData.load(currentWorkingDir_.canonicalPath() + "/LOGS/" + scriptExecuting_ + ".results");
        resultsData.save();
    }
    
    if (scriptsToBeExecuted_.isEmpty()) {
        importImage();
        return;
    }

    ScriptParser parser(currentWorkingDir_.canonicalPath());
    QString scriptPath = scriptsToBeExecuted_.first();
    scriptsToBeExecuted_.removeFirst();

    if (scriptPath.startsWith("cp -f")) {
        process_.setWorkingDirectory(currentWorkingDir_.canonicalPath());
        process_.start(scriptPath, QIODevice::ReadOnly);
    } else {
        QString scriptName = QFileInfo(scriptPath).fileName().remove(QRegExp("\\.script$"));
        scriptExecuting_ = scriptName;
        if (QFileInfo(scriptPath).exists()) {
            addStatusToList("Executing: " + scriptName);

            parser.parse(scriptPath, currentWorkingDir_.canonicalPath() + "/proc/" + scriptName + ".com");
            process_.setWorkingDirectory(currentWorkingDir_.canonicalPath());
            process_.setStandardOutputFile(currentWorkingDir_.canonicalPath() + "/LOGS/" + scriptName + ".log");
            process_.start('"' + parser.executionString() + '"', QIODevice::ReadOnly);
        }
    }
}

QStringList AutoImportWindow::selectedScriptPaths() {
    QStringList paths;
    for (int row = 0; row < selectedScriptsCont->count(); row++) {
        paths.append(selectedScriptsCont->item(row)->data(Qt::UserRole + 5).toString());
    }
    return paths;
}

void AutoImportWindow::addStatusToList(const QString& text, bool error) {
    QStringList cell = text.split(';');
    for(QString s : cell) {
        if(!s.trimmed().isEmpty()) {
            QListWidgetItem* item = new QListWidgetItem(s.trimmed());
            item->setFlags(item->flags() & Qt::ItemIsEnabled);
            if(error) item->setForeground(Qt::red);
            statusEntryList_->addItem(item);
        }
    }
    statusLabel_->setText("Processing: " + numberExecuting_ + " (" + cell.last() + ")");
    statusEntryList_->scrollToBottom();
}

bool AutoImportWindow::isSafeToCopy(const QString& imageName) {
    if(!QFileInfo(imageName).exists()) return false;
    
    int safe_interval = 30000; //in milli secs
    if(QDateTime::currentMSecsSinceEpoch() - QFileInfo(imageName).lastModified().toMSecsSinceEpoch() < safe_interval) {
        return false;
    }
    else return true;
}


QStringList AutoImportWindow::imageGroups() {
    QString projectFolder = projectData.projectDir().canonicalPath();

    QStringList imageFolders = QDir(projectFolder).entryList(QDir::NoDotAndDotDot | QDir::Dirs);

    foreach(QString imageFolder, imageFolders) {
        if (!QFile(projectFolder + "/" + imageFolder + "/2dx_master.cfg").exists()) imageFolders.removeAll(imageFolder);
    }

    return imageFolders;
}

QString AutoImportWindow::introText() {
    return (QString("Place the raw, aligned and averaged stacks in this directory with sub-folders separating them. " +
            QString("The corresponding raw, aligned and averaged stacks in the folders should ") +
            QString("have same starting file names.")));
}
