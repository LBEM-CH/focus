#include <QtWidgets>

#include "ImportFolderSettings.h"
#include "AutoImportWindow.h"
#include "ScriptModuleProperties.h"
#include "ScriptData.h"
#include "UserPreferences.h"
#include "GroupContainer.h"
#include "ScriptParser.h"
#include "ResultsData.h"
#include "ParameterWidget.h"

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
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
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
    QTableWidget* filesTable = new QTableWidget(0, 6);
    filesTable->setSelectionBehavior(QAbstractItemView::SelectRows);
    filesTable->setAttribute(Qt::WA_MacShowFocusRect, 0);

    QStringList labels;
    labels << "" << tr("Directory") << tr("Search Name") << tr("Averaged") << tr("Aligned") << tr("Raw");
    filesTable->setHorizontalHeaderLabels(labels);
    filesTable->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    filesTable->verticalHeader()->hide();
    filesTable->setShowGrid(false);
    filesTable->setAlternatingRowColors(true);

    return filesTable;
}

QWidget* AutoImportWindow::setupInputContainer() {
    QWidget* mainContainer = new QWidget;
    QVBoxLayout* mainLayout = new QVBoxLayout();
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addStretch(0);

    mainLayout->addWidget(setupInputFolderContainer(), 0);
    mainLayout->addWidget(setupOptionsContainter(), 0);
    mainLayout->addWidget(setupScriptsContainer(), 1);

    mainContainer->setMaximumWidth(580);
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
    ParametersConfiguration* conf  = projectData.projectParameterData();
    QString importImagesPath = conf->getValue("import_dir");
    filesDirBrowser_->setPath(importImagesPath);
    setupWatcherPaths();
    connect(filesDirBrowser_, &BrowserWidget::pathChanged, [ = ] (const QString & value){
        conf->set("import_dir", value);
        setupWatcherPaths();
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

    //Get the list of parameters to be displayed
    QFile s(ApplicationData::configDir().canonicalPath() + "/import.params.list");
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Import parameters read failed: " << ApplicationData::configDir().canonicalPath() + "/import.params.list";
        return new QWidget();
    }

    QStringList paramsList;
    while (!s.atEnd()) paramsList << s.readLine().simplified();
    s.close();
    
    //Setup the window and add widgets
    ParametersWidget* parameterContainer = new ParametersWidget(projectData.projectParameterData(), paramsList, 2);
    parameterContainer->setFrameStyle(QFrame::NoFrame);
    parameterContainer->setFixedHeight(260);
    parameterContainer->setMinimumHeight(100);

    return parameterContainer;
}

QWidget* AutoImportWindow::setupScriptsContainer() {

    GroupContainer* scriptsContainer = new GroupContainer;
    scriptsContainer->setTitle("Prepare by running following selection of scripts");
    QHBoxLayout* scriptContLayout = new QHBoxLayout();

    QStringList scriptsAvailable;
    
    QStringList scriptFolders = ScriptModuleProperties(ApplicationData::scriptsDir().absolutePath() + "/image/").subfolders();
    availaleScriptsBox = new QTabWidget;
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
        
        availaleScriptsBox->addTab(availableScriptCont, ApplicationData::icon(scriptProps.icon()), "");
    }

    selectedScriptsCont = new QListWidget;
    selectedScriptsCont->setSelectionMode(QAbstractItemView::ExtendedSelection);
    selectedScriptsCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
    resetSelectedScriptsContainer(scriptsAvailable);

    GraphicalButton* moveButton = new GraphicalButton(ApplicationData::icon("move_selected"));
    moveButton->setFixedSize(32, 32);
    connect(moveButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedScripts;
        for (int row = 0; row < selectedScriptsCont->count(); row++) {
            selectedScripts.append(selectedScriptsCont->item(row)->text());
        }

        QListWidget* availableScriptCont = static_cast<QListWidget*> (availaleScriptsBox->currentWidget());
        for (QModelIndex index : availableScriptCont->selectionModel()->selectedIndexes()) {
            if (!selectedScripts.contains(index.data(Qt::DisplayRole).toString())) selectedScripts.append(index.data(Qt::DisplayRole).toString());
        }
        

        ProjectPreferences(projectData.projectDir()).setImportScripts(selectedScripts);
        resetSelectedScriptsContainer(scriptsAvailable);
    });

    GraphicalButton* deleteButton = new GraphicalButton(ApplicationData::icon("delete_selected"));
    deleteButton->setFixedSize(32, 32);
    connect(deleteButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedScripts = ProjectPreferences(projectData.projectDir()).importScripts();
        for (QModelIndex index : selectedScriptsCont->selectionModel()->selectedIndexes()) {
            selectedScripts.removeAll(index.data(Qt::DisplayRole).toString());
        }
        ProjectPreferences(projectData.projectDir()).setImportScripts(selectedScripts);
        resetSelectedScriptsContainer(scriptsAvailable);
    });

    QVBoxLayout *buttonsLayout = new QVBoxLayout;
    buttonsLayout->setMargin(0);
    buttonsLayout->setSpacing(4);
    buttonsLayout->addStretch(0);
    buttonsLayout->addWidget(moveButton, 0);
    buttonsLayout->addWidget(deleteButton, 0);
    buttonsLayout->addStretch(0);

    scriptContLayout->addWidget(availaleScriptsBox);
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

    ParametersConfiguration* conf  = projectData.projectParameterData();
    QString importImagesPath = conf->getValue("import_dir");
    QString importAveragedFolder = conf->getValue("import_averages_folder");
    QString importAlignedFolder = conf->getValue("import_aligned_folder");
    QString importRawFolder =conf->getValue("import_raw_folder");
    QString importGroup =conf->getValue("import_target_group");
    QStringList ignoreImagePattern = conf->getValue("import_ignore_strings").split(' ');
    int imageNumberLength = conf->get("import_numberlength")->value().toInt();
    int rawOption = conf->get("import_rawstack_type")->value().toInt();

    if (importImagesPath.isEmpty() || !QFileInfo(importImagesPath).exists()) {
        std::cerr << "The import image path does not exist\n";
        return;
    }

    int uid = projectData.projectParameterData()->get("import_imagenumber")->value().toInt();

    bool addingAFile = false;
    
    QDir importDir(importImagesPath);
    ImportFolderSettings folderPreferences(importDir);
    QStringList alreadyImportedBaseNames = folderPreferences.importedNames();
    
    //If rawOption is 1, add tif extention to be searched
    QStringList rawExtentions;
    rawExtentions << "*.mrc" << "*.mrcs" << "*.tif" << "*.tiff";
    
    //Get a list of all available files
    QStringList fileNames = QDir(importImagesPath + "/" + importAveragedFolder).entryList(QStringList("*.mrc"), QDir::Files | QDir::NoSymLinks);
    fileNames.append(QDir(importImagesPath + "/" + importAlignedFolder).entryList(QStringList() << "*.mrc" << "*.mrcs", QDir::Files | QDir::NoSymLinks));
    fileNames.append(QDir(importImagesPath + "/" + importRawFolder).entryList(rawExtentions , QDir::Files | QDir::NoSymLinks));
    fileNames.removeDuplicates();
    
    for (QString image : fileNames) {
        bool copying = false;
        bool processed = false;
        bool hasAveraged = false;
        bool hasAligned = false;
        bool hasRaw = false;
        QString dirName;
        
        //get the basename and remove the to_be_ignored strings
        QString baseName = QFileInfo(image).completeBaseName();
        if (!ignoreImagePattern.isEmpty()) {
            for (QString pattern : ignoreImagePattern) {
                if (!pattern.trimmed().isEmpty()) baseName.remove(pattern.trimmed(), Qt::CaseInsensitive);
            }
        }
        
        if(alreadyImportedBaseNames.contains(baseName)) {
            processed = true;
            dirName = folderPreferences.linkedDirectory(baseName);
            hasAveraged = folderPreferences.hadAveraged(baseName);
            hasAligned = folderPreferences.hadAligned(baseName);
            hasRaw = folderPreferences.hadRaw(baseName);
        }
        else {
            QString imageNumber = ProjectData::commitIntToStringLength(++uid, imageNumberLength);

            while (QDir(projectData.projectDir().canonicalPath() + "/" + importGroup + "/" + imageNumber).exists()) {
                imageNumber = ProjectData::commitIntToStringLength(++uid, imageNumberLength);
            }
            
            dirToRowNumber_.insert(imageNumber, resultsTable_->rowCount());
            dirName = importGroup + "/" + imageNumber;
           
            toBeImported_.insert(imageNumber, QStringList() << baseName);
            
            //Check for averaged file
            QString averagedFile;
            if (QDir(importImagesPath + "/" + importAveragedFolder).exists()) {
                QStringList possibleFiles = QDir(importImagesPath + "/" + importAveragedFolder).entryList(QStringList(baseName + "*.mrc*"), QDir::Files | QDir::NoSymLinks);
                if (!possibleFiles.isEmpty()) {
                    hasAveraged = true;
                    averagedFile = importImagesPath + "/" + importAveragedFolder + "/" + possibleFiles.first();
                }
            }
            toBeImported_[imageNumber].append(averagedFile);

            //Check for movie file
            QString movieFile;
            if (QDir(importImagesPath + "/" + importAlignedFolder).exists()) {
                QStringList possibleFiles = QDir(importImagesPath + "/" + importAlignedFolder).entryList(QStringList(baseName + "*.mrc*"), QDir::Files | QDir::NoSymLinks);
                if (!possibleFiles.isEmpty()) {
                    hasAligned = true;
                    movieFile = importImagesPath + "/" + importAlignedFolder + "/" + possibleFiles.first();
                }
            }
            toBeImported_[imageNumber].append(movieFile);
            
            //Check for raw file
            QString rawFile;
            
            if (rawOption != 0) {
                if (QDir(importImagesPath + "/" + importRawFolder).exists()) {
                    QStringList possibleFiles = QDir(importImagesPath + "/" + importRawFolder).entryList(QStringList() << baseName + "*.mrc" << baseName + "*.mrcs" << baseName + "*.tif" << baseName + "*.tiff", QDir::Files | QDir::NoSymLinks);
                    if (!possibleFiles.isEmpty()) {
                        hasRaw = true;
                        rawFile = importImagesPath + "/" + importRawFolder + "/" + possibleFiles.first();
                    }
                }
            }
            toBeImported_[imageNumber].append(rawFile);
            
            //Check if the file is still being copied
            for(int i=1; i<toBeImported_[imageNumber].size(); ++i) {
                if(!isSafeToCopy(toBeImported_[imageNumber][i])) {
                    copying = true;
                    addingAFile = true;
                    dirName = "Not yet ready";
                    toBeImported_.remove(imageNumber);
                    break;
                }
            }
        }
        

        QTableWidgetItem *statusItem = new QTableWidgetItem();
        statusItem->setFlags(statusItem->flags() ^ Qt::ItemIsEditable);
        if(copying) statusItem->setIcon(ApplicationData::icon("import_copying"));
        else if (!processed) statusItem->setIcon(ApplicationData::icon("import_next"));
        else statusItem->setIcon(ApplicationData::icon("import_done"));
        
        QTableWidgetItem* imageItem = new QTableWidgetItem(baseName);
        imageItem->setFlags(imageItem->flags() ^ Qt::ItemIsEditable);
        
        QTableWidgetItem* numberItem = new QTableWidgetItem(dirName);
        numberItem->setFlags(numberItem->flags() ^ Qt::ItemIsEditable);

        QTableWidgetItem *averagedItem = new QTableWidgetItem();
        averagedItem->setFlags(averagedItem->flags() ^ Qt::ItemIsEditable);
        if (hasAveraged) averagedItem->setIcon(ApplicationData::icon("tick"));
        else averagedItem->setIcon(ApplicationData::icon("cross"));
        
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
        resultsTable_->setItem(row, 3, averagedItem);
        resultsTable_->setItem(row, 4, movieItem);
        resultsTable_->setItem(row, 5, rawItem);

    }

    if(addingAFile) timer_.start(30000);
    else timer_.stop();
    
    for (int i = 0; i < resultsTable_->columnCount(); ++i) resultsTable_->resizeColumnToContents(i);

    if(resultsTable_->rowCount()!=0) statusLabel_->setText(tr("%1 image(s) found in folder of which %2 image(s) are to be imported").arg(resultsTable_->rowCount()).arg(toBeImported_.keys().size()));
    else statusLabel_->setText("No such files could be found. (if not intended, please check the options again.)");
    
    if(toBeImported_.isEmpty()) importButton_->setDisabled(true);
    
    resultsTable_->scrollToBottom();
}

void AutoImportWindow::resetSelectedScriptsContainer(QStringList availScripts) {
    selectedScriptsCont->clear();
    QStringList selectedScripts = ProjectPreferences(projectData.projectDir()).importScripts();
    for (QString script : availScripts) {
        if (selectedScripts.contains(script)) {
            QList<QListWidgetItem*> foundItems;
            for(int i=0; i< availaleScriptsBox->count(); ++i)  {
                QListWidget* availCont = static_cast<QListWidget*>(availaleScriptsBox->widget(i));
                foundItems.append(availCont->findItems(script, Qt::MatchExactly));
            }
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

        QString importGroup_ = projectData.projectParameterData()->getValue("import_target_group");

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
    
    QString importGroup_ = projectData.projectParameterData()->getValue("import_target_group");
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
    conf->set("imagenumber", number, false);
    
    bool hasAveraged = false;
    bool hasAligned = false;
    bool hasRaw = false;
    
    //Get the original file name used for search
    QString baseName = files.first();
    
    //Check for the averaged file
    if(files.size() > 1 && !files[1].isEmpty()) {
        conf->set("imagename", baseName + "_2dx", false);
        conf->set("nonmaskimagename", baseName + "_2dx", false);
        conf->set("imagename_original", files[1], false);
        scriptsToBeExecuted_.insert(0, "cp -f " + files[1] + " " + workingDir.canonicalPath() + "/" + baseName + "_2dx.mrc");
        hasAveraged = true;
    }
    
    //Check for aligned file
    if (files.size() > 2 && !files[2].isEmpty()) {
        conf->set("movie_stackname", baseName + "_aligned", false);
        conf->set("movie_stackname_original", files[2], false);
        scriptsToBeExecuted_.insert(0, "cp -f " + files[2] + " " + workingDir.canonicalPath() + "/" + baseName + "_aligned.mrcs");
        hasAligned = true;
    }

    //Check for raw file
    if (files.size() > 3 && !files[3].isEmpty()) {
        int rawOption = conf->get("import_rawstack_type")->value().toInt();
        if(rawOption == 1) {
            conf->set("import_rawstack", baseName + '.' + QFileInfo(files[3]).suffix(), false);
            conf->set("import_rawstack_original", files[3], false);
            scriptsToBeExecuted_.insert(0, "cp -f " + files[3] + " " + workingDir.canonicalPath() + "/" + baseName + '.' + QFileInfo(files[3]).suffix());
            hasRaw = true;
        } else if (rawOption == 2) {
            conf->set("raw_gaincorrectedstack", baseName + "_raw", false);
            conf->set("raw_gaincorrectedstack_original", files[3], false);
            scriptsToBeExecuted_.insert(0, "cp -f " + files[3] + " " + workingDir.canonicalPath() + "/" + baseName + "_raw.mrcs");
            hasRaw = true;
        }
       
        
    }

    //Check for gain reference file
    //TODO
    
    conf->setModified(true);
    
    //Prepare for script execution
    currentWorkingDir_ = workingDir;
    
    //register that this image was imported
    ImportFolderSettings(QDir(conf->getValue("import_dir"))).addImportedImage(baseName, importGroup_ + "/" + number, hasAveraged, hasAligned, hasRaw);
    
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
    if(!QFileInfo(imageName).exists()) return true;
    
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

void AutoImportWindow::setupWatcherPaths() {
    ParametersConfiguration* conf  = projectData.projectParameterData();
    QString importImagesPath = conf->getValue("import_dir");
    QString importAveragedFolder = conf->getValue("import_averages_folder");
    QString importAlignedFolder = conf->getValue("import_aligned_folder");
    QString importRawFolder = conf->getValue("import_raw_folder");
    
    if(watcher_.directories().size() > 0) watcher_.removePaths(watcher_.directories());
    
    QString currDir;
    currDir = importImagesPath + '/' + importAveragedFolder;
    if(QFileInfo(currDir).isDir()) watcher_.addPath(currDir);
    
    currDir = importImagesPath + '/' + importAlignedFolder;
    if(QFileInfo(currDir).isDir()) watcher_.addPath(currDir);
    
    currDir = importImagesPath + '/' + importRawFolder;
    if(QFileInfo(currDir).isDir()) watcher_.addPath(currDir);
}


QString AutoImportWindow::introText() {
    return QString("Place the raw, aligned and averaged stacks in this directory with sub-folders separating them.");
}
