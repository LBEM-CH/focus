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
#include "ScriptSelectorDialog.h"
#include "ImageScriptProcessor.h"

QMutex AutoImportWindow::mutex_;

AutoImportWindow::AutoImportWindow(QWidget* parent)
: QWidget(parent) {
    resultsTable_ = setupFilesTable();

    safeIntervalBox = new QSpinBox();
    safeIntervalBox->setMinimum(30);
    safeIntervalBox->setMaximum(84600);
    safeIntervalBox->setValue(ProjectPreferences().importSafeInterval());

    statusLabel_ = new QLabel(this);
    statusLabel_->setWordWrap(true);
    QFont font = statusLabel_->font();
    font.setBold(true);
    statusLabel_->setFont(font);
    
    deleteLabel_ = new QLabel("Original images will be DELETED after import! If not intended, change the option on left.");
    deleteLabel_->setWordWrap(true);
    deleteLabel_->hide();
    QPalette pal = deleteLabel_->palette();
    pal.setColor(QPalette::WindowText, Qt::red);
    deleteLabel_->setPalette(pal);

    importButton_ = new QPushButton(ApplicationData::icon("play"), tr("Start Import"));
    importButton_->setCheckable(true);
    importButton_->setChecked(false);
    connect(importButton_, &QAbstractButton::clicked, this, &AutoImportWindow::executeImport);
    
    refreshButton_ = new QPushButton(ApplicationData::icon("refresh"), tr("Rescan Import Folder"));
    connect(refreshButton_, &QAbstractButton::clicked, this, &AutoImportWindow::analyzeImport);
    
    priorityQueueOption_ = new QCheckBox("Prioritize the processing of imported images");
    priorityQueueOption_->setChecked(true);
    
    continuous = new QCheckBox("Continuously import new images in the import folder");
    continuous->setChecked(false);
    connect(continuous, &QCheckBox::toggled, [ = ] (bool check){
        if(check) timer_.start(safeIntervalBox->value()*1000);
        else timer_.stop();
    });
    
    inputContiner_ = setupInputContainer();

    QSplitter* mainSplitter = new QSplitter(Qt::Horizontal);
    mainSplitter->setHandleWidth(4);
    mainSplitter->addWidget(inputContiner_);
    mainSplitter->addWidget(setupStatusContinaer());
    
    mainSplitter->setStretchFactor(0, 1);
    mainSplitter->setStretchFactor(1, 1);
    
    int width = mainSplitter->width();
    mainSplitter->setSizes(QList<int>() << width/2 << width/2);

    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(mainSplitter);
    setLayout(mainLayout);

    analyzeImport();
    if(ProjectPreferences().importRestartCheck()) executeImport(true);
    
    connect(&process_, static_cast<void(QProcess::*)(int)>(&QProcess::finished), this, &AutoImportWindow::continueExecution);
    
    connect(&watcher_, &QFileSystemWatcher::directoryChanged, [=] {
        analyzeImport();
        if(continuous->isChecked()) executeImport(true);
    });
    
    connect(&timer_, &QTimer::timeout, [=] {
        analyzeImport();
        if(continuous->isChecked()) executeImport(true);
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
    
    connect(filesTable, &QTableWidget::itemDoubleClicked, [=](QTableWidgetItem *item){
        if(item->row() != -1 && item->row() < rowToImagePaths_.size()) {
            QString path = rowToImagePaths_[item->row()];
            ProjectImage* image = projectData.projectImage(QDir(projectData.projectDir().absoluteFilePath(path)));
            if(image) emit imageToBeOpened(image);
            else QMessageBox::warning(this, "Image Open Error!", "Image: " + path + " was either not imported or not found.");
        }
    });

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

    mainContainer->setLayout(mainLayout);

    return mainContainer;
}

QWidget* AutoImportWindow::setupInputFolderContainer() {
    GroupContainer* container = new GroupContainer();
    container->setTitle("Import Folder");

    QFormLayout* layout = new QFormLayout;
    layout->setHorizontalSpacing(10);
    layout->setVerticalSpacing(0);
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
    restartCheck->setChecked(ProjectPreferences().importRestartCheck());
    connect(restartCheck, &QCheckBox::toggled, [ = ] (bool check){
        ProjectPreferences().setImportRestartCheck(check);
    });
    layout->addRow(restartCheck);

    
    layout->addRow(continuous);
    
    deleteCheck = new QCheckBox("DELETE the original images in import folder after importing them");
    deleteCheck->setChecked(ProjectPreferences().importDeleteCheck());
    deleteLabel_->setVisible(deleteCheck->isChecked());
    connect(deleteCheck, &QCheckBox::toggled, [ = ] (bool check){
        ProjectPreferences().setImportDeleteCheck(check);
        deleteLabel_->setVisible(check);
    });
    layout->addRow(deleteCheck);
    
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
    parameterContainer->setFixedHeight(300);

    return parameterContainer;
}

QWidget* AutoImportWindow::setupScriptsContainer() {
    GroupContainer* scriptsContainer = new GroupContainer;
    scriptsContainer->setTitle("Process after import");
    
    QLabel* introLabel = new QLabel("After the import, following scripts will be processed on the images.");
    introLabel->setWordWrap(true);
    QPalette pal = introLabel->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    introLabel->setPalette(pal);

    selectedScriptsCont = new QListWidget;
    selectedScriptsCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
    resetSelectedScriptsContainer();

    QPushButton* changeButton = new QPushButton("Change scripts");
    connect(changeButton, &QPushButton::clicked, [=]{
        if(scriptSelectorDialog.exec()) {
            resetSelectedScriptsContainer();
        }
    });
    
    QHBoxLayout *buttonsLayout = new QHBoxLayout;
    buttonsLayout->setMargin(0);
    buttonsLayout->setSpacing(4);
    buttonsLayout->addStretch(0);
    buttonsLayout->addWidget(changeButton, 0);
    buttonsLayout->addStretch(1);
    buttonsLayout->addWidget(priorityQueueOption_, 0);


    QVBoxLayout* scriptContLayout = new QVBoxLayout();
    scriptContLayout->addWidget(introLabel, 0);
    scriptContLayout->addLayout(buttonsLayout, 0);
    scriptContLayout->addWidget(new BlockContainer("Selected Scripts", selectedScriptsCont));
    scriptsContainer->setContainerLayout(scriptContLayout);
    return scriptsContainer;
}

QWidget* AutoImportWindow::setupStatusContinaer() {

    safeIntervalBox->setValue(ProjectPreferences().importSafeInterval());
    connect(safeIntervalBox, static_cast<void(QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] (int i){
        ProjectPreferences().setImportSafeInterval(i);
        
    });
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch(0);
    buttonLayout->addWidget(importButton_, 0);
    buttonLayout->addWidget(refreshButton_, 0);
    buttonLayout->addStretch(1);
    
    QHBoxLayout* timerLayout = new QHBoxLayout();
    timerLayout->addStretch(0);
    timerLayout->addWidget(new QLabel("Number of seconds to wait before starting import of fresh (newly created) images"), 0);
    timerLayout->addWidget(safeIntervalBox, 0);
    timerLayout->addStretch(1);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->setMargin(10);
    mainLayout->setSpacing(10);
    mainLayout->addWidget(statusLabel_, 0);
    mainLayout->addLayout(buttonLayout, 0);
    mainLayout->addLayout(timerLayout, 0);
    mainLayout->addWidget(deleteLabel_, 0);
    mainLayout->addWidget(resultsTable_, 1);

    GroupContainer* container = new GroupContainer;
    container->setTitle("Current Status");
    container->setContainerLayout(mainLayout);

    return container;
}

void AutoImportWindow::analyzeImport(bool force) {
    
    if(currentlyExecuting_ && !force) {
        qDebug()<< "The import is already running, not analyzing the import folder for now!";
        return;
    }
    
    QMutexLocker locker(&AutoImportWindow::mutex_);
    
    dirToRowNumber_.clear();
    rowToImagePaths_.clear();
    toBeImported_.clear();
    resultsTable_->setRowCount(0);

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
    
    QStringList stackExtentions;
    stackExtentions << "*.mrc" << "*.mrcs" << "*.tif" << "*.tiff";
    QStringList avgExtentions;
    avgExtentions << "*.mrc" << "*.tif" << "*.tiff";
    
    //Get a list of all available files
    QStringList fileNames = QDir(importImagesPath + "/" + importAveragedFolder).entryList(avgExtentions, QDir::Files | QDir::NoSymLinks);
    fileNames.append(QDir(importImagesPath + "/" + importAlignedFolder).entryList(stackExtentions, QDir::Files | QDir::NoSymLinks));
    fileNames.append(QDir(importImagesPath + "/" + importRawFolder).entryList(stackExtentions , QDir::Files | QDir::NoSymLinks));
    fileNames.removeDuplicates();
    
    QStringList baseNames;
    for(QString image: fileNames) {
         //get the basename and remove the to_be_ignored strings
        QString baseName = QFileInfo(image).completeBaseName();
        if (!ignoreImagePattern.isEmpty()) {
            for (QString pattern : ignoreImagePattern) {
                if (!pattern.trimmed().isEmpty()) baseName.remove(pattern.trimmed(), Qt::CaseInsensitive);
            }
        }
        baseNames.append(baseName);
    }
    baseNames.removeDuplicates();
    
    for (QString baseName : baseNames) {
        bool copying = false;
        bool processed = false;
        bool hasAveraged = false;
        bool hasAligned = false;
        bool hasRaw = false;
        QString dirName;
        
        if(toBeImported_.keys().contains(baseName)) {
            continue;
        }
        
        if(alreadyImportedBaseNames.contains(baseName) 
                && QFileInfo(projectData.projectDir().canonicalPath() + '/' + folderPreferences.linkedDirectory(baseName) + "/2dx_image.cfg").exists()) {
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
            
            //Search string for avg File
            QStringList avgSearchStrings, stackSearchStrings;
            for(QString ext : avgExtentions) avgSearchStrings.append(baseName + "*" + ext);
            for(QString ext : stackExtentions) stackSearchStrings.append(baseName + "*" + ext);
            
            //Check for averaged file
            QString averagedFile;
            if (QDir(importImagesPath + "/" + importAveragedFolder).exists()) {
                QStringList possibleFiles = QDir(importImagesPath + "/" + importAveragedFolder).entryList(avgSearchStrings, QDir::Files | QDir::NoSymLinks);
                if (!possibleFiles.isEmpty()) {
                    hasAveraged = true;
                    averagedFile = importImagesPath + "/" + importAveragedFolder + "/" + possibleFiles.first();
                }
            }
            toBeImported_[imageNumber].append(averagedFile);

            //Check for movie file
            QString movieFile;
            if (QDir(importImagesPath + "/" + importAlignedFolder).exists()) {
                QStringList possibleFiles = QDir(importImagesPath + "/" + importAlignedFolder).entryList(stackSearchStrings, QDir::Files | QDir::NoSymLinks);
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
                    QStringList possibleFiles = QDir(importImagesPath + "/" + importRawFolder).entryList(stackSearchStrings, QDir::Files | QDir::NoSymLinks);
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
        
        rowToImagePaths_.append(dirName);

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

    for (int i = 0; i < resultsTable_->columnCount(); ++i) resultsTable_->resizeColumnToContents(i);

    if(resultsTable_->rowCount()!=0) statusLabel_->setText(tr("%1 image(s) found in folder of which %2 image(s) are to be imported").arg(resultsTable_->rowCount()).arg(toBeImported_.keys().size()));
    else statusLabel_->setText("No such files could be found. (if not intended, please check the options again.)");
    
    if(toBeImported_.isEmpty()) importButton_->setDisabled(true);
    else importButton_->setEnabled(true);
    
    resultsTable_->scrollToBottom();
}

void AutoImportWindow::resetSelectedScriptsContainer() {
    selectedScriptsCont->clear();
    QStringList selectedScripts = ProjectPreferences().processScripts();

    for(QString script : selectedScripts) {
        QListWidgetItem* item = new QListWidgetItem(script);
        selectedScriptsCont->addItem(item);
    }    
}

void AutoImportWindow::resetState() {
    if(!currentlyExecuting_) {
        toBeImported_.clear();
        importButton_->setChecked(false);
        importButton_->setText("Start Import");
        inputContiner_->setEnabled(true);
        refreshButton_->setEnabled(true);
        scriptsToBeExecuted_.clear();
        imageExecuting_ = 0;
    } else {
        importButton_->setChecked(true);
        importButton_->setText("Stop Import");
        inputContiner_->setDisabled(true);
        refreshButton_->setDisabled(true);
    }
}

void AutoImportWindow::finishExecution() {
    currentlyExecuting_ = false;
    resetState();
    analyzeImport();
}

void AutoImportWindow::executeImport(bool execute) {
    if (!execute) {
        int choice = QMessageBox::question(this, "Confirm stop", QString("Please select how you want to stop\n\nIf you stop now, the images which are ") +
                "being copied will be stopped.\n", "Finish current and stop", "Stop now", "Continue importing", 0, 2);
        if(choice == 0) {
            continuous->setChecked(false);
            toBeImported_.clear();
            resetState();
        } else if(choice == 1) {
            if (process_.state() == QProcess::Running) {
                disconnect(&process_, static_cast<void(QProcess::*)(int)>(&QProcess::finished), this, &AutoImportWindow::continueExecution);
                process_.kill();
                while (!process_.waitForFinished()) process_.kill();
                connect(&process_, static_cast<void(QProcess::*)(int)>(&QProcess::finished), this, &AutoImportWindow::continueExecution);
            }
            finishExecution();
        } else {
            resetState();
        }
    } else if(currentlyExecuting_) {
        qDebug() << "Currently importing, skipping this import";
        return;
    } else if(toBeImported_.isEmpty()) {
        resetState();
        return;
    } else {
        currentlyExecuting_ = true;
        resetState();

        timer_.stop();
        
        QString importGroup_ = projectData.projectParameterData()->getValue("import_target_group");
        projectData.projectDir().mkpath(importGroup_);
        QFile(projectData.projectDir().absolutePath() + "/merge").link("../2dx_master.cfg", projectData.projectDir().absolutePath() + "/" + importGroup_ + "/2dx_master.cfg");
        importImage();
    }
}

void AutoImportWindow::importImage() {
    
    if(continuous->isChecked()) {
        analyzeImport(true);
    }
    
    if(toBeImported_.isEmpty()) {
        finishExecution();
        return;
    }

    if (imageExecuting_) {
        QString numberExec = imageExecuting_->directory();
        if (!numberExec.isEmpty()) {
            if (dirToRowNumber_.keys().contains(numberExec)) {
                if (dirToRowNumber_[numberExec] < resultsTable_->rowCount()) {
                    if (resultsTable_->item(dirToRowNumber_[numberExec], 0)) {
                        resultsTable_->item(dirToRowNumber_[numberExec], 0)->setIcon(ApplicationData::icon("import_done"));
                    }
                }
            }
        }
    }
    
    QString importGroup_ = projectData.projectParameterData()->getValue("import_target_group");

    QString number;
    number = toBeImported_.keys().first();

    QStringList files = toBeImported_[number];
    toBeImported_.remove(number);
    
    if (dirToRowNumber_.keys().contains(number)) {
        if (dirToRowNumber_[number] < resultsTable_->rowCount()) {
            if(resultsTable_->item(dirToRowNumber_[number], 0)) {
                resultsTable_->item(dirToRowNumber_[number], 0)->setIcon(ApplicationData::icon("import_working"));
                resultsTable_->scrollToItem(resultsTable_->item(dirToRowNumber_[number], 0));
            }
        }
    }
    
    statusLabel_->setText(QString("Currently importing and %2 are in queue...").arg(toBeImported_.keys().size()));
    projectData.projectParameterData()->set("import_imagenumber", number);
    
    //Create dir
    QDir workingDir = QDir(projectData.projectDir().canonicalPath() + "/" + importGroup_ + "/" + number);
    
    //create Dir
    projectData.projectDir().mkpath(importGroup_ + "/" + number);
    workingDir.mkpath("proc");
    workingDir.mkpath("LOGS");

    //Copy Config File
    projectData.projectParameterData()->saveAs(workingDir.canonicalPath() + "/2dx_image.cfg", true);

    imageExecuting_ = projectData.addImage(importGroup_, number);
    ParametersConfiguration* conf = imageExecuting_->parameters();
    conf->set("imagenumber", number, false);
    
    bool hasAveraged = false;
    bool hasAligned = false;
    bool hasRaw = false;
    
    //Get the original file name used for search
    QString baseName = files.first();
    
    //Check for the averaged file
    if(files.size() > 1 && !files[1].isEmpty()) {
        conf->set("imagename", "image_2dx", false);
        conf->set("nonmaskimagename", "image_2dx", false);
        conf->set("imagename_original", files[1], false);
        conf->set("import_original_time", QString::number(QFileInfo(files[1]).created().toMSecsSinceEpoch()));
        scriptsToBeExecuted_.append("cp -f " + files[1] + " " + workingDir.canonicalPath() + "/" + "image_2dx.mrc");
        if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[1]);
        hasAveraged = true;
    }
    
    //Check for aligned file
    if (files.size() > 2 && !files[2].isEmpty()) {
        conf->set("movie_stackname", "movie_aligned", false);
        conf->set("movie_stackname_original", files[2], false);
        conf->set("import_original_time", QString::number(QFileInfo(files[2]).created().toMSecsSinceEpoch()));
        scriptsToBeExecuted_.append("cp -f " + files[2] + " " + workingDir.canonicalPath() + "/" + "movie_aligned.mrcs");
        if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[2]);
        hasAligned = true;
    }

    //Check for raw file
    if (files.size() > 3 && !files[3].isEmpty()) {
        int rawOption = conf->get("import_rawstack_type")->value().toInt();
        if(rawOption == 1) {
            conf->set("import_rawstack", baseName + '.' + QFileInfo(files[3]).suffix(), false);
            conf->set("import_rawstack_original", files[3], false);
            conf->set("import_original_time", QString::number(QFileInfo(files[3]).created().toMSecsSinceEpoch()));
            scriptsToBeExecuted_.append("cp -f " + files[3] + " " + workingDir.canonicalPath() + "/" + baseName + '.' + QFileInfo(files[3]).suffix());
            if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[3]);
            hasRaw = true;
        } else if (rawOption == 2) {
            conf->set("import_rawstack", baseName + '.' + QFileInfo(files[3]).suffix(), false);
            conf->set("import_rawstack_original", files[3], false);
            conf->set("raw_gaincorrectedstack", "raw_gaincorrectedstack", false);
            conf->set("raw_gaincorrectedstack_original", files[3], false);
            conf->set("import_original_time", QString::number(QFileInfo(files[3]).created().toMSecsSinceEpoch()));
            scriptsToBeExecuted_.append("cp -f " + files[3] + " " + workingDir.canonicalPath() + "/" + "raw_gaincorrectedstack.mrcs");
            if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[3]);
            hasRaw = true;
        }
    }

    //Check for defects list file
    QString defectsFile = conf->getValue("import_defects_original");
    if(QFileInfo(defectsFile).exists()) {
        conf->set("import_defects", "../" + QFileInfo(defectsFile).fileName());
        scriptsToBeExecuted_.append("cp -f " + defectsFile + " " + workingDir.canonicalPath() + "/../" + QFileInfo(defectsFile).fileName());
    }
    
    //Check for gain reference file
    QString gainRefFile = conf->getValue("import_gainref_original");
    if(QFileInfo(gainRefFile).exists()) {
        conf->set("import_gainref", "../" + QFileInfo(gainRefFile).fileName());
        scriptsToBeExecuted_.append("rsync -auvP " + gainRefFile + " " + workingDir.canonicalPath() + "/../" + QFileInfo(gainRefFile).fileName());
    }
    
    conf->setModified(true);
    
    //Write to status folder if required
    if(conf->getValue("status_folder_update") == "y" && QFileInfo(conf->getValue("status_folder")).isDir()) {
        QFile saveFile(conf->getValue("status_folder") + "/last.txt");
        long lastMSecs = 0;
        if(saveFile.exists()) {
            if (saveFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
                while (!saveFile.atEnd()) {
                    lastMSecs = QString(saveFile.readLine().simplified()).toLong();
                }
		saveFile.close();
            }
            saveFile.remove();
        }
        long currentMSecs = conf->getValue("import_original_time").toLong();
        qDebug() << "Last, current time" << lastMSecs << currentMSecs;
	QString toBeWritten;
        if(currentMSecs >= lastMSecs) toBeWritten = QString::number(currentMSecs);
        else toBeWritten = QString::number(lastMSecs);
	qDebug() << "Writing stamp: " << toBeWritten;
        if (saveFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
            saveFile.write(toBeWritten.toLatin1());
        }
        saveFile.close();
    }
    
    //register that this image was imported
    ImportFolderSettings(QDir(conf->getValue("import_dir"))).addImportedImage(baseName, importGroup_ + "/" + number, hasAveraged, hasAligned, hasRaw);
    
    continueExecution();
}

void AutoImportWindow::continueExecution() {
    if (scriptsToBeExecuted_.isEmpty()) {
        QStringList selectedScripts = selectedScriptPaths();
        if(imageExecuting_ && !selectedScripts.isEmpty()) {
            qDebug() << "Adding to processing queue: " << imageExecuting_->toString();
            projectData.addImageToQueue(imageExecuting_, selectedScripts, priorityQueueOption_->isChecked());
        }
        importImage();
        return;
    }

    QString scriptPath = scriptsToBeExecuted_.first();
    qDebug() << "Import is executing: " << scriptPath;
    scriptsToBeExecuted_.removeFirst();

    process_.start(scriptPath, QIODevice::ReadOnly);
}

QStringList AutoImportWindow::selectedScriptPaths() {
    QStringList scripts;
    for (int row = 0; row < selectedScriptsCont->count(); row++) {
        scripts.append(selectedScriptsCont->item(row)->text());
    }
    return scriptSelectorDialog.scriptPaths(scripts);
}

bool AutoImportWindow::isSafeToCopy(const QString& imageName) {
    if(!QFileInfo(imageName).exists()) return true;
    
    int safe_interval = safeIntervalBox->value()*1000;
    if(QDateTime::currentMSecsSinceEpoch() - QFileInfo(imageName).lastModified().toMSecsSinceEpoch() < safe_interval) {
        return false;
    }
    else return true;
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
