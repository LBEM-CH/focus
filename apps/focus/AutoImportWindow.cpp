#include <QtWidgets>

#include "ApplicationData.h"
#include "ProjectData.h"
#include "UserPreferenceData.h"

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
#include "UserPreferenceData.h"
#include "ParameterMaster.h"

QMutex AutoImportWindow::mutex_;

AutoImportWindow::AutoImportWindow(QWidget* parent)
: QWidget(parent) {
    resultsTable_ = setupFilesTable();
    
    fileNameParser_ = new FileNameParserDialog(this);
    filePatternLabel_ = new QLabel();
    
    safeIntervalBox = new QSpinBox();
    safeIntervalBox->setMinimum(0);
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

    resetImportButton_ = new QPushButton(ApplicationData::icon("resetImport"), tr("Reset Import Status of All Files"));
    connect(resetImportButton_, &QAbstractButton::clicked, this, &AutoImportWindow::resetImport);
    
    priorityQueueOption_ = new QCheckBox("Prioritize imported images, so that newest images are processed first");
    priorityQueueOption_->setChecked(true);
    
    continuous = new QCheckBox("Continuously import new images in the import folder");
    continuous->setChecked(false);
    int mintime = safeIntervalBox->value()*1000;
    if ( mintime < 60000 ) mintime = 60000;
    connect(continuous, &QCheckBox::toggled, [ = ] (bool check){
        if(check) timer_.start(mintime);
        else timer_.stop();
    });
    
    inputContainer_ = setupInputContainer();

    QSplitter* mainSplitter = new QSplitter(Qt::Horizontal);
    mainSplitter->setHandleWidth(4);
    mainSplitter->addWidget(inputContainer_);
    mainSplitter->addWidget(setupStatusContainer());
    
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
    labels << "" <<  tr("Import Folder") << tr("Import File Name") << tr("Data") << tr("XML")<< tr("Target Directory");
    filesTable->setHorizontalHeaderLabels(labels);
    filesTable->horizontalHeader()->setSectionResizeMode(5, QHeaderView::Stretch);
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
    container->setTitle("File Import Setting");

    QFormLayout* layout = new QFormLayout;
    layout->setHorizontalSpacing(10);
    layout->setVerticalSpacing(0);
    layout->setRowWrapPolicy(QFormLayout::DontWrapRows);
    layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    layout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    layout->setLabelAlignment(Qt::AlignLeft);

    QLabel* introLabel = new QLabel(introText());
    introLabel->setWordWrap(true);
    QPalette pal = introLabel->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    introLabel->setPalette(pal);
    layout->addRow(introLabel);

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
    
    EPUCheck = new QCheckBox("Import is from EPU (Expects files in ./*/Data/ from above location)");
    // EPUCheck->setChecked(ProjectPreferences().importEPUCheck());
    EPUCheck->setChecked(UserPreferences().showEPU());
    connect(EPUCheck, &QCheckBox::toggled, [ = ] (bool check){
        if (check) {
            resultsTable_->setColumnHidden(4,false);
            QString importFileType = conf->getValue("import_file_type");
            QString importFileStatus = conf->getValue("import_file_status");
            QString importFileExtension = conf->getValue("import_file_extension");
            if ( importFileExtension != "0") {
                importFileExtension = "0";
                conf->set("import_file_extension",importFileExtension,true);
                qDebug()<<"Resetting import_file_extension to "<<importFileExtension;
            }
            if ( importFileStatus != "1" ) { 
                importFileStatus = "1";
                conf->set("import_file_status",importFileStatus,true);
                qDebug()<<"Resetting import_file_status to "<<importFileStatus;
            }
        } else {
            resultsTable_->setColumnHidden(4,true);
        }
    });
    layout->addRow(EPUCheck);
    container->setContainerLayout(layout);
    return container;
}

QWidget* AutoImportWindow::setupOptionsContainter() {

    //Get the list of parameters to be displayed

    //The following section is only important, if EPU option is not active....   ToDo.

    QFile s(ApplicationData::configDir().canonicalPath() + "/import_nonEPU.params.list");
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Import parameters read failed: " << ApplicationData::configDir().canonicalPath() + "/import_nonEPU.params.list";
        return new QWidget();
    }
    QStringList paramsList;
    while (!s.atEnd()) paramsList << s.readLine().simplified();
    s.close();
    
    QFile s1(ApplicationData::configDir().canonicalPath() + "/import.params.list");
    if (!s1.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Import parameters read failed: " << ApplicationData::configDir().canonicalPath() + "/import.params.list";
        return new QWidget();
    }
    while (!s1.atEnd()) paramsList << s1.readLine().simplified();
    s1.close();
    
    if (projectData.projectMode().toInt() == 4) {
    	//For Multi Exposures: Get the list of additional parameters to be displayed
    	QFile s2(ApplicationData::configDir().canonicalPath() + "/import_multi.params.list");
    	if (!s2.open(QIODevice::ReadOnly | QIODevice::Text)) {
    		qDebug() << "Import Multi parameters read failed: " << ApplicationData::configDir().canonicalPath() + "/import_multi.params.list";
    		return new QWidget();
    	}
    	while (!s2.atEnd()) paramsList << s2.readLine().simplified();
    	s2.close();
    }
    
    //Setup the window and add widgets
    ParametersWidget* parameterContainer = new ParametersWidget(projectData.projectParameterData(), paramsList, 2);
    parameterContainer->setFrameStyle(QFrame::NoFrame);
    parameterContainer->setFixedHeight(390);

    return parameterContainer;
}

QWidget* AutoImportWindow::setupScriptsContainer() {
    GroupContainer* scriptsContainer = new GroupContainer;
    scriptsContainer->setTitle("Process imported images");
    
    selectedScriptsCont = new QListWidget;
    selectedScriptsCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
    resetSelectedScriptsContainer();

    QHBoxLayout *buttonsLayout = new QHBoxLayout;
    buttonsLayout->setMargin(0);
    buttonsLayout->setSpacing(4);
    buttonsLayout->addStretch(0);
    buttonsLayout->addWidget(new QLabel("Select scripts: "));
    
    if(importSelectorDialog.hasAvailableScripts()) {
        QPushButton* changeDuringButton = new QPushButton("to run WHILE importing");
        changeDuringButton->setToolTip("Change scripts that are processed while importing (sequentially)");
        connect(changeDuringButton, &QPushButton::clicked, [=]{
            if(importSelectorDialog.exec()) {
                resetSelectedScriptsContainer();
            }
        });
        buttonsLayout->addWidget(changeDuringButton, 0);
    }
    
    QPushButton* changeButton = new QPushButton("to run AFTER import (in job queue)");
    changeButton->setToolTip("Change scripts that are processed after importing (in parallel via batch-queue processor)");
    connect(changeButton, &QPushButton::clicked, [=]{
        if(scriptSelectorDialog.exec()) {
            resetSelectedScriptsContainer();
        }
    });
    buttonsLayout->addWidget(changeButton, 0);
    buttonsLayout->addStretch(1);

    QVBoxLayout* scriptContLayout = new QVBoxLayout();
    scriptContLayout->addLayout(buttonsLayout, 0);
    scriptContLayout->addWidget(priorityQueueOption_, 0);
    scriptContLayout->addWidget(new BlockContainer("Scripts to be executed (Blue: While Importing, Black: After Import)", selectedScriptsCont));
    scriptsContainer->setContainerLayout(scriptContLayout);
    return scriptsContainer;
}

QWidget* AutoImportWindow::setupStatusContainer() {

    safeIntervalBox->setValue(ProjectPreferences().importSafeInterval());
    connect(safeIntervalBox, static_cast<void(QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=] (int i){
        ProjectPreferences().setImportSafeInterval(i);
        
    });
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch(0);
    buttonLayout->addWidget(importButton_, 0);
    buttonLayout->addWidget(refreshButton_, 0);
    buttonLayout->addWidget(resetImportButton_, 0);
    buttonLayout->addStretch(1);
    
    QVBoxLayout* statusLayout = new QVBoxLayout;
    statusLayout->addWidget(statusLabel_, 0);
    statusLayout->addLayout(buttonLayout, 0);
    statusLayout->addWidget(deleteLabel_, 0);
    
    GroupContainer* statusContainer = new GroupContainer;
    statusContainer->setTitle("Current Status");
    statusContainer->setContainerLayout(statusLayout);
    
    filePatternLabel_->setText(FileNameParserDialog::expectedFileNamePattern());
    filePatternLabel_->setWordWrap(true);
    
    QPushButton* changeButton = new QPushButton("Change Pattern");
    connect(changeButton, &QPushButton::clicked, [=]{
        if(fileNameParser_->exec()) {
            filePatternLabel_->setText(FileNameParserDialog::expectedFileNamePattern());
        }
    }); 
    
    QHBoxLayout* fileNameParserLayout = new QHBoxLayout();
    fileNameParserLayout->addStretch(0);
    fileNameParserLayout->addWidget(filePatternLabel_, 0);
    fileNameParserLayout->addWidget(changeButton, 0);
    fileNameParserLayout->addStretch(1);
    
    QHBoxLayout* timerLayout = new QHBoxLayout();
    timerLayout->addStretch(0);
    timerLayout->addWidget(new QLabel("Number of seconds to wait before starting import of fresh (newly created) images"), 0);
    timerLayout->addWidget(safeIntervalBox, 0);
    timerLayout->addStretch(1);
    
    QFormLayout* optionsLayout = new QFormLayout();
    optionsLayout->setRowWrapPolicy(QFormLayout::WrapAllRows);
    
    optionsLayout->addRow("Parameters to be deduced from file names: ", fileNameParserLayout);
    optionsLayout->addRow("Import delay time: ", timerLayout);
    
    //Make the labels bold
    for(int i=0; i< optionsLayout->rowCount(); ++i) {
        QLabel* label = static_cast<QLabel*>(optionsLayout->itemAt(i, QFormLayout::LabelRole)->widget());
        QFont font = label->font();
        font.setBold(true);
        label->setFont(font);
    }
    
    GroupContainer* optionsContainer = new GroupContainer;
    optionsContainer->setTitle("Import Options");
    optionsContainer->setContainerLayout(optionsLayout);
    
    QHBoxLayout* resultsLayout = new QHBoxLayout();
    resultsLayout->addWidget(resultsTable_, 1);
    
    GroupContainer* resultsContainer = new GroupContainer;
    resultsContainer->setTitle("Import Folder Details");
    resultsContainer->setContainerLayout(resultsLayout);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(statusContainer, 0);
    mainLayout->addWidget(optionsContainer, 0);
    mainLayout->addWidget(resultsContainer, 1);
    
    QWidget* mainWid = new QWidget();
    mainWid->setLayout(mainLayout);

    return mainWid;
}

void AutoImportWindow::resetImport(bool force) {
    
    if(currentlyExecuting_ && !force) {
        // qDebug()<< "The import is already running, not doing anything for now!";
        return;
    }
    if(QMessageBox::question(this,
    tr("Move to trash?"),"Are you sure that you want to reset all images to unimported? \n\n Proceed?",
    tr("Yes"),
    tr("No"),
    QString(),0,1) == 0){
        qDebug() << "Resetting import status on selected images.";  
        QFile importFile(projectData.projectDir().canonicalPath() + "/merge/config/project.imported.ini");
        if(importFile.exists()) {
            importFile.remove();
            analyzeImport();
        }
    }
}

void AutoImportWindow::analyzeImport(bool force) {
    
    if(currentlyExecuting_ && !force) {
        // qDebug()<< "The import is already running, not analyzing the import folder for now!";
        return;
    }
    
    QMutexLocker locker(&AutoImportWindow::mutex_);
    
    dirToRowNumber_.clear();
    rowToImagePaths_.clear();
    toBeImported_.clear();
    resultsTable_->setRowCount(0);

    ParametersConfiguration* conf  = projectData.projectParameterData();
    QString importImagesPath = conf->getValue("import_dir");
    QString importFileType = conf->getValue("import_file_type");
    QString importFileExtension = conf->getValue("import_file_extension");
    QString importFileStatus = conf->getValue("import_file_status");
    QString importGroup = conf->getValue("import_target_group");
    QStringList ignoreImagePattern = conf->getValue("import_ignore_strings").split(' ');
    int imageNumberLength = conf->getVariant("import_numberlength").toInt();

    if (importImagesPath.isEmpty() || !QFileInfo(importImagesPath).exists()) {
        statusLabel_->setText("The import path does not exist (Please change on left)");
        return;
    }

    int uid = projectData.projectParameterData()->getVariant("import_imagenumber").toInt();

    if(EPUCheck->isChecked()) {
        if (importFileExtension != "0") {
            importFileExtension = "0";
            conf->set("import_file_extension",importFileExtension,true);
            qDebug()<<"Resetting import_file_extension to "<<importFileExtension;
        }
        if (importFileStatus != "1") { 
            importFileStatus = "1";
            conf->set("import_file_status",importFileStatus,true);
            qDebug()<<"Resetting import_file_status to "<<importFileStatus;
        }
        resultsTable_->setColumnHidden(4,false);
    } else {
        resultsTable_->setColumnHidden(4,true);
    }    
    
    bool addingAFile = false;
   
    QDir importDir(importImagesPath);
    ImportFolderSettings folderPreferences(importDir);
    QStringList alreadyImportedBaseNames;
    if(EPUCheck->isChecked()) {
        QStringList locAlreadyImportedBaseNames = folderPreferences.importedFullNames();
        for (QString tmp3 : locAlreadyImportedBaseNames) {
            int pos = tmp3.lastIndexOf(QChar('/'));
            QString tmp4 = tmp3.left(pos);
            // qDebug()<<"tmp3 = "<<tmp3<<"      tmp4 = "<<tmp4;
            alreadyImportedBaseNames.append(tmp4);
        }
    }
    else {
        alreadyImportedBaseNames = folderPreferences.importedNames();
    }
    
    QStringList EPUExtentions;
    EPUExtentions << "*.mrc" << "*.mrcs";

    QStringList mrcExtentions;
    mrcExtentions << "*.mrc";

    QStringList mrcsExtentions;
    mrcsExtentions << "*.mrcs";

    QStringList tifExtentions;
    tifExtentions << "*.tif" << "*.tiff";

    QStringList XMLExtentions;
    XMLExtentions << "*.xml";
    
    //Get a list of all available directories
    QStringList DirNames = QDir(importImagesPath + "/").entryList(QDir::AllDirs | QDir::NoDotAndDotDot | QDir::NoSymLinks);
    DirNames.removeDuplicates();
 
    //Get a list of all available files
    QStringList fileNames;
    if(EPUCheck->isChecked()){
        for (QString direc: DirNames) {
            QStringList LocNames = QDir(importImagesPath + "/" + direc + "/Data/").entryList(EPUExtentions, QDir::Files | QDir::NoSymLinks);
            LocNames.replaceInStrings(QRegularExpression("^"),direc+"/Data/");
            fileNames.append(LocNames);
            LocNames = QDir(importImagesPath + "/" + direc + "/Data/").entryList(XMLExtentions, QDir::Files | QDir::NoSymLinks);
            LocNames.replaceInStrings(QRegularExpression("^"),direc+"/Data/");
            fileNames.append(LocNames);
        }
    }
    else {
        // fileNames.append(QDir(importImagesPath + "/" + importAveragedFolder).entryList(avgExtentions, QDir::Files | QDir::NoSymLinks));
        // fileNames.append(QDir(importImagesPath + "/" + importAlignedFolder).entryList(stackExtentions, QDir::Files | QDir::NoSymLinks));
        // fileNames.append(QDir(importImagesPath + "/" + importRawFolder).entryList(stackExtentions, QDir::Files | QDir::NoSymLinks));
        if      (importFileExtension == "0") fileNames.append(QDir(importImagesPath).entryList(mrcExtentions,  QDir::Files | QDir::NoSymLinks));
        else if (importFileExtension == "1") fileNames.append(QDir(importImagesPath).entryList(mrcsExtentions, QDir::Files | QDir::NoSymLinks));
        else if (importFileExtension == "2") fileNames.append(QDir(importImagesPath).entryList(tifExtentions,  QDir::Files | QDir::NoSymLinks));
        else qDebug()<<"ERROR: importFileExtension has non-valid entry: "<<importFileExtension;
    }
    fileNames.removeDuplicates();
    // qDebug()<<"fileNames = "<<fileNames;
    
    QStringList baseNames;
    for(QString image: fileNames) {
        //get the basename and remove the to_be_ignored strings
        QString baseName = QFileInfo(image).completeBaseName();
        if (!ignoreImagePattern.isEmpty()) {
            for (QString pattern : ignoreImagePattern) {
                if (!pattern.trimmed().isEmpty()) baseName.remove(pattern.trimmed(), Qt::CaseInsensitive);
            }
        }
        baseName.prepend("/");
        baseName.prepend(QFileInfo(image).path());
        baseNames.append(baseName);
        // qDebug() << "Image="<<image<<" ,  baseName="<<baseName;
    }
    baseNames.removeDuplicates();
    
    for (QString baseName : baseNames) {
        bool copying = false;
        bool processed = false;
        bool hasImage = false;
        bool hasXML = false;
        bool hasSkipImage = false;
        QString dirName;
        QString EPU_baseName;
        
        if(toBeImported_.keys().contains(baseName)) {
            continue;
        }

        // qDebug()<<"alreadyImportedBaseNames = " << alreadyImportedBaseNames;
        // qDebug()<<"baseName = "<<baseName;
        // qDebug()<<"QFileInfo(baseName).fileName() = "<<QFileInfo(baseName).fileName();
        // QString tmp2 = projectData.projectDir().canonicalPath();
        // qDebug()<<"projectData.projectDir().canonicalPath() = "<<tmp2;
        // qDebug()<<"folderPreferences.linkedDirectory(baseName) = "<<folderPreferences.linkedDirectory(baseName)<<endl;

        if(EPUCheck->isChecked()) {
            EPU_baseName = baseName;
        }
        else {
            EPU_baseName = QFileInfo(baseName).fileName();
        }
        // qDebug()<<"EPU_baseName = "<<EPU_baseName;

        //////////////////////////////////////////////////////////////////////////////////////////////////////
        //
        // Here, baseName has the following contents:
        // for EPU files:   Grid_00001/Data/Filename
        // for others   :   ./Filename
        //
        // To access the Filename alone, one needs to use QFileInfo(baseName).fileName().
        //
        // Here, EPU_baseName has the following contents:
        // for EPU files:   Grid_00001/Data/Filename
        // for others   :   Filename
        //
        //////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        if(alreadyImportedBaseNames.contains(EPU_baseName)) {
            if ( QFileInfo(projectData.projectDir().canonicalPath() + '/' + folderPreferences.linkedDirectory(EPU_baseName) + "/2dx_image.cfg").exists()) {
                processed = true;
                dirName = folderPreferences.linkedDirectory(EPU_baseName);
                hasImage = folderPreferences.hadImage(EPU_baseName);
                hasXML = folderPreferences.hadXML(EPU_baseName);
                hasSkipImage = folderPreferences.hadSkipImage(EPU_baseName);
                // qDebug()<<"CHECK 1: alreadyImportedBaseNames= "<<alreadyImportedBaseNames;
            } else {
                processed = false;
                dirName = folderPreferences.linkedDirectory(EPU_baseName);
                hasImage = folderPreferences.hadImage(EPU_baseName);
                hasXML = folderPreferences.hadXML(EPU_baseName);
                hasSkipImage = folderPreferences.hadSkipImage(EPU_baseName);
            }  
        }
        else {
            // 07.09.2018 RDR Changed ++uid to uid++ below to start the imported directories count at 1 instead of 2:
            QString imageNumber = ProjectData::commitIntToStringLength(uid++, imageNumberLength);

            while (QDir(projectData.projectDir().canonicalPath() + "/" + importGroup + "/" + imageNumber).exists()) {
                imageNumber = ProjectData::commitIntToStringLength(uid++, imageNumberLength);
            }
            
            dirToRowNumber_.insert(imageNumber, resultsTable_->rowCount());
            dirName = importGroup + "/" + imageNumber;
 
            QString locBaseName = QFileInfo(baseName).fileName();
            if(EPUCheck->isChecked()) {
                locBaseName.prepend("/");
                locBaseName.prepend(QFileInfo(baseName).path());
            }
            // qDebug()<<"locBaseName= "<<locBaseName;
            toBeImported_.insert(imageNumber, QStringList() << locBaseName);
            
            //Search string for avg File
            QStringList imageSearchStrings, EPUSearchStrings, XMLSearchStrings;
            
            if(EPUCheck->isChecked()){
                for(QString ext : EPUExtentions) EPUSearchStrings.append(QFileInfo(baseName).fileName() + ext);
                for(QString ext : XMLExtentions) XMLSearchStrings.append(QFileInfo(baseName).fileName() + ext);
            }
            else {
                if      (importFileExtension == "0") for(QString ext : mrcExtentions)  imageSearchStrings.append(QFileInfo(baseName).fileName() + ext);
                else if (importFileExtension == "1") for(QString ext : mrcsExtentions) imageSearchStrings.append(QFileInfo(baseName).fileName() + ext);
                else if (importFileExtension == "2") for(QString ext : tifExtentions)  imageSearchStrings.append(QFileInfo(baseName).fileName() + ext);
            }
                
            QString locImportImagesPath = importImagesPath;
            if(EPUCheck->isChecked()){
                locImportImagesPath.append("/");
                locImportImagesPath.append(QFileInfo(baseName).path());
            }
            // qDebug()<<" locImportImagesPath = "<<locImportImagesPath;
            
            
            
            //07.09.2018 RDR below the EPU images were being treated like "normal import" previously, which resulted in always the same movie being imported. It should actually be handled separately (EPUSearchStrings vs imageSearchStrings), as it is now:

            if(EPUCheck->isChecked()){
                //Check for EPU image file
                QString EPUFile;
                if (QDir(locImportImagesPath).exists()) {
                    QStringList possibleFiles = QDir(locImportImagesPath).entryList(EPUSearchStrings, QDir::Files | QDir::NoSymLinks);
                    if (!possibleFiles.isEmpty()) {
                        hasImage = true;
                        // qDebug()<<"possibleFiles= "<<possibleFiles;
                        EPUFile = locImportImagesPath + "/" + possibleFiles.first();
                    }
                }
                // qDebug()<<"EPUFile "<<EPUFile;
                toBeImported_[imageNumber].append(EPUFile);

                //Check for XML file
                QString XMLFile;
                if (QDir(locImportImagesPath).exists()) {
                    QStringList possibleFiles = QDir(locImportImagesPath).entryList(XMLSearchStrings, QDir::Files | QDir::NoSymLinks);
                    if (!possibleFiles.isEmpty()) {
                        hasXML = true;
                        XMLFile = locImportImagesPath + "/" + possibleFiles.first();
                    }
                }
                // qDebug()<<"XMLFile "<<XMLFile;
                toBeImported_[imageNumber].append(XMLFile);
            }
            else {
                // 07.09.2018 RDR here is the "normal import", that should NOT be used for EPU data:
                //Check for image file
                QString imageFile;
                if (QDir(locImportImagesPath).exists()) {
                    QStringList possibleFiles = QDir(locImportImagesPath).entryList(imageSearchStrings, QDir::Files | QDir::NoSymLinks);
                    if (!possibleFiles.isEmpty()) {
                        hasImage = true;
                        // qDebug()<<"possibleFiles= "<<possibleFiles;
                        imageFile = locImportImagesPath + "/" + possibleFiles.first();
                    }
                }
                // qDebug()<<"imageFile= "<<imageFile;
                toBeImported_[imageNumber].append(imageFile);
            }
            
            //Check if the file is still being copied
            // CHEN:ToDo  Check, if the file is a multiple.
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
        else if (!processed) {
            if (hasSkipImage) statusItem->setIcon(ApplicationData::icon("process_skip"));
            else statusItem->setIcon(ApplicationData::icon("process_wait"));
        }
        else statusItem->setIcon(ApplicationData::icon("process_done"));
        
        QString tmp = QFileInfo(baseName).path();
        // if(EPUCheck->isChecked()) tmp.remove("/Data");
        QTableWidgetItem* folderItem = new QTableWidgetItem(tmp);
        folderItem->setFlags(folderItem->flags() ^ Qt::ItemIsEditable);
        folderItem->setTextAlignment(Qt::AlignCenter);
        
        QTableWidgetItem* imageItem = new QTableWidgetItem(QFileInfo(baseName).fileName());
        imageItem->setFlags(imageItem->flags() ^ Qt::ItemIsEditable);
        imageItem->setTextAlignment(Qt::AlignCenter);
                
        QTableWidgetItem *rawItem = new QTableWidgetItem();
        rawItem->setFlags(rawItem->flags() ^ Qt::ItemIsEditable);
        if (hasImage) rawItem->setIcon(ApplicationData::icon("tick"));
        else rawItem->setIcon(ApplicationData::icon("cross"));

        QTableWidgetItem *XMLItem = new QTableWidgetItem();
        XMLItem->setFlags(XMLItem->flags() ^ Qt::ItemIsEditable);
        if (hasXML) XMLItem->setIcon(ApplicationData::icon("tick"));
        else XMLItem->setIcon(ApplicationData::icon("cross"));
        
        QTableWidgetItem* numberItem = new QTableWidgetItem(dirName);
        numberItem->setFlags(numberItem->flags() ^ Qt::ItemIsEditable);
        numberItem->setTextAlignment(Qt::AlignCenter);

        int row = resultsTable_->rowCount();
        resultsTable_->insertRow(row);
        resultsTable_->setItem(row, 0, statusItem);
        resultsTable_->setItem(row, 1, folderItem);
        resultsTable_->setItem(row, 2, imageItem);
        resultsTable_->setItem(row, 3, rawItem);
        resultsTable_->setItem(row, 4, XMLItem);
        resultsTable_->setItem(row, 5, numberItem);
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
    whileImportScriptCount_ = 0;
    QStringList importScripts = ProjectPreferences().scripts("import");
    for(QString script : importScripts) {
        QListWidgetItem* item = new QListWidgetItem(script);
        item->setTextColor(Qt::blue);
        selectedScriptsCont->addItem(item);
        whileImportScriptCount_++;
    } 
    
    QStringList selectedScripts = ProjectPreferences().scripts("process");
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
        inputContainer_->setEnabled(true);
        refreshButton_->setEnabled(true);
        scriptsToBeExecuted_.clear();
        imageExecuting_ = 0;
        commandExecuting_ = "";
    } else {
        importButton_->setChecked(true);
        importButton_->setText("Stop Import");
        inputContainer_->setDisabled(true);
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
        // qDebug() << "Currently importing, skipping this import";
        return;
    } else if(toBeImported_.isEmpty()) {
        resetState();
        return;
    } else {
        currentlyExecuting_ = true;
        resetState();

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
                        resultsTable_->item(dirToRowNumber_[numberExec], 0)->setIcon(ApplicationData::icon("process_done"));
                    }
                }
            }
        }
    }
    qDebug();
    
    QString locScript;

    QString number;
    number = toBeImported_.keys().first();
    // qDebug()<<" toBeImported_="<<toBeImported_;

    QStringList files = toBeImported_[number];
    toBeImported_.remove(number);
    
    //Get the original file name used for search
    QString baseName = files.first();

    
    //Deduce parameters from baseName
    QMap<QString, QString> fileNameParams = FileNameParserDialog::parseFileName(QFileInfo(baseName).fileName());
    
    QString importGroup_ = projectData.projectParameterData()->getValue("import_target_group");
    QString importGroupSuffix = projectData.projectParameterData()->getValue("import_target_group_suffix");
    
    //Add suffix to group name
    QString suffix = "";
    
    //case importGroupSuffix=1 => parameter = specimennumber
    if(importGroupSuffix == "1") { 
        //If the file name contains this param take it from there, otherwise search master.cfg 
        if(fileNameParams.contains("specimennumber")) suffix = fileNameParams["specimennumber"];
        else {
            suffix = projectData.projectParameterData()->getValue("specimennumber");
        }
    }

    //case importGroupSuffix=2 => parameter = tomo_series_number
    if(importGroupSuffix == "2") { 
        //If the file name contains this param take it from there, otherwise search master.cfg 
        if(fileNameParams.contains("tomo_series_number")) suffix = fileNameParams["tomo_series_number"];
        else {
            suffix = projectData.projectParameterData()->getValue("tomo_series_number");
        }
    }
    
    bool hasImage = false;
    bool hasSkipImage = false;
    bool hasXML = false;

    int import_multi_last_number = projectData.projectParameterData()->getValue("import_multi_last_number").toInt();
    int import_multi_first_number = projectData.projectParameterData()->getValue("import_multi_first_number").toInt();
    QString nextFile;
    int currentNumber = -999;
    bool importThisOne = true;
    if (projectData.projectMode().toInt() == 4) {
        if(fileNameParams.isEmpty()) {
            QMessageBox msgBox;
            msgBox.setText("Error: First define parameters to be deduced from file names.");
            msgBox.exec();
            finishExecution();
            return;
        } else {
            if (files.size() > 1 && !files[1].isEmpty()) {
                if(fileNameParams.contains("multi_series_number")) {
                    currentNumber = fileNameParams["multi_series_number"].toInt();
                }
                else {
                    currentNumber = -999;
                }
                if (currentNumber != import_multi_last_number) {
                    importThisOne = false; 
                    hasSkipImage = true;
                }
            }
        }
    }
        
    if(suffix == "-") suffix = "";
    
    importGroup_ = importGroup_ + suffix;
    
    if (dirToRowNumber_.keys().contains(number)) {
        if (dirToRowNumber_[number] < resultsTable_->rowCount()) {
            if(resultsTable_->item(dirToRowNumber_[number], 0)) {
                if(hasSkipImage) {
                    resultsTable_->item(dirToRowNumber_[number], 0)->setIcon(ApplicationData::icon("process_skip"));
                } else {
                    resultsTable_->item(dirToRowNumber_[number], 0)->setIcon(ApplicationData::icon("process_executing"));
                }
                resultsTable_->scrollToItem(resultsTable_->item(dirToRowNumber_[number], 0));
            }
        }
    }
    
    statusLabel_->setText(QString("Currently importing and %2 are in queue...").arg(toBeImported_.keys().size()));
    projectData.projectParameterData()->set("import_imagenumber", number);
    
    //Create dir
    QDir workingDir = QDir(projectData.projectDir().canonicalPath() + "/" + importGroup_ + "/" + number);
    
    ParametersConfiguration* conf;
    QString importFileType;
    QString importFileExtension;
    QString importFileStatus;
    
    if (importThisOne) {
        //create import group
        projectData.projectDir().mkpath(importGroup_);
        QFile(projectData.projectDir().absolutePath() + "/merge").link("../2dx_master.cfg", projectData.projectDir().absolutePath() + "/" + importGroup_ + "/2dx_master.cfg");
    
        //create Dir
        projectData.projectDir().mkpath(importGroup_ + "/" + number);
        workingDir.mkpath("proc");
        workingDir.mkpath("LOGS");

        //Copy Config File
        projectData.projectParameterData()->saveAs(workingDir.canonicalPath() + "/2dx_image.cfg", true);


        imageExecuting_ = projectData.addImage(importGroup_, number);
        conf = imageExecuting_->parameters();
        
        conf->set("imagenumber", number, false);
        importFileType = conf->getValue("import_file_type");
        importFileExtension = conf->getValue("import_file_extension");
        importFileStatus = conf->getValue("import_file_status");
        conf->set("import_rawstack_type", importFileType, false);
    }

    if(EPUCheck->isChecked()){
        //Check for EPU raw file
        // qDebug()<<" baseName="<<baseName<<"    files="<<files;
        if (importThisOne) conf->set("EPU_dirname", QFileInfo(baseName).path().remove("/Data"), false);
        if (files.size() > 1 && !files[1].isEmpty()) {
            if (importThisOne) {
                conf->set("import_rawstack", QFileInfo(baseName).baseName() + '.' + QFileInfo(files[1]).suffix(), false);
                conf->set("import_rawstack_original", baseName + '.' + QFileInfo(files[1]).suffix(), false);
                conf->set("raw_gaincorrectedstack", "raw_gaincorrectedstack", false);
                conf->set("raw_gaincorrectedstack_original", baseName + '.' + QFileInfo(files[1]).suffix(), false);
                conf->set("import_original_time", QString::number(QFileInfo(files[1]).created().toMSecsSinceEpoch()), false);
                scriptsToBeExecuted_.append("cp -f " + files[1] + " " + workingDir.canonicalPath() + "/" + "raw_gaincorrectedstack" + ".mrcs");
                if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[1]);
            }
            hasImage = true;
        }

        //Check for EPU XML file 
        if (files.size() > 2 && !files[2].isEmpty()) {
            if (importThisOne) {
                scriptsToBeExecuted_.append("cp -f " + files[2] + " " + workingDir.canonicalPath() + "/" + QFileInfo(files[2]).baseName() + ".xml");
                if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[2]);
            }
            hasXML = true;
        }
    }
    else {

        //Check for the 2D file
        if(importFileType == "0") {
            if(files.size() > 1 && !files[1].isEmpty()) {
                if(importFileStatus == "0") {
                    // qDebug() << "ERROR: This option is not yet implemented: dark-subtracted 2D images alone.";
                    // CHEN:ToDo : In this case, set importFileStatus to "1".                    
                } else if (importFileStatus == "1") {
                    if (importThisOne) {
                        conf->set("imagename", "image_2dx", false);
                        conf->set("nonmaskimagename", "image_2dx", false);
                        conf->set("imagename_original", files[1], false);
                        conf->set("import_original_time", QString::number(QFileInfo(files[1]).created().toMSecsSinceEpoch()), false);
                        scriptsToBeExecuted_.append("cp -f " + files[1] + " " + workingDir.canonicalPath() + "/" + "image_2dx." + QFileInfo(files[1]).suffix());
                        if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[1]);
                    }
                    hasImage = true;
                }
            }
        }
    
        //Check for raw stack file
        if(importFileType == "1") {
            // qDebug()<<" baseName="<<baseName<<"    files="<<files;
            if (projectData.projectMode().toInt() != 4) {
                if (files.size() > 1 && !files[1].isEmpty()) {
                    if(importFileStatus == "0") {
                        if (importThisOne) {
                            conf->set("import_rawstack", baseName + '.' + QFileInfo(files[1]).suffix(), false);
                            conf->set("import_rawstack_original", files[1], false);
                            conf->set("import_original_time", QString::number(QFileInfo(files[1]).created().toMSecsSinceEpoch()), false);
                            scriptsToBeExecuted_.append("cp -f " + files[1] + " " + workingDir.canonicalPath() + "/" + baseName + '.' + QFileInfo(files[1]).suffix());
                            if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[1]);
                        }
                        hasImage = true;
                    } else if (importFileStatus == "1") {
                        if (importThisOne) {
                            conf->set("import_rawstack", baseName + '.' + QFileInfo(files[1]).suffix(), false);
                            conf->set("import_rawstack_original", files[1], false);
                            conf->set("raw_gaincorrectedstack", "raw_gaincorrectedstack", false);
                            conf->set("raw_gaincorrectedstack_original", files[1], false);
                            conf->set("import_original_time", QString::number(QFileInfo(files[1]).created().toMSecsSinceEpoch()), false);
                            scriptsToBeExecuted_.append("cp -f " + files[1] + " " + workingDir.canonicalPath() + "/" + "raw_gaincorrectedstack" + '.' + QFileInfo(files[1]).suffix());
                            if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[1]);
                        }
                        hasImage = true;
                    }
                }
            }
            else {
                //For Multi Exposures: Read this and also the next file(s)
                if(importFileStatus == "1") {
                    // qDebug() << "ERROR: This option is not yet implemented: gain-corrected multi-stacks.";
                    // CHEN:ToDo : In this case, set importFileStatus to "1".                    
                }
                if (files.size() > 1 && !files[1].isEmpty()) {
                    if (importThisOne) {
                        bool keepGoing = true;
                        conf->set("import_rawstack", baseName + "." + QFileInfo(files[1]).suffix(), false);
                        conf->set("import_rawstack_original", files[1], false);
                        conf->set("import_original_time", QString::number(QFileInfo(files[1]).created().toMSecsSinceEpoch()), false);
                        int nextNumber = currentNumber;
                        while (keepGoing) {
                            QString nextnum = QString::number(nextNumber).rightJustified(4, '0');
                            QStringList selectedParams = ProjectPreferences().importFileParams();
                            QString sep = ProjectPreferences().importFileSeparator();
                            if(sep.isEmpty() || selectedParams.isEmpty()) {
                                qDebug()<<"ERROR: Parameters for file name pattern are not defined.";
                            } else {
                                int ifound = 0;
                                for(int i=1; i< selectedParams.size(); ++i){
                                    if ("multi_series_number" == selectedParams[i]) {
                                        ifound = i;
                                        break;
                                    }
                                }
                                if (ifound == 0) {
                                    qDebug()<<"ERROR: Parameter multi_series_number not found.";
                                }
                                else {
                                    QStringList list1 = files[1].split(sep);
                                    list1[ifound]=nextnum;
                                    nextFile = list1[0];
                                    for (int i=1;i<=ifound;i++) {
                                        nextFile.append(sep);
                                        nextFile.append(list1[i]);
                                    }
                                    nextFile.append("*." + QFileInfo(files[1]).suffix());
                                    QStringList possibleNextF;
                                    possibleNextF.append(QFileInfo(nextFile).fileName());
                                    QString possibleNextD = QFileInfo(nextFile).path();
                                    QStringList possibleNextFiles = QDir(possibleNextD).entryList(possibleNextF, QDir::Files | QDir::NoSymLinks);
                                    if (!possibleNextFiles.isEmpty()) {
                                        // qDebug()<<"Importing file number "<<nextNumber<<" as "<<possibleNextFiles.first();
                                        int iloc = QString(nextnum).toInt();
                                        QString scriptJob = "cp -f " + possibleNextD + "/" + possibleNextFiles.first() + " " + workingDir.canonicalPath() + "/" + baseName + '_' + QString::number(iloc) + '.' + QFileInfo(files[1]).suffix();
                                        // qDebug()<<"Importing: "<<scriptJob;
                                        scriptsToBeExecuted_.append(scriptJob);
                                        if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + possibleNextD + "/" + possibleNextFiles.first());
                                        hasImage = true;
                                        nextNumber -= 1;
                                        if (nextNumber < import_multi_first_number) {
                                            keepGoing = false;
                                            // qDebug()<<"Multi-Files imported."<<endl;
                                        }
                                    }
                                    else {
                                        qDebug()<<"WARNING: Searching in "<<possibleNextD<<" for file "<<possibleNextF<<" without success."<<endl;
                                        keepGoing = false;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        //Check for drift-corrected stack file
        if(importFileType == "2") {
            if (files.size() > 1 && !files[1].isEmpty()) {
                if(importFileStatus == "0") {
                    // qDebug() << "ERROR: This option is not yet implemented: dark-subtracted gain-corrected stacks.";
                    // CHEN:ToDo : In this case, set importFileStatus to "1".                    
                } else if(importFileStatus == "1") {
                    if (importThisOne) {
                        conf->set("import_rawstack", baseName + '.' + QFileInfo(files[1]).suffix(), false);
                        conf->set("import_rawstack_original", files[1], false);
                        conf->set("import_original_time", QString::number(QFileInfo(files[1]).created().toMSecsSinceEpoch()), false);
                        conf->set("raw_gaincorrectedstack", baseName, false);
                        conf->set("raw_gaincorrectedstack_original", files[1], false);
                        conf->set("movie_stackname", "movie_aligned", false);
                        conf->set("movie_stackname_original", files[2], false);
                        conf->set("import_original_time", QString::number(QFileInfo(files[2]).created().toMSecsSinceEpoch()), false);
                        scriptsToBeExecuted_.append("cp -f " + files[2] + " " + workingDir.canonicalPath() + "/" + "movie_aligned" + '.' + QFileInfo(files[1]).suffix());
                        if(deleteCheck->isChecked()) scriptsToBeExecuted_.append("rm -f " + files[2]);
                    }
                    hasImage = true;
                }
            }
        }   
    }
    
    if (importThisOne) {
        //Check for defects list file
        QString defectsFile = conf->getValue("import_defects_original");
        if(QFileInfo(defectsFile).exists()) {
            conf->set("import_defects", "../" + QFileInfo(defectsFile).fileName(), false);
            locScript = "rsync -auvP " + defectsFile + " " + workingDir.canonicalPath() + "/../" + QFileInfo(defectsFile).fileName();
            // qDebug()<<"Adding to execution queue: "<<locScript;
            scriptsToBeExecuted_.append(locScript);
        }
    
        //Check for gain reference file
        QString gainRefFile = conf->getValue("import_gainref_original");
        if(QFileInfo(gainRefFile).exists()) {
            conf->set("import_gainref", "../" + QFileInfo(gainRefFile).fileName(), false);
            locScript = "rsync -auvP " + gainRefFile + " " + workingDir.canonicalPath() + "/../" + QFileInfo(gainRefFile).fileName();
            // qDebug()<<"Adding to execution queue: "<<locScript;
            scriptsToBeExecuted_.append(locScript);
        }
    
        //Reset the initialization script for 2D crystals
        conf->set("initialization_executable", "y", false);
        conf->set("initialization_reset", "y", false);

        //Reset some other parameters
        conf->set("comment", "-", false);
        conf->set("QVAL", "-", false);
        conf->set("QVAL2", "-", false);
        conf->set("QVALMA", "-", false);
        conf->set("QVALMB", "-", false);
        conf->set("image_flag", "none", false);
        conf->set("import_drift", "-", false);
        conf->set("TLTAXIS", "-", false);
        conf->set("TLTANG", "-", false);
        conf->set("TAXA", "-", false);
        conf->set("TANGL", "-", false);
        conf->set("defocus", "0.0,0.0,0.0", false);
        conf->set("defocus_defocus", "0.0,0.0,0.0", false);

        //Set the parameters from filename
        for(QString param : fileNameParams.keys()) {
            if(parameterMaster.containsParameter(param)) {
                conf->set(param, fileNameParams[param], false);
                //qDebug() << "Parameter set: " << param << " => " << fileNameParams[param];
            }
        }
    
        conf->setModified(true);
    
        //Write to status folder if required
        if(userPreferenceData.get("status_folder_update") == "y" && QFileInfo(userPreferenceData.get("status_folder")).isDir()) {
            long currentMSecs = conf->getValue("import_original_time").toLong();
        
            //Write the last imported data
            QFile saveFile(userPreferenceData.get("status_folder") + "/last.txt");
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
        
	    QString toBeWritten;
            if(currentMSecs >= lastMSecs) toBeWritten = QString::number(currentMSecs);
            else toBeWritten = QString::number(lastMSecs);
            if (saveFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
                saveFile.write(toBeWritten.toLatin1());
            }
            saveFile.close();
        
            //Write the time stamp in the last hour processed data
            ProjectData::writeStatisticsToStatusFolder("last_recorded.txt", currentMSecs);
            ProjectData::writeStatisticsToStatusFolder("last_imported.txt");
        }
     
        //Add the scripts to be executed during import
        QStringList scripts = importSelectorDialog.scriptPaths(ProjectPreferences().scripts("import"));
        for(QString script : scripts) {
            if(!script.isEmpty()) {
                // qDebug()<<"Adding to execution queue: "<<script;
                scriptsToBeExecuted_.append("SCRIPT:" + script);
            }
        }
    }
    
    //register that this image was imported
    
    // ImportFolderSettings(QDir(conf->getValue("import_dir"))).addImportedImage(baseName, importGroup_ + "/" + number, hasImage, hasXML);
    ImportFolderSettings(QDir(projectData.projectParameterData()->getValue("import_dir"))).addImportedImage(baseName, importGroup_ + "/" + number, hasImage, hasXML, hasSkipImage);
    
    if (! importThisOne) imageExecuting_ = 0;
   
    continueExecution();
}

void AutoImportWindow::continueExecution() {
    if (! imageExecuting_) {
        commandExecuting_ = "";
        importImage();
        return;
    }
    if (!commandExecuting_.isEmpty() && commandExecuting_.startsWith("SCRIPT:")) {
        QString resultsFile = QFileInfo(commandExecuting_.split(":")[1]).fileName().remove(QRegExp("\\.script$")) + ".results";
        qDebug() << "Saving results from file: " << resultsFile;

        ResultsData resultsData(imageExecuting_->workingPath());
        resultsData.load(imageExecuting_->workingPath() + "/LOGS/" + resultsFile);
        resultsData.save();
    }
    
    if (scriptsToBeExecuted_.isEmpty()) {
        commandExecuting_ = "";
        QStringList selectedScripts = selectedScriptPaths();
        if(imageExecuting_ && !selectedScripts.isEmpty()) {
            // qDebug() << "Adding to processing queue: " << imageExecuting_->toString();
            projectData.addImageToQueue(imageExecuting_, selectedScripts, priorityQueueOption_->isChecked());
        }
        importImage();
        return;
    }

    QString scriptPath = scriptsToBeExecuted_.first();
    commandExecuting_ = scriptPath;
    // qDebug() << "Import is executing: " << scriptPath;
    // qDebug() << endl << "scriptsToBeExecuted_ is now: "<<scriptsToBeExecuted_ << endl;
    scriptsToBeExecuted_.removeFirst();

    if(scriptPath.startsWith("SCRIPT:")) {
        scriptPath = scriptPath.split(":")[1];
        QString scriptName = QFileInfo(scriptPath).fileName().remove(QRegExp("\\.script$"));
        if (QFileInfo(scriptPath).exists()) {
            //qDebug() << "Executing script: " << scriptPath;
            ScriptParser parser(imageExecuting_->workingPath());
            parser.parse(scriptPath, imageExecuting_->workingPath() + "/proc/" + scriptName + ".com");
            process_.setWorkingDirectory(imageExecuting_->workingPath());
            process_.setStandardOutputFile(imageExecuting_->workingPath() + "/LOGS/" + scriptName + ".log");
            process_.start('"' + parser.executionString() + '"', QIODevice::ReadOnly);
        }
    }
    else {
        process_.start(scriptPath, QIODevice::ReadOnly);
    }
}

QStringList AutoImportWindow::selectedScriptPaths() {
    QStringList scripts;
    for (int row = whileImportScriptCount_; row < selectedScriptsCont->count(); row++) {
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
    
    if(watcher_.directories().size() > 0) watcher_.removePaths(watcher_.directories());
    
    QString currDir;
    currDir = importImagesPath;
    if(QFileInfo(currDir).isDir()) watcher_.addPath(currDir);
    
    //CHEN:ToDo : Also watch sub-directories here, for EPU.
}


QString AutoImportWindow::introText() {
    return QString("Directory, from where files are to be imported:");
}
