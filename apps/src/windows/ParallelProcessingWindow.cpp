#include <QtWidgets>

#include "ParallelProcessingWindow.h"

#include "ScriptModuleProperties.h"
#include "ScriptData.h"
#include "UserPreferences.h"
#include "GroupContainer.h"
#include "ProjectData.h"

ParallelProcessingWindow::ParallelProcessingWindow(QWidget* parent)
: QWidget(parent) {
    statusLabel_ = new QLabel("Idle");
    statusLabel_->setWordWrap(true);
    QFont font = statusLabel_->font();
    font.setBold(true);
    statusLabel_->setFont(font);
    changeSelectionCount(projectData.imagesSelected().count());
    connect(&projectData, &ProjectData::selectionChanged, [=](const QStringList& paths){
        if(!currentlyExecuting_) changeSelectionCount(paths.count());
    });

    importButton_ = new QPushButton(ApplicationData::icon("play"), tr("Start Processing"));
    importButton_->setCheckable(true);
    importButton_->setChecked(false);
    connect(importButton_, &QAbstractButton::clicked, this, &ParallelProcessingWindow::executeProcesses);
    
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

}

QWidget* ParallelProcessingWindow::setupInputContainer() {
    QWidget* mainContainer = new QWidget;
    QVBoxLayout* mainLayout = new QVBoxLayout();
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addStretch(0);

    mainLayout->addWidget(setupInputFolderContainer(), 0);
    mainLayout->addWidget(setupScriptsContainer(), 1);

    mainContainer->setMaximumWidth(580);
    mainContainer->setLayout(mainLayout);

    return mainContainer;
}

QWidget* ParallelProcessingWindow::setupInputFolderContainer() {
    GroupContainer* container = new GroupContainer();
    container->setTitle("Import Folder");

    QFormLayout* layout = new QFormLayout;
    layout->setHorizontalSpacing(10);
    layout->setVerticalSpacing(2);
    layout->setRowWrapPolicy(QFormLayout::DontWrapRows);
    layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    layout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    layout->setLabelAlignment(Qt::AlignLeft);

    QDir projectDir = projectData.projectDir();
    
    int numberOfThreads = QThread::idealThreadCount();
    if(numberOfThreads < 1) numberOfThreads = 2;
    
    IntLineEditSet* processesBox = new IntLineEditSet(1);
    processesBox->setAllRanges(1, numberOfThreads);
    processesBox->setValue(QString::number(ProjectPreferences(projectDir).processJobs()));
    connect(processesBox, &IntLineEditSet::valueChanged, [=](){
        ProjectPreferences(projectDir).setProcessJobs(processesBox->valueAt(0).toInt());
    });
    layout->addRow("Number of jobs to run in parallel", processesBox);
    
    QLabel* introLabel = new QLabel("The maximum number of threads available on your system is: " + QString::number(numberOfThreads));
    introLabel->setWordWrap(true);
    QPalette pal = introLabel->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    introLabel->setPalette(pal);
    layout->addRow(introLabel);
    
    container->setContainerLayout(layout);

    return container;
}

QWidget* ParallelProcessingWindow::setupScriptsContainer() {

    GroupContainer* scriptsContainer = new GroupContainer;
    scriptsContainer->setTitle("Scripts to be executed");
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
        

        ProjectPreferences(projectData.projectDir()).setProcessScripts(selectedScripts);
        resetSelectedScriptsContainer(scriptsAvailable);
    });

    GraphicalButton* deleteButton = new GraphicalButton(ApplicationData::icon("delete_selected"));
    deleteButton->setFixedSize(32, 32);
    connect(deleteButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedScripts = ProjectPreferences(projectData.projectDir()).processScripts();
        for (QModelIndex index : selectedScriptsCont->selectionModel()->selectedIndexes()) {
            selectedScripts.removeAll(index.data(Qt::DisplayRole).toString());
        }
        ProjectPreferences(projectData.projectDir()).setProcessScripts(selectedScripts);
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

QWidget* ParallelProcessingWindow::setupStatusContinaer() {
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch(0);
    buttonLayout->addWidget(importButton_, 0);
    buttonLayout->addStretch(1);

    progressBar_ = new QProgressBar;
    progressBar_->setMaximum(100);
    progressBar_->setFixedHeight(10);
    progressBar_->setValue(0);
    progressBar_->setTextVisible(false);
    progressBar_->hide();

    statusEntryTable_ = new QTableWidget(0, 3);
    statusEntryTable_->setAttribute(Qt::WA_MacShowFocusRect, 0);

    QStringList labels;
    labels << tr("Id") << tr("Image") << tr("Process");
    statusEntryTable_->setHorizontalHeaderLabels(labels);
    statusEntryTable_->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    statusEntryTable_->verticalHeader()->hide();
    statusEntryTable_->setShowGrid(false);
    statusEntryTable_->setAlternatingRowColors(true);
    statusEntryTable_->setSortingEnabled(true);
    statusEntryTable_->sortByColumn(2, Qt::SortOrder::AscendingOrder);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->setMargin(10);
    mainLayout->setSpacing(10);
    mainLayout->addWidget(statusLabel_);
    mainLayout->addLayout(buttonLayout);
    mainLayout->addWidget(progressBar_);
    mainLayout->addWidget(statusEntryTable_);

    GroupContainer* container = new GroupContainer;
    container->setTitle("Current Status");
    container->setContainerLayout(mainLayout);

    return container;
}

void ParallelProcessingWindow::resetSelectedScriptsContainer(QStringList availScripts) {
    selectedScriptsCont->clear();
    QStringList selectedScripts = ProjectPreferences(projectData.projectDir()).processScripts();
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

void ParallelProcessingWindow::executeProcesses(bool execute) {
    if (!execute) {
        qDebug() << "Stopping parallel processing";
        imagesToBeProcessed_.clear();

        progressBar_->hide();
        inputContiner_->setEnabled(true);
        scriptsToBeExecuted_.clear();
        
        for (int i=0; i<processors_.size(); ++i) {
            processors_[i]->stopExecution();
        }
        processorId_.clear();
        processors_.clear();
        currentlyExecuting_ = false;
        importButton_->setChecked(false);
        importButton_->setText("Start Processing");

        changeSelectionCount(projectData.imagesSelected().count());
        
        statusEntryTable_->resizeColumnToContents(0);
        statusEntryTable_->resizeColumnToContents(1);
        
    } else if (currentlyExecuting_) {
        qDebug() << "Currently working, skipping this processing";
        return;
    } else {

        currentlyExecuting_ = true;
        importButton_->setChecked(true);
        importButton_->setText("Stop Processing");
        
        imagesToBeProcessed_ = projectData.imagesSelected();
        
        progressBar_->setMaximum(imagesToBeProcessed_.size());
        progressBar_->setValue(0);
        progressBar_->show();
        
        statusLabel_->setText("Processing...");

        statusEntryTable_->setRowCount(0);
        statusEntryTable_->show();
        
        inputContiner_->setDisabled(true);
        
        scriptsToBeExecuted_ = selectedScriptPaths();

        processors_.clear();
        processorId_.clear();
        
        int numJobs = ProjectPreferences(projectData.projectDir()).processJobs();
        for(int i=0; i< numJobs; ++i) {
            ImageScriptProcessor* processor = new ImageScriptProcessor();
            processorId_.insert(processor, i);
            connect(processor, &ImageScriptProcessor::processFinished, [=]() {
                executeImage(processor);
            });
            connect(processor, &ImageScriptProcessor::statusChanged, [=](const QString& status, bool withError){
                addStatusToTable(processorId_[processor], processor->workingDir().canonicalPath(), status, withError);
            });
            processors_.append(processor);
            executeImage(processor);
        }
    }
}

void ParallelProcessingWindow::executeImage(ImageScriptProcessor* processor) {
            
    if(imagesToBeProcessed_.isEmpty()) {
        if(processor == processors_.last()) executeProcesses(false);
        return;
    }
    
    //Lock to protect the next lines to be done only by one processor at a time
    mutex_.lock();
    processor->execute(QDir(imagesToBeProcessed_.first()), scriptsToBeExecuted_);
    imagesToBeProcessed_.removeFirst();
    progressBar_->setValue(progressBar_->value() + 1);
    mutex_.unlock();
}

QStringList ParallelProcessingWindow::selectedScriptPaths() {
    QStringList paths;
    for (int row = 0; row < selectedScriptsCont->count(); row++) {
        paths.append(selectedScriptsCont->item(row)->data(Qt::UserRole + 5).toString());
    }
    return paths;
}

void ParallelProcessingWindow::addStatusToTable(int processId, const QString& image, const QString& text, bool error) {
    QStringList cell = text.split(';');
    for(QString s : cell) {
        if(!s.trimmed().isEmpty()) {
            QTableWidgetItem* idItem = new QTableWidgetItem();
            idItem->setFlags(idItem->flags() ^ Qt::ItemIsEnabled);
            idItem->setText(QString::number(processId));
            
            QTableWidgetItem* imageItem = new QTableWidgetItem();
            imageItem->setFlags(imageItem->flags() ^ Qt::ItemIsEnabled);
            imageItem->setText(projectData.projectDir().relativeFilePath(image));
            
            QTableWidgetItem* processItem = new QTableWidgetItem();
            processItem->setFlags(processItem->flags() ^ Qt::ItemIsEnabled);
            processItem->setText(QTime::currentTime().toString("hh:mm:ss.zzz") + " "  + text);
            
            if(error) {
                idItem->setForeground(Qt::red);
                imageItem->setForeground(Qt::red);
                processItem->setForeground(Qt::red);
            }
            int row = statusEntryTable_->rowCount();
            statusEntryTable_->setSortingEnabled(false);
            statusEntryTable_->insertRow(row);
            statusEntryTable_->setItem(row, 0, idItem);
            statusEntryTable_->setItem(row, 1, imageItem);
            statusEntryTable_->setItem(row, 2, processItem);
            statusEntryTable_->setSortingEnabled(true);
        }
    }
    statusEntryTable_->scrollToBottom();
}

void ParallelProcessingWindow::changeSelectionCount(int count) {
    statusLabel_->setText(QString::number(count) + " images are selected, and are ready to be processed.");
}

