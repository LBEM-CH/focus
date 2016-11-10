#include <QtWidgets>
#ifdef Q_OS_MAC
#include <QtMac>
#endif

#include "ProcessingManager.h"
#include "ApplicationData.h"
#include "GroupContainer.h"
#include "BlockContainer.h"

QMutex ProcessingManager::mutex_;

ProcessingManager::ProcessingManager(QWidget* parent) :
QWidget(parent) {
    QSplitter* mainSplitter = new QSplitter(Qt::Horizontal);
    mainSplitter->setHandleWidth(4);
    mainSplitter->addWidget(setupQueueContainer());
    mainSplitter->addWidget(setupStatusContainer());

    mainSplitter->setStretchFactor(0, 1);
    mainSplitter->setStretchFactor(1, 1);

    QGridLayout* mainLayout = new QGridLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(mainSplitter);
    setLayout(mainLayout);
    
    setQueueCount(queueModel_->rowCount());
    connect(queueModel_, &ProcessingModel::rowCountChanged, this, &ProcessingManager::setQueueCount);
}

QWidget* ProcessingManager::setupQueueContainer() {
    queueModel_ = new ProcessingModel(this);
    connect(&projectData, &ProjectData::toBeAddedToProcessingQueue, queueModel_, &ProcessingModel::addProcesses);
    
    QFormLayout* layout = new QFormLayout;
    layout->setHorizontalSpacing(10);
    layout->setVerticalSpacing(2);
    layout->setRowWrapPolicy(QFormLayout::DontWrapRows);
    layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    layout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    layout->setLabelAlignment(Qt::AlignLeft);

    int numberOfThreads = QThread::idealThreadCount();
    if(numberOfThreads < 1) numberOfThreads = 1;
    
    processesBox_ = new QSpinBox;
    processesBox_->setFrame(false);
    processesBox_->setMinimum(1);
    processesBox_->setMaximum(numberOfThreads);
    processesBox_->setValue(ProjectPreferences().processJobs());
    connect(processesBox_, static_cast<void(QSpinBox::*)(int)>(&QSpinBox::valueChanged), [=](int value){
        ProjectPreferences().setProcessJobs(value);
    });
    layout->addRow("Number of jobs to run in parallel", processesBox_);
    
    QLabel* introLabel = new QLabel("The maximum number of threads on your system is: " + QString::number(numberOfThreads));
    introLabel->setWordWrap(true);
    QPalette pal = introLabel->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    introLabel->setPalette(pal);
    layout->addRow(introLabel);
    
    GroupContainer* jobcontainer = new GroupContainer;
    jobcontainer->setTitle("Concurrency Selection");
    jobcontainer->setContainerLayout(layout);
    
    QPushButton* clearSelectedButton = new QPushButton("Remove highlighted");
    connect(clearSelectedButton, &QPushButton::clicked, [=]() {
        while(!queueView_->selectionModel()->selectedIndexes().isEmpty()) {
            QModelIndex i = queueView_->selectionModel()->selectedIndexes().first();
            if(!i.parent().isValid()) {
                queueModel_->removeRow(i.row());
            }
        }
        
        setQueueCount(queueModel_->rowCount());
    });
    
    clearButton_ = new QPushButton("Remove All");
    connect(clearButton_, &QAbstractButton::clicked, queueModel_, &ProcessingModel::clearAll);
    
    QPushButton* prioritizeButton = new QPushButton("Prioritize highlighted");
    connect(prioritizeButton, &QPushButton::clicked, [=]() {
        for(QModelIndex i : queueView_->selectionModel()->selectedRows(0)) {
            if(!i.parent().isValid()) {
                queueModel_->insertRow(0, queueModel_->takeRow(i.row()));
            }
        }
    });
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch(0);
    buttonLayout->addWidget(prioritizeButton, 0);
    buttonLayout->addStretch(1);
    buttonLayout->addWidget(clearButton_, 0);
    buttonLayout->addWidget(clearSelectedButton, 0);
    
    
    queueView_ = new QTreeView(this);
    queueView_->setAttribute(Qt::WA_MacShowFocusRect, 0);
    queueView_->setModel(queueModel_);
    queueView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    queueView_->setSortingEnabled(true);
    queueView_->setAllColumnsShowFocus(true);
    queueView_->setAlternatingRowColors(true);
    queueView_->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    queueView_->setHeaderHidden(true);
    
    BlockContainer* queueContainer = new BlockContainer("Images in queue", queueView_);
    
    QVBoxLayout *queueLayout = new QVBoxLayout;
    queueLayout->setMargin(10);
    queueLayout->setSpacing(10);
    queueLayout->addLayout(buttonLayout, 0);
    queueLayout->addWidget(queueContainer, 1);

    GroupContainer* container = new GroupContainer;
    container->setTitle("Processing Queue");
    container->setContainerLayout(queueLayout);
    
    
    QVBoxLayout* mainLayout = new QVBoxLayout();
    mainLayout->setMargin(0);
    mainLayout->setSpacing(10);
    mainLayout->addStretch(0);
    mainLayout->addWidget(jobcontainer, 0);
    mainLayout->addWidget(container, 1);
    
    QWidget* mainContainer = new QWidget;
    mainContainer->setLayout(mainLayout);
    mainContainer->setMaximumWidth(500);
    
    return mainContainer;
}

QWidget* ProcessingManager::setupStatusContainer() {
    queueLabel_ = new QLabel("Idle");
    queueLabel_->setWordWrap(true);
    QFont font = queueLabel_->font();
    font.setBold(true);
    queueLabel_->setFont(font);
    
    processingLabel_ = new QLabel("Processing, ");
    processingLabel_->setFont(font);
    processingLabel_->setVisible(false);
    
    QHBoxLayout* labelLayout = new QHBoxLayout();
    labelLayout->setSpacing(0);
    labelLayout->addStretch(0);
    labelLayout->addWidget(processingLabel_, 0);
    labelLayout->addWidget(queueLabel_, 0);
    labelLayout->addStretch(1);
    
    executeButton_ = new QPushButton(ApplicationData::icon("play"), tr("Start Processing"));
    executeButton_->setCheckable(true);
    executeButton_->setChecked(false);
    connect(executeButton_, &QAbstractButton::clicked, this, &ProcessingManager::executeProcesses);
    
    autoProcessButton_ = new QCheckBox("Automatically start processing, once there are images in queue");
    autoProcessButton_->setChecked(ProjectPreferences().processAutoCheck());
    connect(autoProcessButton_, &QCheckBox::toggled, [=](bool check){
        ProjectPreferences().setProcessAutoCheck(check);
    });
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch(0);
    buttonLayout->addWidget(executeButton_, 0);
    buttonLayout->addStretch(1);
    buttonLayout->addWidget(autoProcessButton_, 0);
    
    statusEntryTable_ = new QTableWidget(0, 3);
    statusEntryTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
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

    connect(statusEntryTable_, &QTableWidget::itemDoubleClicked, [ = ](QTableWidgetItem* it){
        if (it && it->row() != -1) {
            QTableWidgetItem* sibling = statusEntryTable_->item(it->row(), 1);
            if (sibling && !sibling->toolTip().isEmpty()) {
                ProjectImage* image = projectData.projectImage(sibling->toolTip());
                projectData.openImage(image);
            }
        }
    });

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->setMargin(10);
    mainLayout->setSpacing(10);
    mainLayout->addLayout(labelLayout);
    mainLayout->addLayout(buttonLayout);
    mainLayout->addWidget(statusEntryTable_);

    GroupContainer* container = new GroupContainer;
    container->setTitle("Current Status");
    container->setContainerLayout(mainLayout);
    
    return container;
}

void ProcessingManager::executeProcesses(bool execute) {
    if (!execute) {
        qDebug() << "Stopping parallel processing";
        currentlyExecuting_ = false;
        processingLabel_->setVisible(false);
        for(ImageScriptProcessor* processor : processors_) processor->stopExecution();
        processesBox_->setEnabled(true);
        executeButton_->setChecked(false);
        executeButton_->setText("Start Processing");
    } else if (currentlyExecuting_) {
        qDebug() << "Currently working, skipping this processing";
    } else {
        if(queueModel_->rowCount() == 0) {
            QMessageBox::information(this, "Empty selection", "No images are in queue, so nothing to process.\n\nAdd images to queue from LIBRARY tab to start processing them.");
            executeProcesses(false);
            return;
        }
        
        currentlyExecuting_ = true;
        processingLabel_->setVisible(true);
        processesBox_->setEnabled(false);
        executeButton_->setChecked(true);
        executeButton_->setText("Stop Processing");
        statusEntryTable_->setRowCount(0);
    
        distributeProcesses();
    }
}

void ProcessingManager::distributeProcesses() {
    QMutexLocker locker(&ProcessingManager::mutex_);
    
    int numJobs = ProjectPreferences().processJobs();
    setupProcessors(numJobs);
    
    //Loop over all the processors that are free
    for (ImageScriptProcessor* processor : processors_) {
        int queueSize = queueModel_->rowCount();
        if (processor->state() == QProcess::NotRunning && queueSize > 0) {
            QMap<ProjectImage*, QStringList> next = queueModel_->nextInQueue();
            ProjectImage* image=0;
            QStringList scripts;
            if (!next.isEmpty()) {
                image = next.keys().first();
                scripts = next[image];
            }

            if (image && !scripts.isEmpty()) {
                processor->execute(image, scripts);
            }
        }
    }

    //Check if all the processors are free, if yes stop the execution
    bool running = false;
    for (ImageScriptProcessor* processor : processors_) {
        if(processor->state() == QProcess::Running || processor->state() == QProcess::Starting){
            running = true;
            break;
        }
    }
    
    if(!running) executeProcesses(false);
    
}

void ProcessingManager::setupProcessors(int numProcessors) {
    int difference = numProcessors - processors_.size();
    if(difference == 0) return;
    
    //Add new ones if required
    if(difference > 0) {
        for(int i=processors_.size(); i<numProcessors; ++i) {
            ImageScriptProcessor* processor = new ImageScriptProcessor(this);
            processorId_.insert(processor, i);
            processor->connect(processor, &ImageScriptProcessor::processFinished, [=]() {
                if(currentlyExecuting_) distributeProcesses();
            });
            processor->connect(processor, &ImageScriptProcessor::statusChanged, [=](const QString& status, bool withError){
                addStatusToTable(processorId_[processor], processor->workingImage(), status, withError);
            });
            processors_.append(processor);
        }
    }
}

void ProcessingManager::setQueueCount(int count) {
    if(count <= 0) queueLabel_->setText("Nothing in queue");
    else if(count == 1) queueLabel_->setText(QString::number(count) + " image is in queue");
    else queueLabel_->setText(QString::number(count) + " images are in queue");

    //Put it on icon for OSX
#ifdef Q_OS_MAC
    QString badgeStr;
    if(count > 0) badgeStr = QString::number(count);
    QtMac::setBadgeLabelText(badgeStr);
#endif
    
    //Execute process if required
    if(!currentlyExecuting_ && count > 0 && autoProcessButton_->isChecked()) executeProcesses(true);
}

void ProcessingManager::addStatusToTable(int processId, ProjectImage* image, const QString& text, bool error) {
    QStringList cell = text.split(';');
    for(QString s : cell) {
        if(!s.trimmed().isEmpty()) {
            QTableWidgetItem* idItem = new QTableWidgetItem();
            idItem->setFlags(idItem->flags() ^ Qt::ItemIsEditable);
            idItem->setText(QString::number(processId));
            
            QTableWidgetItem* imageItem = new QTableWidgetItem();
            imageItem->setFlags(imageItem->flags() ^ Qt::ItemIsEditable);
            QString imageStr, imageToolTip;
            if(image) {
                imageStr = image->toString();
                imageToolTip = image->workingPath();
            }
            imageItem->setText(imageStr);
            imageItem->setToolTip(imageToolTip);
            
            QTableWidgetItem* processItem = new QTableWidgetItem();
            processItem->setFlags(processItem->flags() ^ Qt::ItemIsEditable);
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
    statusEntryTable_->resizeColumnToContents(0);
    statusEntryTable_->resizeColumnToContents(1);
    statusEntryTable_->scrollToBottom();
}

