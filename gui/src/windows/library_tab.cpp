#include <QFormLayout>

#include "library_tab.h"

LibraryTab::LibraryTab(confData *dat, resultsData* results, QWidget* parent)
: QWidget(parent) {
    this->data = dat;

    previews = new QStackedWidget(this);
    previews->setFixedSize(235, 235);

    mapPreview = new imagePreview(data, "NP", false, previews);
    refPreview = new imagePreview(data, "NR", false, previews);
    dualPreview = new imagePreview(data, "NH", false, previews);

    previews->addWidget(mapPreview);
    previews->addWidget(refPreview);
    previews->addWidget(dualPreview);

    setupDirectoryContainer(data);
    dirModel->setResultsFile(results);

    previewTimer = new QTimer(this);
    connect(previewTimer, SIGNAL(timeout()), this, SLOT(updatePreview()));

    QToolButton* showHeaderButton = new QToolButton();
    showHeaderButton->setIcon(*(data->getIcon("info")));
    showHeaderButton->setToolTip("Show Image Header");
    showHeaderButton->setAutoRaise(false);
    showHeaderButton->setCheckable(true);
    connect(showHeaderButton, SIGNAL(toggled(bool)), mapPreview, SLOT(showImageHeader(bool)));
    connect(showHeaderButton, SIGNAL(toggled(bool)), refPreview, SLOT(showImageHeader(bool)));
    connect(showHeaderButton, SIGNAL(toggled(bool)), dualPreview, SLOT(showImageHeader(bool)));

    QToolButton* autoPreviewsButton = new QToolButton();
    autoPreviewsButton->setIcon(*(data->getIcon("timer")));
    autoPreviewsButton->setToolTip("Auto switch previews");
    autoPreviewsButton->setAutoRaise(false);
    autoPreviewsButton->setCheckable(true);
    connect(autoPreviewsButton, SIGNAL(toggled(bool)), this, SLOT(autoSwitch(bool)));

    //Setup preview control combo box
    viewControl = new QComboBox(this);
    viewControl->addItems(QStringList() << "Map Preview" << "Reference Preview" << "Half-Half Preview");
    connect(viewControl, SIGNAL(currentIndexChanged(int)), previews, SLOT(setCurrentIndex(int)));

    QWidget* headerWidget = new QWidget();
    QGridLayout* headerWidgetLayout = new QGridLayout(headerWidget);
    headerWidgetLayout->setMargin(0);
    headerWidgetLayout->setSpacing(0);
    headerWidget->setLayout(headerWidgetLayout);

    headerWidgetLayout->addWidget(viewControl, 0, 0, 1, 1, Qt::AlignVCenter);
    headerWidgetLayout->addItem(new QSpacerItem(3, 3), 0, 1);
    headerWidgetLayout->addWidget(showHeaderButton, 0, 2);
    headerWidgetLayout->addItem(new QSpacerItem(3, 3), 0, 3);
    headerWidgetLayout->addWidget(autoPreviewsButton, 0, 4);

    imageDataWidget = new libraryImageStatus(dirModel);
    imageDataWidget->setFixedHeight(235);
    
    QWidget* dataAndPreview = new QWidget;
    QHBoxLayout* dataAndPreviewLayout = new QHBoxLayout;
    dataAndPreviewLayout->setMargin(0);
    dataAndPreviewLayout->setSpacing(0);
    dataAndPreviewLayout->addWidget(imageDataWidget, 1);
    dataAndPreviewLayout->addWidget(previews, 0);
    dataAndPreview->setLayout(dataAndPreviewLayout);
    
    blockContainer* previewContainer = new blockContainer("Image Data and Preview", this);
    previewContainer->setMainWidget(dataAndPreview);
    previewContainer->setHeaderWidget(headerWidget);

    QGridLayout* mainLayout = new QGridLayout(this);
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addWidget(dirView, 0, 0);
    mainLayout->addWidget(previewContainer, 1, 0);
    mainLayout->addWidget(setupToolBar(), 0, 1, 2, 1, Qt::AlignTop);
    
    mainLayout->setColumnStretch(0, 1);
    mainLayout->setColumnStretch(1, 0);
    
    mainLayout->setRowStretch(0, 1);
    mainLayout->setRowStretch(1, 0);
    
    this->setLayout(mainLayout);

}

void LibraryTab::showContents(bool show) {
    setVisible(show);
    QSize size = sizeHint();
    resize(size);
}

void LibraryTab::setupDirectoryContainer(confData* data) {
    if (!QDir(data->getDir("project")).exists()) {
        dirView = NULL;
        std::cerr << "The project directory does not exit\n";
    }

    QString projectDir = QDir(data->getDir("project")).canonicalPath();

    dirModel = new projectModel(data, projectDir, data->getDir("working") + "/config/" + "projectMenu.inf", this);

    const QString& savePath = data->getDir("working") + "/2dx_merge_dirfile.dat";
    if (!savePath.isEmpty()) {
        dirModel->setSaveName(savePath);
        dirModel->loadSelection();
    }

    connect(dirModel, SIGNAL(currentImage(const QString&)), this, SLOT(setPreviewImages(const QString&)));
    connect(dirModel, SIGNAL(reloading()), this, SLOT(reload()));
    connect(dirModel, SIGNAL(submitting()), this, SLOT(resetSelectionState()));

    sortModel = new QSortFilterProxyModel(this);
    sortModel->setSourceModel(dirModel);
    sortModel->setDynamicSortFilter(true);
    sortModel->setSortRole(projectModel::SortRole);

    dirView = new QTreeView(this);
    dirView->setModel(sortModel);
    dirView->setSelectionMode(QAbstractItemView::ExtendedSelection);
    dirView->setSortingEnabled(true);
    dirView->setAllColumnsShowFocus(true);
    loadProjectState();
    connect(dirView->header(), SIGNAL(sectionMoved(int, int, int)), this, SLOT(saveProjectState()));
    connect(dirView->header(), SIGNAL(sectionResized(int, int, int)), this, SLOT(saveProjectState()));
    connect(dirView->header(), SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)), this, SLOT(saveProjectState()));

    connect(dirView->selectionModel(), SIGNAL(selectionChanged(const QItemSelection&, const QItemSelection&)), this, SLOT(resetSelectionState()));
    connect(dirView->selectionModel(), SIGNAL(currentRowChanged(const QModelIndex&, const QModelIndex&)), dirModel, SLOT(currentRowChanged(const QModelIndex&, const QModelIndex&)));

    projectDelegate *delegate = new projectDelegate(data);

    //  dirView->setItemDelegate(new projectDelegate(mainData));
    QItemDelegate *defaultDelegate = new QItemDelegate(data);
    defaultDelegate->setClipping(true);
    dirView->setAlternatingRowColors(true);
    dirView->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    dirView->setItemDelegateForColumn(0, defaultDelegate);
    dirView->setItemDelegateForColumn(1, defaultDelegate);
    for (int i = 2; i < sortModel->columnCount(); i++)
        dirView->setItemDelegateForColumn(i, delegate);


    //Add context menu of possible columns in the header
    QAction *action;

    QSignalMapper *mapper = new QSignalMapper(this);


    for (int i = 0; i < dirModel->columnCount(); i++) {
        bool visible = dirModel->getColumnProperty(i, "visible").toBool();
        dirView->setColumnHidden(i, !visible);

        action = new QAction(dirModel->getColumnProperty(i, "shortname").toString(), dirView);
        action->setCheckable(true);

        if (visible) action->setChecked(true);
        connect(action, SIGNAL(triggered(bool)), mapper, SLOT(map()));
        mapper->setMapping(action, i);

        dirView->header()->addAction(action);
    }
    connect(mapper, SIGNAL(mapped(int)), this, SLOT(columnActivated(int)));

    dirView->header()->setContextMenuPolicy(Qt::ActionsContextMenu);
    dirView->expandAll();
    int width = 0;
    for (int i = 0; i < dirView->model()->columnCount(); i++) {
        dirView->resizeColumnToContents(i);
        width += dirView->columnWidth(i);
    }
}

QWidget* LibraryTab::setupToolBar() {
    QTabWidget* tabWidget = new QTabWidget;
    tabWidget->addTab(setupSelectionTab(), "Select");
    tabWidget->addTab(setupProjectTab(), "Project");
    
    selectionState = new QLabel(" ");
    selectionState->setAlignment(Qt::AlignCenter);
    QFont* font = new QFont();
    font->setItalic(true);
    font->setPixelSize(10);
    selectionState->setFont(*font);
    resetSelectionState();
    
    //Setup tool bar
    QWidget* toolBar =  new QWidget;
    //toolBar->setFixedWidth(200);
    QVBoxLayout* toolBarLayout =  new QVBoxLayout;
    toolBarLayout->addStretch(0);
    toolBarLayout->setMargin(0);
    
    toolBarLayout->addWidget(selectionState);
    toolBarLayout->addWidget(tabWidget);
    toolBar->setLayout(toolBarLayout);
    
    return toolBar;
}

QWidget* LibraryTab::setupSelectionTab() {
    
    QSizePolicy sizePolicy((QSizePolicy::Policy)QSizePolicy::Minimum,(QSizePolicy::Policy)QSizePolicy::Fixed);
    sizePolicy.setHorizontalStretch(0);
    sizePolicy.setVerticalStretch(0);
    
    QPushButton *refreshAction = new QPushButton(*(data->getIcon("refresh")), tr("&Refresh Library"), this);
    refreshAction->setSizePolicy(sizePolicy);
    refreshAction->setShortcut(tr("Ctrl+Shift+r"));
    connect(refreshAction, SIGNAL(clicked()), this, SLOT(reload()));
    
    
    //Check Group
    QGroupBox* checkGroup = new QGroupBox("Manage Check");
    checkGroup->setSizePolicy(sizePolicy);
    QVBoxLayout* checkGroupLayout = new QVBoxLayout;
    
    QPushButton* showSelectedAction = new QPushButton(*(data->getIcon("selected")), tr("&Show checked images only"), this);
    showSelectedAction->setShortcut(tr("Ctrl+X"));
    showSelectedAction->setCheckable(true);
    connect(showSelectedAction, SIGNAL(toggled(bool)), this, SLOT(showSelected(bool)));
    checkGroupLayout->addWidget(showSelectedAction);
    
    QPushButton *selectAllAction = new QPushButton(*(data->getIcon("check_all")), "Check all images", this);
    selectAllAction->setShortcut(tr("Ctrl+A"));
    connect(selectAllAction, SIGNAL(clicked()), dirModel, SLOT(selectAll()));
    checkGroupLayout->addWidget(selectAllAction);
    
    QPushButton *invertSelectedAction = new QPushButton(*(data->getIcon("check_invert")), "Invert check", this);
    invertSelectedAction->setShortcut(tr("Ctrl+I"));
    connect(invertSelectedAction, SIGNAL(clicked()), dirModel, SLOT(invertSelection()));
    checkGroupLayout->addWidget(invertSelectedAction);
    
    QPushButton* addSelectionAction = new QPushButton(*(data->getIcon("add_selection")), "Check highlighted", dirView);
    connect(addSelectionAction, SIGNAL(clicked()), this, SLOT(extendSelection()));
    checkGroupLayout->addWidget(addSelectionAction);

    QPushButton* removeSelectionAction = new QPushButton(*(data->getIcon("remove_selection")), "Uncheck highlighted", dirView);
    connect(removeSelectionAction, SIGNAL(clicked()), this, SLOT(reduceSelection()));
    checkGroupLayout->addWidget(removeSelectionAction);

    checkGroup->setLayout(checkGroupLayout);
    
    //Auto Selection Group
    QGroupBox* autoSelectionGroup = new QGroupBox("Automatic Selection");
    
    minDegree = new QLineEdit;
    minDegree->setFrame(false);
    minDegree->setMaximumWidth(50);
    minDegree->setText("0");
    minDegree->setValidator(new QIntValidator(-90, 90));
    
    maxDegree = new QLineEdit;
    maxDegree->setFrame(false);
    maxDegree->setMaximumWidth(50);
    maxDegree->setText("90");
    maxDegree->setValidator(new QIntValidator(-90, 90));
    
    negPosOption = new QComboBox;
    negPosOption->addItems(QStringList() << "Yes" << "No");
    negPosOption->setCurrentIndex(0);
        
    QPushButton* changeSelection = new QPushButton("Change Selection");
    connect(changeSelection, SIGNAL(clicked()), this, SLOT(autoSelect()));
    
    QFormLayout* autoSelectLayout = new QFormLayout;
    autoSelectLayout->setRowWrapPolicy(QFormLayout::WrapLongRows);
    autoSelectLayout->setFieldGrowthPolicy(QFormLayout::FieldsStayAtSizeHint);
    autoSelectLayout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    autoSelectLayout->setLabelAlignment(Qt::AlignLeft);
    autoSelectLayout->addRow("Minimum Tilt Angle", minDegree);
    autoSelectLayout->addRow("Maximum Tilt Angle", maxDegree);
    autoSelectLayout->addRow("Positive/negative values", negPosOption);
    autoSelectLayout->addRow(changeSelection);
    
    autoSelectionGroup->setLayout(autoSelectLayout);
    
    //Image Folder Group
    QGroupBox* imageFolderGroup = new QGroupBox("Image Folder");
    imageFolderGroup->setSizePolicy(sizePolicy);
    QVBoxLayout* imageFolderGroupLayout = new QVBoxLayout;
    
    QPushButton* addFolderAction = new QPushButton(*(data->getIcon("add_folder")), "Add image folder", dirView);
    connect(addFolderAction, SIGNAL(clicked()), this, SLOT(addImageFolder()));
    imageFolderGroupLayout->addWidget(addFolderAction);
    
    QPushButton* moveImageAction = new QPushButton(*(data->getIcon("move_selection")), "Move highlighted", dirView);
    connect(moveImageAction, SIGNAL(clicked()), this, SLOT(moveSelectiontoFolder()));
    imageFolderGroupLayout->addWidget(moveImageAction);
    
    QPushButton* trashImageAction = new QPushButton(*(data->getIcon("trash_selection")), "Trash highlighted", dirView);
    connect(trashImageAction, SIGNAL(clicked()), this, SLOT(trashSelection()));
    imageFolderGroupLayout->addWidget(trashImageAction);
    
    QPushButton* copyImageAction = new QPushButton(*(data->getIcon("copy_selection")), "Copy to second project", dirView);
    connect(copyImageAction, SIGNAL(clicked()), this, SLOT(copyImage()));
    imageFolderGroupLayout->addWidget(copyImageAction);
    
    imageFolderGroup->setLayout(imageFolderGroupLayout);
    
    
    //Backup/Save Group
    
    QGroupBox* backupSaveGroup = new QGroupBox("Save and Backup");
    backupSaveGroup->setSizePolicy(sizePolicy);
    QVBoxLayout* backupSaveLayout = new QVBoxLayout;
    
    QPushButton *saveDirectorySelectionAction = new QPushButton(*(data->getIcon("check_save")), "Save checked list", this);
    connect(saveDirectorySelectionAction, SIGNAL(clicked()), this, SLOT(saveDirectorySelection()));
    backupSaveLayout->addWidget(saveDirectorySelectionAction);

    QPushButton *loadDirectorySelectionAction = new QPushButton(*(data->getIcon("check_load")), "Load checked list", this);
    connect(loadDirectorySelectionAction, SIGNAL(clicked()), this, SLOT(loadDirectorySelection()));
    backupSaveLayout->addWidget(loadDirectorySelectionAction);
    
    backupSaveGroup->setLayout(backupSaveLayout);
    
    QWidget* tab = new QWidget;
    QVBoxLayout* layout = new QVBoxLayout;
    layout->setSpacing(10);
    layout->addStretch(0);
    layout->addWidget(refreshAction, 0, Qt::AlignTop);
    layout->addWidget(checkGroup, 0, Qt::AlignTop);
    layout->addWidget(autoSelectionGroup, 0);
    layout->addWidget(imageFolderGroup, 0, Qt::AlignTop);
    layout->addWidget(backupSaveGroup, 0, Qt::AlignTop);
    tab->setLayout(layout);
    return tab;
}

QWidget* LibraryTab::setupProjectTab() {
    
    
    QWidget* tab = new QWidget;
    QVBoxLayout* layout = new QVBoxLayout;
    layout->addStretch(0);
    
    tab->setLayout(layout);
    return tab;
}

void LibraryTab::loadDirectorySelection() {
    QString loadName = QFileDialog::getOpenFileName(this, "Save Selection As...", data->getDir("working") + "/2dx_merge_dirfile.dat");
    loadSelection(loadName);
}

void LibraryTab::saveDirectorySelection() {
    QString saveName = QFileDialog::getSaveFileName(this, "Save Selection As...", data->getDir("working") + "/2dx_merge_dirfile.dat");
    if (QFileInfo(saveName).exists()) QFile::remove(saveName);
    QFile::copy(data->getDir("working") + "/2dx_merge_dirfile.dat", saveName);
}

projectModel* LibraryTab::getDirModel() {
    return dirModel;
}

QTreeView* LibraryTab::getDirView() {
    return dirView;
}

void LibraryTab::reload() {
    maskResults();
    updateModel();
}

void LibraryTab::loadProjectState() {
    QString projectHeaderState = data->getDir("working") + "/config/projectHeaderState.dat";
    if (QFileInfo(projectHeaderState).exists()) {
        QFile f(projectHeaderState);
        if (!f.open(QIODevice::ReadOnly)) return;
        dirView->header()->restoreState(f.readAll());
        f.close();
    }
}

void LibraryTab::saveProjectState() {
    QString projectHeaderState = data->getDir("working") + "/config/projectHeaderState.dat";
    if (!projectHeaderState.isEmpty()) {
        QFile f(projectHeaderState);
        if (!f.open(QIODevice::WriteOnly)) return;
        f.write(dirView->header()->saveState());
        f.close();
    }
}

void LibraryTab::showSelected(bool enable) {
    sortModel->setFilterRole(Qt::CheckStateRole);
    sortModel->setDynamicSortFilter(true);
    if (enable) {
        sortModel->setFilterRegExp((QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked)));
    } else {
        sortModel->setFilterRegExp(".*");
    }
}

bool LibraryTab::loadSelection(const QString &fileName) {
    return dirModel->loadSelection(fileName);
}

void LibraryTab::updateModel() {
    dirModel->reload();

    for (int i = 0; i < dirModel->columnCount(); i++) {
        bool visible = dirModel->getColumnProperty(i, "visible").toBool();
        dirView->setColumnHidden(i, !visible);
    }

    dirView->expandAll();
    int width = 0;
    for (int i = 0; i < dirView->model()->columnCount(); i++) {
        dirView->resizeColumnToContents(i);
        width += dirView->columnWidth(i);
    }
    dirModel->loadSelection();
    dirView->expandAll();
}

void LibraryTab::maskResults() {
    dirModel->maskResults();
}

void LibraryTab::copyImage() {
    QString secondDir = data->get("second_dir", "value");

    if (secondDir == "-" || secondDir == "") {
        QMessageBox::critical(
                this,
                tr("Copy Error"),
                QString("Second project not set!\n")
                + QString("Use <Prepare second Project> script from custom scripts to change it!")
                );
        return;
    }

    QString targetDirPath = QString(secondDir + "/export/");


    QDir targetDir = QDir(targetDirPath);

    if (!targetDir.exists()) {
        targetDir.mkpath(targetDirPath);
    }

    QModelIndex i;
    QModelIndexList selection = dirView->selectionModel()->selectedRows();

    foreach(i, selection) {
        QString sourcePath = dirModel->pathFromIndex(i);
        QFileInfo fi(sourcePath);
        QString imageDirName = fi.fileName();

        QDir targetImageDir = QDir(targetDir.absolutePath() + "/" + imageDirName);

        bool target_exist = false;
        while (targetImageDir.exists()) {
            targetImageDir.setPath(targetImageDir.absolutePath() + "_1");
            target_exist = true;
        }

        if (target_exist) {
            QMessageBox::warning(
                    this,
                    tr("Move warning"),
                    "The target folder: " + targetDirPath + "/" + imageDirName + " already exists!\n"
                    + "Renaming it to: " + targetImageDir.absolutePath()
                    );
        }

        bool moved = copyRecursively(sourcePath, targetImageDir.absolutePath());
        if (!moved) {
            QMessageBox::warning(
                    this,
                    tr("Warning: Unable to copy"),
                    "Unable to move folder: " + sourcePath + " to:\n"
                    + targetImageDir.absolutePath()
                    );
            targetImageDir.removeRecursively();
        } else {
            qDebug() << "Moved folder: " + sourcePath + " to: " + targetImageDir.absolutePath();
        }
    }
}

void LibraryTab::addImageFolder(const QString& folder) {
    QString projectFolder = data->getDir("project");
    QDir projectDir(projectFolder);
    QFile newDirCfg(projectFolder + "/" + folder + "/2dx_master.cfg");
    if (newDirCfg.exists()) {
        QMessageBox::warning(
                this,
                tr("Add image folder"),
                "The image folder: <" + folder + "> already exists and is linked!"
                );
    } else {
        if (!QDir(projectFolder + "/" + folder).exists()) {
            projectDir.mkdir(folder);
        }
        QFile(projectFolder + "/2dx_master.cfg").link(QString("../2dx_master.cfg"), projectFolder + "/" + folder + "/2dx_master.cfg");
    }

    reload();

}

void LibraryTab::addImageFolder() {
    bool ok;
    QString folder = QInputDialog::getText(this, tr("Add image folder"),
            tr("Enter the folder name to be created"), QLineEdit::Normal,
            "image_folder", &ok);

    if (ok && !folder.isEmpty()) {
        addImageFolder(folder);
    }
}

void LibraryTab::moveSelectionToFolder(const QString& targetPath) {
    QModelIndexList selection = dirView->selectionModel()->selectedRows();
    int numFiles = selection.count();
    int count = 0;
    QProgressDialog progress("Moving files...", "Abort Move", 0, numFiles, this);
    progress.setWindowModality(Qt::WindowModal);

    QModelIndex i;

    foreach(i, selection) {
        progress.setValue(count++);
        if (progress.wasCanceled()) break;

        QString sourcePath = dirModel->pathFromIndex(i);
        QString sourceImage = QFileInfo(sourcePath).fileName();

        if (!QFile(sourcePath + "/2dx_image.cfg").exists()) {
            qDebug() << "2dx_image.cfg does not exist";
            continue;
        }

        QDir targetImageDir = QDir(targetPath + "/" + sourceImage);

        if (targetImageDir.absolutePath() == QDir(sourcePath).absolutePath()) {
            qDebug() << "Target same as source";
            continue;
        }

        bool target_exist = false;
        while (targetImageDir.exists()) {
            targetImageDir.setPath(targetImageDir.absolutePath() + "_1");
            target_exist = true;
        }

        if (target_exist) {
            QMessageBox::warning(
                    this,
                    tr("Move warning"),
                    "The target folder: " + targetPath + "/" + sourceImage + " already exists!\n"
                    + "Renaming it to: " + targetImageDir.absolutePath()
                    );
        }

        bool moved = copyRecursively(sourcePath, targetImageDir.absolutePath());
        if (!moved) {
            QMessageBox::warning(
                    this,
                    tr("Warning: Unable to copy"),
                    "Unable to move folder: " + sourcePath + " to:\n"
                    + targetImageDir.absolutePath()
                    );
            targetImageDir.removeRecursively();
        } else {
            qDebug() << "Moved folder: " + sourcePath + " to: " + targetImageDir.absolutePath();
            dirModel->itemDeselected(sortModel->mapToSource(i));
            QDir(sourcePath).removeRecursively();
        }

    }

    progress.setValue(numFiles);
    reload();

}

void LibraryTab::trashSelection() {
    QString projectFolder = data->getDir("project");
    QDir projectDir(projectFolder);
    QString folder = "TRASH";
    if (!QFile(data->getDir("project") + "/" + folder + "/2dx_master.cfg").exists()) {
        if (!QDir(projectFolder + "/" + folder).exists()) {
            projectDir.mkdir(folder);
        }
        QFile().link(projectFolder + "/2dx_master.cfg", projectFolder + "/" + folder + "/2dx_master.cfg");
    }
    moveSelectionToFolder(data->getDir("project") + "/" + folder);
}

void LibraryTab::moveSelectiontoFolder() {
    QString projectFolder = data->getDir("project");

    //Get the list of current folders
    QStringList imageFolders = QDir(projectFolder).entryList(QDir::NoDotAndDotDot | QDir::Dirs);

    //Remove non-linked folders
    QString imageFolder;

    foreach(imageFolder, imageFolders) {
        if (!QFile(projectFolder + "/" + imageFolder + "/2dx_master.cfg").exists()) imageFolders.removeAll(imageFolder);
    }

    //Ask for the folder to move to!
    bool ok;
    QString folder = QInputDialog::getItem(this, tr("Folder selection"),
            tr("Select one of the following available folder to be moved to:"), imageFolders, 0, false, &ok);

    if (ok && !folder.isEmpty()) {
        QString targetPath = projectFolder + "/" + folder;
        moveSelectionToFolder(targetPath);
    }

}

bool LibraryTab::copyRecursively(const QString& srcFilePath, const QString& tgtFilePath) {
    QFileInfo srcFileInfo(srcFilePath);
    if (srcFileInfo.isDir()) {
        QDir targetDir(tgtFilePath);
        targetDir.cdUp();
        if (!targetDir.mkdir(QFileInfo(tgtFilePath).fileName())) {
            qDebug() << "Error in mkdir: " + QFileInfo(tgtFilePath).fileName();
            return false;
        }
        QDir sourceDir(srcFilePath);
        QStringList fileNames = sourceDir.entryList(QDir::Files | QDir::Dirs | QDir::NoDotAndDotDot | QDir::Hidden | QDir::System);

        foreach(const QString &fileName, fileNames) {
            const QString newSrcFilePath
                    = srcFilePath + QLatin1Char('/') + fileName;
            const QString newTgtFilePath
                    = tgtFilePath + QLatin1Char('/') + fileName;
            if (!copyRecursively(newSrcFilePath, newTgtFilePath))
                return false;
        }
    } else {
        if (!QFile::copy(srcFilePath, tgtFilePath)) {
            qDebug() << "Error in copying file: " + srcFilePath + " to: " + tgtFilePath;
            return false;
        }
    }
    return true;
}

void LibraryTab::columnActivated(int i) {
    dirModel->setColumnProperty(i, "visible", dirModel->getColumnProperty(i, "visible").toBool()^true);
    dirModel->saveColumns();
    dirView->setColumnHidden(i, !dirModel->getColumnProperty(i, "visible").toBool());
    dirView->resizeColumnToContents(i);
}

void LibraryTab::autoSelect() {
    bool useAbsolute = false;
    if(negPosOption->currentText() == "Yes") useAbsolute=true;
    
    dirModel->autoSelect(minDegree->text().toInt(), maxDegree->text().toInt(), useAbsolute);
}

void LibraryTab::extendSelection() {
    modifySelection(true);
}

void LibraryTab::reduceSelection() {
    modifySelection(false);
}

void LibraryTab::modifySelection(bool select) {
    QModelIndex i;
    QModelIndexList selection = dirView->selectionModel()->selectedRows();

    if (select) {

        foreach(i, selection) {
            dirModel->itemSelected(sortModel->mapToSource(i));
        }

    } else {

        foreach(i, selection) {
            dirModel->itemDeselected(sortModel->mapToSource(i));
        }
    }
}

void LibraryTab::setPreviewImages(const QString& imagePath) {
    loadDataContainer(imagePath);
    mapPreview->setImage(imagePath + "/final_map.mrc");
    refPreview->setImage(imagePath + "/reference_map.mrc");
    dualPreview->setImage(imagePath + "/half_half.mrc");
}

void LibraryTab::loadDataContainer(const QString& imagePath) {
    imageDataWidget->updateData();
}

void LibraryTab::autoSwitch(bool play) {
    if (play) previewTimer->start(1000);
    else previewTimer->stop();
}

void LibraryTab::updatePreview() {
    int id = (previews->currentIndex() + 1) % 2;
    viewControl->setCurrentIndex(id);
}

void LibraryTab::resetSelectionState() {
    QString checked = QString::number(dirModel->getSelectionNames().count());
    QString selected = QString::number(dirView->selectionModel()->selectedRows().count());
    selectionState->setText(checked + " checked and " + selected + " highlighted ");
}
