#include "libraryContainer.h"

libraryContainer::libraryContainer(confData *dat, resultsData* results, QWidget* parent)
: QWidget(parent) {
    this->data = dat;

    selectionWidget = new QListWidget;
    selectionWidget->setFixedWidth(235);
    selectionWidget->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    selectionWidget->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Expanding);

    previews = new QStackedWidget(this);
    previews->setFixedWidth(235);

    mapPreview = new imagePreview(data, "NP", false, previews);
    refPreview = new imagePreview(data, "NR", false, previews);
    dualPreview = new imagePreview(data, "NH", false, previews);

    previews->addWidget(mapPreview);
    previews->addWidget(refPreview);
    previews->addWidget(dualPreview);

    setupDirectoryContainer(data);
    QToolBar* headerToolBar = setupContextAndMenu();
    dirModel->setResultsFile(results);

    QWidget* widget = new QWidget(this);
    QGridLayout* layout = new QGridLayout(widget);
    layout->setMargin(0);
    layout->setSpacing(0);
    widget->setLayout(layout);

    layout->addWidget(headerToolBar, 0, 0, 1, 1);
    layout->addWidget(dirView, 0, 1, 1, 1);

    previewTimer = new QTimer(this);
    connect(previewTimer, SIGNAL(timeout()), this, SLOT(updatePreview()));

    blockContainer* libraryContainer = new blockContainer(QString("Project Library (" + data->getDir("project") + ")"));
    libraryContainer->setMainWidget(widget);

    selectionState = new QLabel(" ");
    QFont* font = new QFont();
    font->setItalic(true);
    font->setPixelSize(10);
    selectionState->setFont(*font);
    resetSelectionState();

    libraryContainer->setHeaderWidget(selectionState);

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

    blockContainer* previewContainer = new blockContainer("", this);
    previewContainer->setFixedWidth(235);
    previewContainer->setMainWidget(previews);
    previewContainer->setHeaderWidget(headerWidget);

    QGridLayout* mainLayout = new QGridLayout(this);
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addWidget(libraryContainer, 0, 0, 2, 1);
    mainLayout->addWidget(previewContainer, 0, 1, 1, 1, Qt::AlignTop);
    mainLayout->addWidget(selectionWidget, 1, 1, 1, 1);

    mainLayout->setRowStretch(0, 0);
    mainLayout->setRowStretch(1, 1);

    this->setLayout(mainLayout);

}

void libraryContainer::showContents(bool show) {
    setVisible(show);
    QSize size = sizeHint();
    resize(size);
}

void libraryContainer::setupDirectoryContainer(confData* data) {
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

QToolBar* libraryContainer::setupContextAndMenu() {
    //Setup Actions
    QAction *addSelectionAction;
    addSelectionAction = new QAction(*(data->getIcon("add_selection")), "Check highlighted", dirView);
    connect(addSelectionAction, SIGNAL(triggered()), this, SLOT(extendSelection()));

    QAction *removeSelectionAction;
    removeSelectionAction = new QAction(*(data->getIcon("remove_selection")), "Uncheck highlighted", dirView);
    connect(removeSelectionAction, SIGNAL(triggered()), this, SLOT(reduceSelection()));

    QAction *copyImageAction;
    copyImageAction = new QAction(*(data->getIcon("copy_selection")), "Copy highlighted to second project", dirView);
    connect(copyImageAction, SIGNAL(triggered()), this, SLOT(copyImage()));

    QAction *moveImageAction;
    moveImageAction = new QAction(*(data->getIcon("move_selection")), "Move highlighted to another folder", dirView);
    connect(moveImageAction, SIGNAL(triggered()), this, SLOT(moveSelectiontoFolder()));

    QAction *trashImageAction;
    trashImageAction = new QAction(*(data->getIcon("trash_selection")), "Trash highlighted images", dirView);
    connect(trashImageAction, SIGNAL(triggered()), this, SLOT(trashSelection()));

    QAction *addFolderAction;
    addFolderAction = new QAction(*(data->getIcon("add_folder")), "Add image folder", dirView);
    connect(addFolderAction, SIGNAL(triggered()), this, SLOT(addImageFolder()));

    //Right-Click Menu
    dirView->setContextMenuPolicy(Qt::ActionsContextMenu);
    dirView->addAction(addSelectionAction);
    dirView->addAction(removeSelectionAction);
    dirView->addAction(addFolderAction);
    dirView->addAction(moveImageAction);
    dirView->addAction(trashImageAction);
    dirView->addAction(copyImageAction);


    //Setup tool bar
    QToolBar* toolBar = new QToolBar("Library Actions");
    toolBar->setIconSize(QSize(36, 36));
    toolBar->setOrientation(Qt::Vertical);
    toolBar->addAction(addSelectionAction);
    toolBar->addAction(removeSelectionAction);
    toolBar->addAction(addFolderAction);
    toolBar->addAction(moveImageAction);
    toolBar->addAction(trashImageAction);
    toolBar->addAction(copyImageAction);

    return toolBar;
}

projectModel* libraryContainer::getDirModel() {
    return dirModel;
}

QTreeView* libraryContainer::getDirView() {
    return dirView;
}

void libraryContainer::reload() {
    maskResults();
    updateModel();
}

void libraryContainer::loadProjectState() {
    QString projectHeaderState = data->getDir("working") + "/config/projectHeaderState.dat";
    if (QFileInfo(projectHeaderState).exists()) {
        QFile f(projectHeaderState);
        if (!f.open(QIODevice::ReadOnly)) return;
        dirView->header()->restoreState(f.readAll());
        f.close();
    }
}

void libraryContainer::saveProjectState() {
    QString projectHeaderState = data->getDir("working") + "/config/projectHeaderState.dat";
    if (!projectHeaderState.isEmpty()) {
        QFile f(projectHeaderState);
        if (!f.open(QIODevice::WriteOnly)) return;
        f.write(dirView->header()->saveState());
        f.close();
    }
}

void libraryContainer::showSelected(bool enable) {
    sortModel->setFilterRole(Qt::CheckStateRole);
    sortModel->setDynamicSortFilter(true);
    if (enable) {
        sortModel->setFilterRegExp((QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked)));
    } else {
        sortModel->setFilterRegExp(".*");
    }
}

bool libraryContainer::loadSelection(const QString &fileName) {
    return dirModel->loadSelection(fileName);
}

void libraryContainer::updateModel() {
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

void libraryContainer::maskResults() {
    dirModel->maskResults();
}

void libraryContainer::copyImage() {
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

void libraryContainer::addImageFolder(const QString& folder) {
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

void libraryContainer::addImageFolder() {
    bool ok;
    QString folder = QInputDialog::getText(this, tr("Add image folder"),
            tr("Enter the folder name to be created"), QLineEdit::Normal,
            "image_folder", &ok);

    if (ok && !folder.isEmpty()) {
        addImageFolder(folder);
    }
}

void libraryContainer::moveSelectionToFolder(const QString& targetPath) {
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

void libraryContainer::trashSelection() {
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

void libraryContainer::moveSelectiontoFolder() {
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

bool libraryContainer::copyRecursively(const QString& srcFilePath, const QString& tgtFilePath) {
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

void libraryContainer::columnActivated(int i) {
    dirModel->setColumnProperty(i, "visible", dirModel->getColumnProperty(i, "visible").toBool()^true);
    dirModel->saveColumns();
    dirView->setColumnHidden(i, !dirModel->getColumnProperty(i, "visible").toBool());
    dirView->resizeColumnToContents(i);
}

void libraryContainer::extendSelection() {
    modifySelection(true);
}

void libraryContainer::reduceSelection() {
    modifySelection(false);
}

void libraryContainer::modifySelection(bool select) {
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

void libraryContainer::setPreviewImages(const QString& imagePath) {
    loadDataContainer(imagePath);
    mapPreview->setImage(imagePath + "/final_map.mrc");
    refPreview->setImage(imagePath + "/reference_map.mrc");
    dualPreview->setImage(imagePath + "/half_half.mrc");
}

void libraryContainer::loadDataContainer(const QString& imagePath) {
    selectionWidget->clear();

    if(!QFileInfo(imagePath+"/2dx_image.cfg").exists() || imagePath.isEmpty()) return;
    
    QString confName = imagePath + "/" + "2dx_image.cfg";
    confData* data = new confData(confName);
    selectionWidget->addItem("Name: " + data->get("imagename", "value"));
    selectionWidget->addItem("Number: " + data->get("imagenumber", "value"));
    selectionWidget->addItem("QVal2: " + data->get("QVAL2", "value"));
    selectionWidget->addItem("QValS: " + data->get("QVALS", "value"));
    selectionWidget->addItem("Phase Residual: " + data->get("MergePhaseResidual", "value"));
    selectionWidget->addItem("TAXA: " + data->get("TAXA", "value"));
    selectionWidget->addItem("TANGL: " + data->get("TANGL", "value"));
    selectionWidget->addItem("Magnification: " + data->get("magnification", "value"));
    selectionWidget->addItem("Defocus: " + data->get("defocus", "value"));
    selectionWidget->addItem("Comment: " + data->get("comment", "value"));
}

void libraryContainer::autoSwitch(bool play) {
    if (play) previewTimer->start(1000);
    else previewTimer->stop();
}

void libraryContainer::updatePreview() {
    int id = (previews->currentIndex() + 1) % 2;
    viewControl->setCurrentIndex(id);
}

void libraryContainer::resetSelectionState() {
    QString checked = QString::number(dirModel->getSelectionNames().count());
    QString selected = QString::number(dirView->selectionModel()->selectedRows().count());
    selectionState->setText(checked + " checked and " + selected + " highlighted ");
}
