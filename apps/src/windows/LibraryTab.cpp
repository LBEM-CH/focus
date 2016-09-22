#include <QtWidgets>

#include "ProjectData.h"
#include "ApplicationData.h"
#include "LibraryTab.h"
#include "ProjectPreferences.h"

LibraryTab::LibraryTab(QWidget* parent)
: QWidget(parent) {

    previews = new QStackedWidget(this);
    previews->setMinimumSize(235, 235);

    mapPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("Image not<br>processed"), previews);
    refPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("No reference<br>available"), previews);
    dualPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("Half-Half map<br>not available"), previews);

    previews->addWidget(mapPreview);
    previews->addWidget(refPreview);
    previews->addWidget(dualPreview);

    setupDirectoryContainer();

    previewTimer = new QTimer(this);
    connect(previewTimer, SIGNAL(timeout()), this, SLOT(updatePreview()));

    showHeaderButton = new QToolButton();
    showHeaderButton->setIcon(ApplicationData::icon("info"));
    showHeaderButton->setToolTip("Show Image Header");
    showHeaderButton->setAutoRaise(false);
    showHeaderButton->setCheckable(true);
    showHeaderButton->setChecked(false);
    connect(showHeaderButton, &QToolButton::toggled, [=] (bool) {
        setPreviewImages(dirModel->getCurrentRowPath());
    });

    QToolButton* autoPreviewsButton = new QToolButton();
    autoPreviewsButton->setIcon(ApplicationData::icon("timer"));
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

    imageDataWidget = new LibraryImageStatus(dirModel);
    imageDataWidget->setFixedHeight(235);
    
    QWidget* dataAndPreview = new QWidget;
    QHBoxLayout* dataAndPreviewLayout = new QHBoxLayout;
    dataAndPreviewLayout->setMargin(0);
    dataAndPreviewLayout->setSpacing(0);
    dataAndPreviewLayout->addWidget(imageDataWidget, 1);
    dataAndPreviewLayout->addWidget(previews, 0);
    dataAndPreview->setLayout(dataAndPreviewLayout);
    
    BlockContainer* previewContainer = new BlockContainer("Image Data and Preview", dataAndPreview, headerWidget, this);

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

    connect(&projectData, &ProjectData::imageDirsChanged, [=] () {
        reload();
    });
}

void LibraryTab::showContents(bool show) {
    setVisible(show);
    QSize size = sizeHint();
    resize(size);
}

void LibraryTab::setupDirectoryContainer() {
    if (!projectData.projectDir().exists()) {
        dirView = NULL;
        std::cerr << "The project directory does not exit\n";
    }

    QString projectDir = projectData.projectDir().canonicalPath();

    dirModel = new ProjectModel(projectDir, projectData.projectWorkingDir().canonicalPath() + "/config/projectMenu.inf", this);

    QString savePath = projectData.selectionDirfile();
    QString evenPath = projectData.evenSelectionDirfile();
    QString oddPath = projectData.oddSelectionDirfile();
    if (!savePath.isEmpty()) {
        dirModel->setSaveName(savePath);
        dirModel->setEvenImageFileName(evenPath);
        dirModel->setOddImageFileName(oddPath);
        dirModel->loadSelection();
    }

    connect(dirModel, SIGNAL(currentImage(const QString&)), this, SLOT(setPreviewImages(const QString&)));
    connect(dirModel, SIGNAL(reloading()), this, SLOT(reload()));
    connect(dirModel, SIGNAL(submitting()), this, SLOT(resetSelectionState()));

    sortModel = new QSortFilterProxyModel(this);
    sortModel->setSourceModel(dirModel);
    sortModel->setDynamicSortFilter(true);
    sortModel->setSortRole(ProjectModel::SortRole);

    dirView = new QTreeView(this);
    dirView->setAttribute(Qt::WA_MacShowFocusRect, 0);
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

    ProjectDelegate *delegate = new ProjectDelegate();

    //  dirView->setItemDelegate(new projectDelegate(mainData));
    QItemDelegate *defaultDelegate = new QItemDelegate();
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
    tabWidget->addTab(setupSelectionTab(), "Library Tools");
    
    projectNameLabel = new QLabel("");
    projectNameLabel->setAlignment(Qt::AlignCenter);
    projectNameLabel->setWordWrap(true);
    QFont font = projectNameLabel->font();
    font.setBold(true);
    font.setPointSize(18);
    projectNameLabel->setFont(font);
    updateProjectName();
    
    selectionState = new QLabel(" ");
    selectionState->setAlignment(Qt::AlignCenter);
    QPalette pal = selectionState->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    selectionState->setPalette(pal);
    resetSelectionState();
    
    //Setup tool bar
    QWidget* toolBar =  new QWidget;
    //toolBar->setFixedWidth(200);
    QVBoxLayout* toolBarLayout =  new QVBoxLayout;
    toolBarLayout->addStretch(0);
    toolBarLayout->setMargin(10);
    toolBarLayout->setSpacing(15);
    
    toolBarLayout->addWidget(projectNameLabel);
    toolBarLayout->addWidget(selectionState);
    toolBarLayout->addWidget(tabWidget);
    toolBar->setLayout(toolBarLayout);
    
    return toolBar;
}

QWidget* LibraryTab::setupSelectionTab() {
    
    QSizePolicy sizePolicy((QSizePolicy::Policy)QSizePolicy::Minimum,(QSizePolicy::Policy)QSizePolicy::Fixed);
    sizePolicy.setHorizontalStretch(0);
    sizePolicy.setVerticalStretch(0);
    
    //Check Group
    QGroupBox* checkGroup = new QGroupBox("Manage Check");
    checkGroup->setSizePolicy(sizePolicy);
    QHBoxLayout* checkGroupLayout = new QHBoxLayout;
    checkGroupLayout->setSpacing(0);
    
    QToolButton* showSelectedAction = new QToolButton(dirView);
    showSelectedAction->setIcon(ApplicationData::icon("selected"));
    showSelectedAction->setIconSize(QSize(32, 32));
    showSelectedAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    showSelectedAction->setShortcut(tr("Ctrl+X"));
    showSelectedAction->setToolTip("Show checked images only\nShortcut: " + showSelectedAction->shortcut().toString(QKeySequence::NativeText));
    showSelectedAction->setCheckable(true);
    connect(showSelectedAction, SIGNAL(toggled(bool)), this, SLOT(showSelected(bool)));
    checkGroupLayout->addWidget(showSelectedAction);
    
    QToolButton *selectAllAction = new QToolButton(dirView);
    selectAllAction->setIcon(ApplicationData::icon("check_all"));
    selectAllAction->setIconSize(QSize(32, 32));
    selectAllAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    selectAllAction->setShortcut(tr("Ctrl+A"));
    selectAllAction->setToolTip("Check All Images\nShortcut: " + selectAllAction->shortcut().toString(QKeySequence::NativeText));
    connect(selectAllAction, SIGNAL(clicked()), dirModel, SLOT(selectAll()));
    checkGroupLayout->addWidget(selectAllAction);
    
    QToolButton *invertSelectedAction = new QToolButton(dirView);
    invertSelectedAction->setIcon(ApplicationData::icon("check_invert"));
    invertSelectedAction->setIconSize(QSize(32, 32));
    invertSelectedAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    invertSelectedAction->setShortcut(tr("Ctrl+I"));
    invertSelectedAction->setToolTip("Invert Check\nShortcut: " + invertSelectedAction->shortcut().toString(QKeySequence::NativeText));
    connect(invertSelectedAction, SIGNAL(clicked()), dirModel, SLOT(invertSelection()));
    checkGroupLayout->addWidget(invertSelectedAction);
    
    QToolButton* addSelectionAction = new QToolButton(dirView);
    addSelectionAction->setIcon(ApplicationData::icon("add_selection"));
    addSelectionAction->setIconSize(QSize(32, 32));
    addSelectionAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    addSelectionAction->setShortcut(tr("Ctrl+C"));
    addSelectionAction->setToolTip("Check highlighted images\nShortcut: " + addSelectionAction->shortcut().toString(QKeySequence::NativeText));
    connect(addSelectionAction, SIGNAL(clicked()), this, SLOT(extendSelection()));
    checkGroupLayout->addWidget(addSelectionAction);

    QToolButton* removeSelectionAction = new QToolButton(dirView);
    removeSelectionAction->setIcon(ApplicationData::icon("remove_selection"));
    removeSelectionAction->setIconSize(QSize(32, 32));
    removeSelectionAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    removeSelectionAction->setShortcut(tr("Ctrl+U"));
    removeSelectionAction->setToolTip("Uncheck highlighted images\nShortcut: " + removeSelectionAction->shortcut().toString(QKeySequence::NativeText));
    connect(removeSelectionAction, SIGNAL(clicked()), this, SLOT(reduceSelection()));
    checkGroupLayout->addWidget(removeSelectionAction);

    checkGroup->setLayout(checkGroupLayout);
    
    //Flag Group
    QGroupBox* flagGroup = new QGroupBox("Flag Images");
    flagGroup->setSizePolicy(sizePolicy);
    QHBoxLayout* flagGroupLayout = new QHBoxLayout;
    flagGroupLayout->setSpacing(0);
    
    QToolButton* noFlag = new QToolButton(dirView);
    noFlag->setToolTip("Remove flag from highlighted");
    noFlag->setIcon(ApplicationData::icon("flag_none"));
    noFlag->setIconSize(QSize(32, 32));
    noFlag->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(noFlag, &QToolButton::clicked, [=] () {flagSelection("none");});
    flagGroupLayout->addWidget(noFlag);
    
    QToolButton* redFlag = new QToolButton(dirView);
    redFlag->setToolTip("Flag highlighted to red");
    redFlag->setIcon(ApplicationData::icon("flag_red"));
    redFlag->setIconSize(QSize(32, 32));
    redFlag->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(redFlag, &QToolButton::clicked, [=] () {flagSelection("red");});
    flagGroupLayout->addWidget(redFlag);
    
    QToolButton* greenFlag = new QToolButton(dirView);
    greenFlag->setToolTip("Flag highlighted to green");
    greenFlag->setIcon(ApplicationData::icon("flag_green"));
    greenFlag->setIconSize(QSize(32, 32));
    greenFlag->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(greenFlag, &QToolButton::clicked, [=] () {flagSelection("green");});
    flagGroupLayout->addWidget(greenFlag);
    
    QToolButton* blueFlag = new QToolButton(dirView);
    blueFlag->setToolTip("Flag highlighted to blue");
    blueFlag->setIcon(ApplicationData::icon("flag_blue"));
    blueFlag->setIconSize(QSize(32, 32));
    blueFlag->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(blueFlag, &QToolButton::clicked, [=] () {flagSelection("blue");});
    flagGroupLayout->addWidget(blueFlag);
    
    QToolButton* goldFlag = new QToolButton(dirView);
    goldFlag->setToolTip("Flag highlighted to gold");
    goldFlag->setIcon(ApplicationData::icon("flag_gold"));
    goldFlag->setIconSize(QSize(32, 32));
    goldFlag->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(goldFlag, &QToolButton::clicked, [=] () {flagSelection("gold");});
    flagGroupLayout->addWidget(goldFlag);
    
    flagGroup->setLayout(flagGroupLayout);
    
    
    //Image Folder Group
    QGroupBox* imageFolderGroup = new QGroupBox("Organize Image Folder");
    imageFolderGroup->setSizePolicy(sizePolicy);
    QHBoxLayout* imageFolderGroupLayout = new QHBoxLayout;
    imageFolderGroupLayout->setSpacing(0);
    
    QToolButton* addFolderAction = new QToolButton(dirView);
    addFolderAction->setToolTip("Add Image Folder");
    addFolderAction->setIcon(ApplicationData::icon("add_folder"));
    addFolderAction->setIconSize(QSize(32, 32));
    addFolderAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(addFolderAction, SIGNAL(clicked()), this, SLOT(addImageFolder()));
    imageFolderGroupLayout->addWidget(addFolderAction);
    
    QToolButton* renameFolderAction = new QToolButton(dirView);
    renameFolderAction->setToolTip("Rename Image Folder");
    renameFolderAction->setIcon(ApplicationData::icon("rename"));
    renameFolderAction->setIconSize(QSize(32, 32));
    renameFolderAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(renameFolderAction, SIGNAL(clicked()), this, SLOT(renameImageFolder()));
    imageFolderGroupLayout->addWidget(renameFolderAction);
    
    QToolButton* moveImageAction = new QToolButton(dirView);
    moveImageAction->setToolTip("Move highlighted");
    moveImageAction->setIcon(ApplicationData::icon("move_selection"));
    moveImageAction->setIconSize(QSize(32, 32));
    moveImageAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(moveImageAction, SIGNAL(clicked()), this, SLOT(moveSelectiontoFolder()));
    imageFolderGroupLayout->addWidget(moveImageAction);
    
    QToolButton* trashImageAction = new QToolButton(dirView);
    trashImageAction->setToolTip("Trash highlighted");
    trashImageAction->setIcon(ApplicationData::icon("trash_selection"));
    trashImageAction->setIconSize(QSize(32, 32));
    trashImageAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(trashImageAction, SIGNAL(clicked()), this, SLOT(trashSelection()));
    imageFolderGroupLayout->addWidget(trashImageAction);
    
    QToolButton* copyImageAction = new QToolButton(dirView);
    copyImageAction->setToolTip("Copy to second project");
    copyImageAction->setIcon(ApplicationData::icon("copy_selection"));
    copyImageAction->setIconSize(QSize(32, 32));
    copyImageAction->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(copyImageAction, SIGNAL(clicked()), this, SLOT(copyImage()));
    imageFolderGroupLayout->addWidget(copyImageAction);
    
    imageFolderGroup->setLayout(imageFolderGroupLayout);
    
    //Auto Selection Group
    QGroupBox* autoSelectionGroup = new QGroupBox("Automatic Selection");
    
    minDegree = new QLineEdit;
    minDegree->setFrame(false);
    minDegree->setMaximumWidth(50);
    minDegree->setText("0");
    
    maxDegree = new QLineEdit;
    maxDegree->setFrame(false);
    maxDegree->setMaximumWidth(50);
    maxDegree->setText("10");
    
    parameterToUse = new QComboBox;
    parameterToUse->addItems(QStringList() << "tltang" << "tangl" << "QVAL2" << "QVALMA" << "QVALMB" );
    parameterToUse->setCurrentIndex(0);
    
    negPosOption = new QComboBox;
    negPosOption->addItems(QStringList() << "Yes" << "No");
    negPosOption->setCurrentIndex(0);
    
    noFlagged = new QCheckBox("Un-flagged");
    noFlagged->setChecked(true);
    redFlagged = new QCheckBox("Red");
    redFlagged->setChecked(true);
    greenFlagged = new QCheckBox("Green");
    greenFlagged->setChecked(true);
    blueFlagged = new QCheckBox("Blue");
    blueFlagged->setChecked(true);
    goldFlagged = new QCheckBox("Gold");
    goldFlagged->setChecked(true);
    
    QGridLayout* selectionFlagLayout = new QGridLayout;
    selectionFlagLayout->setMargin(0);
    selectionFlagLayout->setSpacing(2);
    selectionFlagLayout->addWidget(noFlagged, 0, 0, 1, 2);
    selectionFlagLayout->addWidget(redFlagged, 1, 0, 1, 1);
    selectionFlagLayout->addWidget(greenFlagged, 1, 1, 1, 1);
    selectionFlagLayout->addWidget(blueFlagged, 2, 0, 1, 1);
    selectionFlagLayout->addWidget(goldFlagged, 2, 1, 1, 1);
        
    QPushButton* changeSelection = new QPushButton("Change Selection");
    connect(changeSelection, SIGNAL(clicked()), this, SLOT(autoSelect()));
    
    QFormLayout* autoSelectLayout = new QFormLayout;
    autoSelectLayout->setRowWrapPolicy(QFormLayout::WrapLongRows);
    autoSelectLayout->setFieldGrowthPolicy(QFormLayout::FieldsStayAtSizeHint);
    autoSelectLayout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    autoSelectLayout->setLabelAlignment(Qt::AlignLeft);
    autoSelectLayout->addRow("Parameter to use", parameterToUse);
    autoSelectLayout->addRow("Minimum value", minDegree);
    autoSelectLayout->addRow("Maximum value", maxDegree);
    autoSelectLayout->addRow("Pos+neg values", negPosOption);
    autoSelectLayout->addRow("Include images", selectionFlagLayout);
    autoSelectLayout->addRow(changeSelection);
    
    autoSelectionGroup->setLayout(autoSelectLayout);
    
    
    //Backup/Save Group
    
    QGroupBox* backupSaveGroup = new QGroupBox("Save and Backup");
    backupSaveGroup->setSizePolicy(sizePolicy);
    QVBoxLayout* backupSaveLayout = new QVBoxLayout;
    
    QPushButton *saveDirectorySelectionAction = new QPushButton(ApplicationData::icon("check_save"), "Save checked list", this);
    connect(saveDirectorySelectionAction, SIGNAL(clicked()), this, SLOT(saveDirectorySelection()));
    backupSaveLayout->addWidget(saveDirectorySelectionAction);

    QPushButton *loadDirectorySelectionAction = new QPushButton(ApplicationData::icon("check_load"), "Load checked list", this);
    connect(loadDirectorySelectionAction, SIGNAL(clicked()), this, SLOT(loadDirectorySelection()));
    backupSaveLayout->addWidget(loadDirectorySelectionAction);
    
    backupSaveGroup->setLayout(backupSaveLayout);
    
    QWidget* tab = new QWidget;
    QVBoxLayout* layout = new QVBoxLayout;
    layout->setSpacing(10);
    layout->addStretch(0);
    //layout->addWidget(refreshAction, 0, Qt::AlignTop);
    //layout->addWidget(importAction, 0, Qt::AlignTop);
    layout->addWidget(backupSaveGroup, 0, Qt::AlignTop);
    layout->addWidget(checkGroup, 0, Qt::AlignTop);
    layout->addWidget(flagGroup, 0, Qt::AlignTop);
    layout->addWidget(imageFolderGroup, 0, Qt::AlignTop);
    layout->addWidget(autoSelectionGroup, 0);
    tab->setLayout(layout);
    return tab;
}

void LibraryTab::loadDirectorySelection() {
    QString loadName = QFileDialog::getOpenFileName(this, "Save Selection As...", projectData.projectWorkingDir().canonicalPath() + "/2dx_merge_dirfile.dat");
    loadSelection(loadName);
}

void LibraryTab::saveDirectorySelection() {
    QString saveName = QFileDialog::getSaveFileName(this, "Save Selection As...", projectData.projectWorkingDir().canonicalPath() + "/2dx_merge_dirfile.dat");
    if (QFileInfo(saveName).exists()) QFile::remove(saveName);
    QFile::copy(projectData.projectWorkingDir().canonicalPath() + "/2dx_merge_dirfile.dat", saveName);
}

ProjectModel* LibraryTab::getDirModel() {
    return dirModel;
}

QTreeView* LibraryTab::getDirView() {
    return dirView;
}

void LibraryTab::reload() {
    updateModel();
}

void LibraryTab::import() {
}

void LibraryTab::loadProjectState() {
    QString projectHeaderState = projectData.projectWorkingDir().canonicalPath() + "/config/projectHeaderState.dat";
    if (QFileInfo(projectHeaderState).exists()) {
        QFile f(projectHeaderState);
        if (!f.open(QIODevice::ReadOnly)) return;
        dirView->header()->restoreState(f.readAll());
        f.close();
    }
}

void LibraryTab::saveProjectState() {
    QString projectHeaderState = projectData.projectWorkingDir().canonicalPath() + "/config/projectHeaderState.dat";
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

void LibraryTab::copyImage() {
    QString secondDir = projectData.projectParameterData()->get("second_dir")->value().toString();

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
    QDir projectDir = projectData.projectDir();
    QString projectFolder = projectDir.canonicalPath();
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

void LibraryTab::renameImageFolder() {
    QString projectFolder = projectData.projectDir().canonicalPath();

    //Get the list of current folders
    QStringList imageFolders = QDir(projectFolder).entryList(QDir::NoDotAndDotDot | QDir::Dirs);

    //Remove non-linked folders
    QString imageFolder;
    foreach(imageFolder, imageFolders) {
        if (!QFile(projectFolder + "/" + imageFolder + "/2dx_master.cfg").exists()) imageFolders.removeAll(imageFolder);
    }

    //Ask for the folder to move to!
    bool ok2;
    QString folder = QInputDialog::getItem(this, tr("Folder selection"),
            tr("Select one of the following available folder to be renamed:"), imageFolders, 0, false, &ok2);

    if (ok2 && !folder.isEmpty()) {
        
        //Check if any image is already open
        QStringList imagesOpen = projectData.imagesOpen();
        foreach(QString im, imagesOpen) {
            if(im.split('/').contains(folder)) {
                QMessageBox::warning(
                    this,
                    tr("Warning: Unable to rename"),
                    "Unable to rename folder: " + projectFolder + "/" + folder + "\n" 
                    + "Images from this folder are  already open. Please close them and try again."
                    );
                return;
            }
        }
        
        bool ok;
        QString name = QInputDialog::getText(this, tr("New name"),
            tr("Enter the new name of the folder"), QLineEdit::Normal,
            folder, &ok);

        if (ok && !name.isEmpty()) {
            if(!QDir().rename(projectFolder + "/" + folder, projectFolder + "/" + name)) {
                QMessageBox::warning(
                    this,
                    tr("Warning: Unable to rename"),
                    "Unable to rename folder: " + projectFolder + "/" + folder + " to:\n"
                    + projectFolder + "/" + name
                    );
            }
            else {
                projectData.indexImages();
            }
        }
    }
}

void LibraryTab::flagSelection(const QString& color) {
    QModelIndexList selection = dirView->selectionModel()->selectedRows();
    QStringList sourcePaths;
    for(int ii=0; ii<selection.size();  ++ii) {
        QModelIndex i = selection[ii];
        if(dirModel->isRowValidImage(i)) sourcePaths.append(dirModel->pathFromIndex(i));
    }
    
    for(int ii=0; ii< sourcePaths.size(); ++ii) {   
        QString sourcePath = sourcePaths[ii];
        projectData.parameterData(QDir(sourcePath))->set("image_flag", color);
    }
}


void LibraryTab::moveSelectionToFolder(const QString& targetPath) {
    QModelIndexList selection = dirView->selectionModel()->selectedRows();
    QStringList sourcePaths;
    for(int ii=0; ii<selection.size();  ++ii) {
        QModelIndex i = selection[ii];
        if(dirModel->isRowValidImage(i)) sourcePaths.append(dirModel->pathFromIndex(i));
    }
    
    int numFiles = selection.count();
    int count = 0;
    QProgressDialog progress("Moving files...", "Abort Move", 0, numFiles, this);
    progress.setWindowModality(Qt::WindowModal);

    QString warnings;
    QString projectDir = projectData.projectDir().canonicalPath();
    
    for(int ii=0; ii< sourcePaths.size(); ++ii) {   
        progress.setValue(count++);
        if (progress.wasCanceled()) break;
        
        QString sourcePath = sourcePaths[ii];
        QString sourceImage = QFileInfo(sourcePath).fileName();
        
        //Check if the image is open.
        if(projectData.imageOpen(sourcePath)) {
            warnings += "<B>" + sourcePath.remove(projectDir) + "</B>: Image already open<br>";
            qDebug() << sourcePath << " Image already open";
            continue;
        }

        if (!QFile(sourcePath + "/2dx_image.cfg").exists()) {
            warnings += "<B>" + sourcePath.remove(projectDir) + "</B>: 2dx_image.cfg does not exist<br>";
            qDebug() << sourcePath << " 2dx_image.cfg does not exist";
            continue;
        }

        QDir targetImageDir = QDir(targetPath + "/" + sourceImage);

        if (targetImageDir.absolutePath() == QDir(sourcePath).absolutePath()) {
            warnings += "<B>" + sourcePath.remove(projectDir) + "</B>: Target same as source<br>";
            qDebug() << sourcePath << " Target same as source";
            continue;
        }

        bool target_exist = false;
        while (targetImageDir.exists()) {
            targetImageDir.setPath(targetImageDir.absolutePath() + "_1");
            target_exist = true;
        }

        if (target_exist) {
            warnings += "<B>" + sourcePath.remove(projectDir) + "</B>: Already exists at destination. Renaming it to: " + targetImageDir.absolutePath() + "<br>";
            qDebug() << sourcePath << " Already exists. Renaming it to: " + targetImageDir.absolutePath();
        }

        bool moved = QDir().rename(sourcePath, targetImageDir.absolutePath());
        if (!moved) {
            warnings += "<B>" + sourcePath.remove(projectDir) + "</B>: Unable to move image to " + targetImageDir.absolutePath() + "<br>";
            qDebug() << sourcePath << ": Unable to move image to " + targetImageDir.absolutePath();
            //targetImageDir.removeRecursively();
        } else {
            qDebug() << "Moved image: " + sourcePath + " to: " + targetImageDir.absolutePath();
            //dirModel->itemDeselected(sortModel->mapToSource(i));
            //QDir(sourcePath).removeRecursively();
        }
    }

    progress.setValue(numFiles);
    
    if(!warnings.isEmpty()) QMessageBox::warning(this, tr("Move Errors"), warnings);
    
    projectData.indexImages();

}

void LibraryTab::trashSelection() {
    if(QMessageBox::question(this,
			   tr("Move to trash?"),"Are you sure you want to move all the selected images to TRASH? \n\n Proceed?",
			   tr("Yes"),
			   tr("No"),
			   QString(),0,1) == 0){
    
        QString projectFolder = projectData.projectDir().canonicalPath();
        QDir projectDir(projectFolder);
        QString folder = "TRASH";
        if (!QFile(projectData.projectDir().canonicalPath() + "/" + folder + "/2dx_master.cfg").exists()) {
            if (!QDir(projectFolder + "/" + folder).exists()) {
                projectDir.mkdir(folder);
            }
            QFile().link(projectFolder + "/2dx_master.cfg", projectFolder + "/" + folder + "/2dx_master.cfg");
        }
        moveSelectionToFolder(projectData.projectDir().canonicalPath() + "/" + folder);
    }
}

void LibraryTab::moveSelectiontoFolder() {
    QString projectFolder = projectData.projectDir().canonicalPath();

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
    
    QStringList colorList;
    if(noFlagged->isChecked()) colorList << "none";
    if(redFlagged->isChecked()) colorList << "red";
    if(blueFlagged->isChecked()) colorList << "blue";
    if(greenFlagged->isChecked()) colorList << "green";
    if(goldFlagged->isChecked()) colorList << "gold";
    
    dirModel->autoSelect(minDegree->text().toInt(), maxDegree->text().toInt(), parameterToUse->currentText(), useAbsolute, colorList);
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
    mapPreview->loadFile(imagePath + "/final_map.mrc", "mrc", showHeaderButton->isChecked());
    refPreview->loadFile(imagePath + "/reference_map.mrc", "mrc", showHeaderButton->isChecked());
    dualPreview->loadFile(imagePath + "/half_half.mrc", "mrc", showHeaderButton->isChecked());
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

void LibraryTab::updateProjectName(const QString& name) {
    if(name.isEmpty()) projectNameLabel->setText(projectData.projectName());
    else projectNameLabel->setText(name);
    projectNameLabel->setToolTip(projectData.projectDir().canonicalPath());
}
