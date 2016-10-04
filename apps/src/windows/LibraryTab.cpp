#include <QtWidgets>

#include "ProjectData.h"
#include "ApplicationData.h"
#include "LibraryTab.h"
#include "ProjectPreferences.h"
#include "GraphicalButton.h"

LibraryTab::LibraryTab(QWidget* parent)
: QWidget(parent) {
    
    previewContainer = setupPreviewContainer();
    previewContainer->setMaximumWidth(310);
    previewContainer->hide();
    
    autoSelectContainer = setupAutoSelectionTool();
    autoSelectContainer->hide();
    
    QGridLayout* mainLayout = new QGridLayout(this);
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addWidget(setupLibraryControls(), 0, 0, 1, 2);
    mainLayout->addWidget(autoSelectContainer, 1, 0, 1, 2);
    mainLayout->addWidget(dirView, 2, 0, 1, 1);
    mainLayout->addWidget(previewContainer, 2, 1, 1, 1, Qt::AlignTop);
    
    mainLayout->setColumnStretch(0, 1);
    mainLayout->setColumnStretch(1, 0);
    
    mainLayout->setRowStretch(0, 0);
    mainLayout->setRowStretch(1, 0);
    mainLayout->setRowStretch(2, 1);
    
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
    dirModel->loadSelectionList(projectData.imagesSelected());

    connect(dirModel, SIGNAL(currentImage(const QString&)), this, SLOT(setPreviewImages(const QString&)));
    connect(dirModel, SIGNAL(reloading()), this, SLOT(reload()));
    connect(&projectData, &ProjectData::selectionChanged, this, &LibraryTab::resetSelectionState);

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

QToolBar* LibraryTab::setupLibraryControls() {
    QToolBar* toolbar = new QToolBar(this);
    toolbar->setIconSize(QSize(24,24));
    
    //Check Group
    QAction* showSelectedAction = getLibraryToolBarAction("selected", "Show checked images only", "Ctrl+X", true);
    connect(showSelectedAction, SIGNAL(toggled(bool)), this, SLOT(showSelected(bool)));
    toolbar->addAction(showSelectedAction);
    
    QAction* selectAllAction = getLibraryToolBarAction("check_all", "Check All Images", "Ctrl+A", false);
    connect(selectAllAction, SIGNAL(triggered()), dirModel, SLOT(selectAll()));
    toolbar->addAction(selectAllAction);
    
    QAction *invertSelectedAction = getLibraryToolBarAction("check_invert", "Invert Check", "Ctrl+I", false);
    connect(invertSelectedAction, SIGNAL(triggered()), dirModel, SLOT(invertSelection()));
    toolbar->addAction(invertSelectedAction);
    
    QAction* addSelectionAction = getLibraryToolBarAction("add_selection", "Check highlighted images", "Ctrl+C", false);
    connect(addSelectionAction, SIGNAL(triggered()), this, SLOT(extendSelection()));
    toolbar->addAction(addSelectionAction);

    QAction* removeSelectionAction = getLibraryToolBarAction("remove_selection", "Un-check highlighted images", "Ctrl+U", false);
    connect(removeSelectionAction, SIGNAL(triggered()), this, SLOT(reduceSelection()));
    toolbar->addAction(removeSelectionAction);
    
    toolbar->addSeparator();
    
    
    //Flag Group
    QAction* noFlag = getLibraryToolBarAction("flag_none", "Remove flag from highlighted", "Ctrl+Shift+N", false);
    connect(noFlag, &QAction::triggered, [=] () {flagSelection("none");});
    toolbar->addAction(noFlag);
    
    QAction* redFlag = getLibraryToolBarAction("flag_red", "Flag highlighted to red", "Ctrl+Shift+R", false);
    connect(redFlag, &QAction::triggered, [=] () {flagSelection("red");});
    toolbar->addAction(redFlag);
    
    QAction* greenFlag = getLibraryToolBarAction("flag_green", "Flag highlighted to green", "Ctrl+Shift+G", false);
    connect(greenFlag, &QAction::triggered, [=] () {flagSelection("green");});
    toolbar->addAction(greenFlag);
    
    QAction* blueFlag = getLibraryToolBarAction("flag_blue", "Flag highlighted to blue", "Ctrl+Shift+B", false);
    connect(blueFlag, &QAction::triggered, [=] () {flagSelection("blue");});
    toolbar->addAction(blueFlag);
    
    QAction* goldFlag = getLibraryToolBarAction("flag_gold", "Flag highlighted to gold", "Ctrl+Shift+O", false);
    connect(goldFlag, &QAction::triggered, [=] () {flagSelection("gold");});
    toolbar->addAction(goldFlag);

    toolbar->addSeparator();
    
    //Image Folder Group
    QAction* addFolderAction = getLibraryToolBarAction("add_folder", "Add Image Folder", "Ctrl+Alt+F", false);
    connect(addFolderAction, SIGNAL(triggered()), this, SLOT(addImageFolder()));
    toolbar->addAction(addFolderAction);
    
    QAction* renameFolderAction = getLibraryToolBarAction("rename", "Rename Image Folder", "Ctrl+Alt+R", false);
    connect(renameFolderAction, SIGNAL(triggered()), this, SLOT(renameImageFolder()));
    toolbar->addAction(renameFolderAction);
    
    QAction* moveImageAction = getLibraryToolBarAction("move_selection", "Move highlighted", "Ctrl+Alt+M", false);
    connect(moveImageAction, SIGNAL(triggered()), this, SLOT(moveSelectiontoFolder()));
    toolbar->addAction(moveImageAction);
    
    QAction* trashImageAction = getLibraryToolBarAction("trash_selection", "Trash highlighted", "Ctrl+Alt+T", false);
    connect(trashImageAction, SIGNAL(triggered()), this, SLOT(trashSelection()));
    toolbar->addAction(trashImageAction);
    
    QAction* copyImageAction = getLibraryToolBarAction("copy_selection", "Copy to second project", "Ctrl+Alt+S", false);
    connect(copyImageAction, SIGNAL(triggered()), this, SLOT(copyImage()));
    toolbar->addAction(copyImageAction);
    
    toolbar->addSeparator();
    
    //Backup/Save Group
    QAction *saveDirectorySelectionAction = getLibraryToolBarAction("check_save", "Save checked list", "", false);
    connect(saveDirectorySelectionAction, SIGNAL(triggered()), this, SLOT(saveDirectorySelection()));
    toolbar->addAction(saveDirectorySelectionAction);

    QAction *loadDirectorySelectionAction = getLibraryToolBarAction("check_load", "Load checked list", "", false);
    connect(loadDirectorySelectionAction, SIGNAL(triggered()), this, SLOT(loadDirectorySelection()));
    toolbar->addAction(loadDirectorySelectionAction);
    
    toolbar->addSeparator();
    
    //Autoselect
    QToolButton* showAutoSelect = new QToolButton();
    showAutoSelect->setText("Auto Selection Tool");
    showAutoSelect->setCheckable(true);
    showAutoSelect->setChecked(false);
    connect(showAutoSelect, &QPushButton::toggled, [=](bool check){
       autoSelectContainer->setVisible(check); 
    });
    
    toolbar->addWidget(showAutoSelect);
    
    //Setup Project title and header
    selectionState = new QLabel(" ");
    selectionState->setAlignment(Qt::AlignCenter);
    QPalette pal = selectionState->palette();
    pal.setColor(QPalette::WindowText, Qt::darkGray);
    selectionState->setPalette(pal);
    resetSelectionState();
    
    QWidget* s = new QWidget();
    s->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    
    toolbar->addWidget(s);
    toolbar->addWidget(selectionState);
    
    return toolbar;
}

QAction* LibraryTab::getLibraryToolBarAction(const QString& ic, const QString& tooltip, const QString& shortcut, bool checkable) {
    QAction* action = new QAction(dirView);
    action->setIcon(ApplicationData::icon(ic));
    action->setCheckable(checkable);
    if(shortcut.isEmpty()) {
        action->setToolTip(tooltip);
    }
    else {
        action->setShortcut(shortcut);
        action->setToolTip(tooltip + "\nShortcut: " + action->shortcut().toString(QKeySequence::NativeText));
    }
    
    return action;
}

QWidget* LibraryTab::setupPreviewContainer() {
    QWidget* container = new QWidget();
    
    previews = new QStackedWidget(this);
    previews->setFixedSize(200, 200);

    GainCorrectedPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("GainCorrected<br>image<br>not found"), previews);
    GainCorrectedfftPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("FFT of<br>GainCorrected <br>image<br>not found"), previews);
    DriftCorrectedPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("DriftCorrected<br>image<br>not found"), previews);
    DriftCorrectedfftPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("FFT of<br>DriftCorrected <br>image<br>not found"), previews);
    rawPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("Raw image<br>not found"), previews);
    fftPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("Fourier image<br>not found"), previews);
    mapPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("Image not<br>processed"), previews);
    refPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("No reference<br>available"), previews);
    dualPreview = new ImageViewer(projectData.projectWorkingDir().canonicalPath(), QString("Half-Half map<br>not available"), previews);

    previews->addWidget(GainCorrectedPreview);
    previews->addWidget(GainCorrectedfftPreview);
    previews->addWidget(DriftCorrectedPreview);
    previews->addWidget(DriftCorrectedfftPreview);
    previews->addWidget(rawPreview);
    previews->addWidget(fftPreview);
    previews->addWidget(mapPreview);
    previews->addWidget(refPreview);
    previews->addWidget(dualPreview);

    setupDirectoryContainer();

    previewTimer = new QTimer(this);
    connect(previewTimer, SIGNAL(timeout()), this, SLOT(updatePreview()));

    
    QVBoxLayout* mainLayout = new QVBoxLayout();
    mainLayout->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
    mainLayout->setMargin(5);
    mainLayout->setSpacing(5);
    mainLayout->addStretch(0);
    
    previewLabel = new QLabel;
    QFont font = previewLabel->font();
    font.setBold(true);
    previewLabel->setFont(font);
    
    GraphicalButton* rightButton = new GraphicalButton(ApplicationData::icon("move_right"));
    rightButton->setFixedSize(42, 42);
    connect(rightButton, &GraphicalButton::clicked, [=]() {
        previews->setCurrentIndex((previews->currentIndex()+1)%(previews->count()));
        setPreviewLabelText();
    });
    
    GraphicalButton* leftButton = new GraphicalButton(ApplicationData::icon("move_left"));
    leftButton->setFixedSize(42, 42);
    connect(leftButton, &GraphicalButton::clicked, [=]() {
        previews->setCurrentIndex((previews->currentIndex()-1)%(previews->count()));
        setPreviewLabelText();
    });
    
    QHBoxLayout* changerLayout = new QHBoxLayout;
    changerLayout->setMargin(0);
    changerLayout->setSpacing(0);
    changerLayout->addWidget(leftButton, 0);
    changerLayout->addWidget(previews, 1);
    changerLayout->addWidget(rightButton, 0);
    
    previews->setCurrentIndex(0);
    setPreviewLabelText();
    
    showHeaderButton = new QToolButton();
    showHeaderButton->setIcon(ApplicationData::icon("header_info"));
    showHeaderButton->setText("Show Header");
    showHeaderButton->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
    showHeaderButton->setToolTip("Show Image Header");
    showHeaderButton->setCheckable(true);
    showHeaderButton->setChecked(false);
    connect(showHeaderButton, &QToolButton::toggled, [=] (bool) {
        setPreviewImages(dirModel->getCurrentRowPath());
    });
    
    QToolButton* autoPreviewsButton = new QToolButton();
    autoPreviewsButton->setIcon(ApplicationData::icon("overlay"));
    autoPreviewsButton->setText("Activate Overlay");
    autoPreviewsButton->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
    autoPreviewsButton->setToolTip("Auto switch previews");
    autoPreviewsButton->setCheckable(true);
    autoPreviewsButton->setChecked(false);
    connect(autoPreviewsButton, SIGNAL(toggled(bool)), this, SLOT(autoSwitch(bool)));
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->setSpacing(0);
    buttonLayout->setMargin(0);
    buttonLayout->addWidget(autoPreviewsButton);
    buttonLayout->addWidget(showHeaderButton);
    
    mainLayout->addWidget(previewLabel, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    mainLayout->addLayout(changerLayout, 0);
    mainLayout->addLayout(buttonLayout, 0);
    
    QFrame* vLine = new QFrame(this);
    vLine->setFrameStyle(QFrame::HLine | QFrame::Sunken);
    mainLayout->addWidget(vLine, 0);
    
    imageStatus = new LibraryImageStatus(dirModel);
    mainLayout->addWidget(imageStatus, 0, Qt::AlignHCenter | Qt::AlignVCenter);
    
    mainLayout->addStretch(1);
    
    container->setLayout(mainLayout);
    return container;
}

void LibraryTab::setPreviewLabelText() {
    if(previews->currentWidget() == GainCorrectedPreview) previewLabel->setText("GainCorrected Image");
    else if(previews->currentWidget() == GainCorrectedfftPreview) previewLabel->setText("FFT of GainCorrected Image");
    else if(previews->currentWidget() == DriftCorrectedPreview) previewLabel->setText("DriftCorrected Image");
    else if(previews->currentWidget() == DriftCorrectedfftPreview) previewLabel->setText("FFT of DriftCorrected Image");
    else if(previews->currentWidget() == rawPreview) previewLabel->setText("Raw Image");
    else if(previews->currentWidget() == fftPreview) previewLabel->setText("FFT of Raw Image");
    else if(previews->currentWidget() == mapPreview) previewLabel->setText("Final Map");
    else if(previews->currentWidget() == refPreview) previewLabel->setText("Reference Map");
    else if(previews->currentWidget() == dualPreview) previewLabel->setText("Half-Half Map");
    else previewLabel->setText("Preview");
}

QWidget* LibraryTab::setupAutoSelectionTool() {
    //Auto Selection Group
    QFrame* autoSelectionGroup = new QFrame();
    autoSelectionGroup->setFrameShadow(QFrame::Plain);
    autoSelectionGroup->setFrameShape(QFrame::StyledPanel);
    
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
    
    negPosOption = new QCheckBox("Ignore Sign (+/-)");
    negPosOption->setChecked(true);
    
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
    
    QPushButton* changeSelection = new QPushButton("Change Selection");
    connect(changeSelection, SIGNAL(clicked()), this, SLOT(autoSelect()));
    
    QHBoxLayout* selectionLayout = new QHBoxLayout;
    selectionLayout->setMargin(5);
    selectionLayout->setSpacing(5);
    selectionLayout->addStretch(0);
    selectionLayout->addWidget(new QLabel("Selection Parameter"));
    selectionLayout->addWidget(parameterToUse);
    selectionLayout->addSpacing(5);
    selectionLayout->addWidget(new QLabel("Range:"));
    selectionLayout->addWidget(minDegree);
    selectionLayout->addWidget(new QLabel("to"));
    selectionLayout->addWidget(maxDegree);
    selectionLayout->addSpacing(5);
    selectionLayout->addWidget(negPosOption);
    selectionLayout->addSpacing(5);
    selectionLayout->addWidget(noFlagged);
    selectionLayout->addWidget(redFlagged);
    selectionLayout->addWidget(greenFlagged);
    selectionLayout->addWidget(blueFlagged);
    selectionLayout->addWidget(goldFlagged);
    selectionLayout->addSpacing(10);
    selectionLayout->addWidget(changeSelection);
    selectionLayout->addStretch(1);
    
    autoSelectionGroup->setLayout(selectionLayout);
    
    return autoSelectionGroup;
}


void LibraryTab::loadDirectorySelection() {
    QString loadName = QFileDialog::getOpenFileName(this, "Save Selection As...", projectData.projectWorkingDir().canonicalPath() + "/2dx_merge_dirfile.dat");
    projectData.setImagesSelected(projectData.loadSelection(loadName));
}

void LibraryTab::saveDirectorySelection() {
    QString saveName = QFileDialog::getSaveFileName(this, "Save Selection As...", projectData.projectWorkingDir().canonicalPath() + "/2dx_merge_dirfile.dat");
    projectData.saveSelection(saveName);
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
    dirModel->loadSelectionList(projectData.imagesSelected());
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
    
    QStringList colorList;
    if(noFlagged->isChecked()) colorList << "none";
    if(redFlagged->isChecked()) colorList << "red";
    if(blueFlagged->isChecked()) colorList << "blue";
    if(greenFlagged->isChecked()) colorList << "green";
    if(goldFlagged->isChecked()) colorList << "gold";
    
    dirModel->autoSelect(minDegree->text().toInt(), maxDegree->text().toInt(), parameterToUse->currentText(), negPosOption->isChecked(), colorList);
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
    if(dirModel->isCurrentRowValidImage()) {
        previewContainer->show();
        imageStatus->updateData();
        ParametersConfiguration* imageConf = projectData.parameterData(QDir(imagePath));
        QString GainCorrectedImage, GainCorrectedfftImage, DriftCorrectedImage, DriftCorrectedfftImage, rawImage, fftImage;
        if(imageConf) {
            GainCorrectedImage = imageConf->getValue("raw_gaincorrectedstack") + ".mrc";
            GainCorrectedfftImage = imageConf->getValue("raw_gaincorrectedstack") + "_fft.mrc";
            DriftCorrectedImage = imageConf->getValue("movie_stackname") + ".mrc";
            DriftCorrectedfftImage = imageConf->getValue("movie_stackname") + "_fft.mrc";
            rawImage = imageConf->getValue("imagename") + ".mrc";
            fftImage = "FFTIR/" + imageConf->getValue("nonmaskimagename") + "_fft.mrc";
        }
        GainCorrectedPreview->loadFile(imagePath + "/" + GainCorrectedImage, "mrc", showHeaderButton->isChecked());
        GainCorrectedfftPreview->loadFile(imagePath + "/" + GainCorrectedfftImage, "mrc", showHeaderButton->isChecked());
        DriftCorrectedPreview->loadFile(imagePath + "/" + DriftCorrectedImage, "mrc", showHeaderButton->isChecked());
        DriftCorrectedfftPreview->loadFile(imagePath + "/" + DriftCorrectedfftImage, "mrc", showHeaderButton->isChecked());
        rawPreview->loadFile(imagePath + "/" + rawImage, "mrc", showHeaderButton->isChecked());
        fftPreview->loadFile(imagePath + "/" + fftImage, "mrc", showHeaderButton->isChecked());
        mapPreview->loadFile(imagePath + "/final_map.mrc", "mrc", showHeaderButton->isChecked());
        refPreview->loadFile(imagePath + "/reference_map.mrc", "mrc", showHeaderButton->isChecked());
        dualPreview->loadFile(imagePath + "/half_half.mrc", "mrc", showHeaderButton->isChecked());
    } else {
        previewContainer->hide();
    }
}

void LibraryTab::autoSwitch(bool play) {
    if (play) previewTimer->start(1000);
    else previewTimer->stop();
}

void LibraryTab::updatePreview() {
    int id = (previews->currentIndex() + 1) % 2;
    previews->setCurrentIndex(id + 2);
    setPreviewLabelText();
}

void LibraryTab::resetSelectionState() {
    QString checked = QString::number(projectData.imagesSelected().count());
    QString selected = QString::number(dirView->selectionModel()->selectedRows().count());
    selectionState->setText(checked + " checked and " + selected + " highlighted ");
}
