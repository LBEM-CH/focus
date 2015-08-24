#include "albumContainer.h"

albumContainer::albumContainer(confData *dat, resultsData* results, QWidget* parent) 
    : QWidget(parent)
{
    this->data = dat;
    
    preview = new imagePreview(data, "", false,this);
    
    setupDirectoryContainer(data);
    dirModel->setResultsFile(results);
    
    QWidget* widget = new QWidget(this);
    QGridLayout* layout = new QGridLayout(widget);
    layout->setMargin(0);
    layout->setSpacing(0);
    widget->setLayout(layout);
    
    layout->addWidget(dirView, 0, 0);
    layout->addWidget(preview, 0, 1, Qt::AlignTop);
    
    blockContainer* container = new blockContainer("Project Library");
    container->setMainWidget(widget);
    
    QToolButton* showHeaderButton = new QToolButton();
    showHeaderButton->setIcon(*(data->getIcon("info")));
    showHeaderButton->setToolTip("Show Image Header");
    showHeaderButton->setAutoRaise(false);
    showHeaderButton->setCheckable(true);
    connect(showHeaderButton, SIGNAL(toggled(bool)), preview, SLOT(showImageHeader(bool)));
    
    container->setHeaderWidget(showHeaderButton);
    
    QGridLayout* mainLayout = new QGridLayout(this);
    mainLayout->setSpacing(0);
    mainLayout->setMargin(0);
    mainLayout->addWidget(container);
    this->setLayout(mainLayout);
    
    
    
}

void albumContainer::showContents(bool show) 
{
    setVisible(show);
    QSize size = sizeHint();
    resize(size);
}

void albumContainer::setupDirectoryContainer(confData* data) 
{
    if (!QDir(data->getDir("project")).exists()) 
    {
        dirView = NULL;
        std::cerr << "The project directory does not exit\n";
    }
    
    QString projectDir = QDir(data->getDir("project")).canonicalPath();
    
    dirModel = new projectModel(data, projectDir, data->getDir("working") + "/config/" + "projectMenu.inf", this);

    const QString& savePath = data->getDir("working")+ "/2dx_merge_dirfile.dat";
    if (!savePath.isEmpty()) {
        dirModel->setSaveName(savePath);
        dirModel->loadSelection();
    }
    
    connect(dirModel, SIGNAL(currentImage(const QString&)), preview, SLOT(setImage(const QString&)));
    connect(dirModel, SIGNAL(reloading()), this, SLOT(reload()));

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


    //Right-Click Menu
    QAction *addSelectionAction;
    addSelectionAction = new QAction("add to selection", dirView);
    dirView->addAction(addSelectionAction);
    connect(addSelectionAction, SIGNAL(triggered(bool)), this, SLOT(extendSelection()));

    QAction *removeSelectionAction;
    removeSelectionAction = new QAction("remove from selection", dirView);
    dirView->addAction(removeSelectionAction);
    connect(removeSelectionAction, SIGNAL(triggered(bool)), this, SLOT(reduceSelection()));
    dirView->setContextMenuPolicy(Qt::ActionsContextMenu);

    QAction *copyImageAction;
    copyImageAction = new QAction("copy image to second project", dirView);
    dirView->addAction(copyImageAction);
    connect(copyImageAction, SIGNAL(triggered(bool)), this, SLOT(copyImage()));


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
    //  dirView->resize((int)width*(1.05),300);
    connect(dirView, SIGNAL(doubleClicked(const QModelIndex&)), dirModel, SLOT(itemActivated(const QModelIndex&)));
    //  container->resize(width,dirView->height());
}


projectModel* albumContainer::getDirModel() 
{
    return dirModel;
}

QTreeView* albumContainer::getDirView() 
{
    return dirView;
}

void albumContainer::reload() 
{
    maskResults();
    updateModel();
}


void albumContainer::loadProjectState() {
    QString projectHeaderState = data->getDir("working") + "/config/projectHeaderState.dat";
    if (QFileInfo(projectHeaderState).exists()) {
        QFile f(projectHeaderState);
        if (!f.open(QIODevice::ReadOnly)) return;
        dirView->header()->restoreState(f.readAll());
        f.close();
    }
}

void albumContainer::saveProjectState() {
    QString projectHeaderState = data->getDir("working") + "/config/projectHeaderState.dat";
    if (!projectHeaderState.isEmpty()) {
        QFile f(projectHeaderState);
        if (!f.open(QIODevice::WriteOnly)) return;
        f.write(dirView->header()->saveState());
        f.close();
    }
}

void albumContainer::showSelected(bool enable) {
    sortModel->setFilterRole(Qt::CheckStateRole);
    sortModel->setDynamicSortFilter(true);
    if (enable) {
        sortModel->setFilterRegExp((QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked)));
    } else {
        sortModel->setFilterRegExp(".*");
    }
}

bool albumContainer::loadSelection(const QString &fileName)
{
    return dirModel->loadSelection(fileName);
}

void albumContainer::updateModel() 
{
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

void albumContainer::maskResults() 
{
    dirModel->maskResults();
}

void albumContainer::copyImage() 
{
    QString secondDir = data->get("second_dir", "value");
    
    if(secondDir == "-" || secondDir == "")
    {
        std::cerr << "Error in copying images! Second project not set!\n";
        return;
    }
            
    QString targetDirPath = QString( secondDir + "/export/");
    
    
    QDir targetDir = QDir(targetDirPath);

    if (!targetDir.exists()) 
    {
        targetDir.mkpath(targetDirPath);
    }

    QModelIndex i;
    QModelIndexList selection = dirView->selectionModel()->selectedIndexes();

    foreach(i, selection) {
        QString sourcePath = dirModel->pathFromIndex(i);
        QFileInfo fi(sourcePath);
        QString imageDirName = fi.fileName();
        
        QDir sourceDir = QDir(sourcePath);
        QDir targetImageDir = QDir(targetDir.absolutePath() + "/" + imageDirName);
        
        dirModel->itemDeselected(sortModel->mapToSource(i));

        QStringList toBeCopied = sourceDir.entryList(QDir::Files);
        if(!targetImageDir.exists())
        {
            targetDir.mkdir(imageDirName);
            foreach(const QString& sourceFileName, toBeCopied)
            {
                QFile sourceFile(sourceDir.absolutePath() + "/" + sourceFileName);
                QString targetFileName = targetImageDir.absolutePath() + "/" + sourceFileName;
                if(!sourceFile.copy(targetFileName))
                    std::cerr << "Failed copying file" << sourceFile.fileName().toStdString() << " to " << targetFileName.toStdString() << "\n";
            
            }
        }
    }
}

void albumContainer::columnActivated(int i) 
{
    dirModel->setColumnProperty(i, "visible", dirModel->getColumnProperty(i, "visible").toBool()^true);
    dirModel->saveColumns();
    dirView->setColumnHidden(i, !dirModel->getColumnProperty(i, "visible").toBool());
}

void albumContainer::extendSelection() 
{
    modifySelection(true);
}

void albumContainer::reduceSelection() 
{
    modifySelection(false);
}

void albumContainer::modifySelection(bool select) 
{
    QModelIndex i;
    QModelIndexList selection = dirView->selectionModel()->selectedIndexes();

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
