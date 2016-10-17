#include "ImageThumbnails.h"

ImageThumbnails::ImageThumbnails(QWidget* parent)
: QTreeView(parent) {
    setHeaderHidden(true);
    setSortingEnabled(false);
    setIconSize(QSize(64, 64));
    setAttribute(Qt::WA_MacShowFocusRect, 0);

    QPalette pal = palette();
    pal.setColor(QPalette::Highlight, Qt::darkCyan);
    setPalette(pal);
    
    model = new QStandardItemModel();
    setModel(model);
    updateThumbanils();
    
    connect(&projectData, &ProjectData::imageDirsChanged, [=] () {
        updateThumbanils();
    });
    
    connect(model, &QStandardItemModel::itemChanged, this, &ImageThumbnails::saveChecks);
    connect(&projectData, &ProjectData::selectionChanged, this, &ImageThumbnails::updateChecks);
}

QString ImageThumbnails::getPath(int colId) {
    if (colId >= columnPaths.size()) return "";
    return columnPaths[colId];
}

QStandardItemModel* ImageThumbnails::getModel() {
    return model;
}

void ImageThumbnails::updateThumbanils() {
    disconnect(model, &QStandardItemModel::itemChanged, this, &ImageThumbnails::saveChecks);
    model->clear();
    columnPaths.clear();
    QStringList imageList = projectData.imageList();
    QStringList selectedList = projectData.imagesSelected();
    
    QProgressDialog progressDialog;
    progressDialog.setCancelButtonText(tr("&Cancel"));
    progressDialog.setRange(0, imageList.size());
    progressDialog.setWindowTitle(tr("Updating thumbnails..."));
    int saveProgress = 0;
    
    QList<QStandardItem*> items;
    for (int i = 0; i < imageList.size(); ++i) {
        saveProgress++;
        progressDialog.setValue(saveProgress);
        progressDialog.setLabelText(tr("Processing image %1 of %2...").arg(saveProgress).arg(imageList.size()));
        qApp->processEvents();

        if (progressDialog.wasCanceled()) break;
        
        QIcon icon;
        QString imageDir = imageList[i];
        if (QFileInfo(imageDir + "/final_map.mrc").exists()) {
            mrcImage image(imageDir + "/final_map.mrc", true);
            icon.addPixmap(image.getPixmap());
        } else {
            icon = ApplicationData::icon("default_preview");
        }

        QStandardItem* item = new QStandardItem();
        item->setIcon(icon);
        item->setToolTip(projectData.projectDir().relativeFilePath(imageDir));
        item->setEditable(false);
        item->setCheckable(true);
        if(selectedList.contains(imageDir)) item->setCheckState(Qt::Checked);

        columnPaths.append(imageDir);
        items.append(item);
    }

    QStandardItem* lastitem = new QStandardItem();
    lastitem->setEditable(false);
    lastitem->setSelectable(false);
    items << lastitem;

    model->appendRow(items);    

    progressDialog.reset();
    progressDialog.close();
    
    for (int i = 0; i < model->columnCount() - 1; i++) resizeColumnToContents(i);
    connect(model, &QStandardItemModel::itemChanged, this, &ImageThumbnails::saveChecks);
}


void ImageThumbnails::updateChecks(const QStringList& checkedImages) {
    disconnect(model, &QStandardItemModel::itemChanged, this, &ImageThumbnails::saveChecks);
    for (int i = 0; i < columnPaths.size(); ++i) {
        if (checkedImages.contains(columnPaths[i])) {
            model->item(0, i)->setCheckState(Qt::Checked);
        } else model->item(0, i)->setCheckState(Qt::Unchecked);
    }
    connect(model, &QStandardItemModel::itemChanged, this, &ImageThumbnails::saveChecks);
}

void ImageThumbnails::saveChecks() {
    disconnect(&projectData, &ProjectData::selectionChanged, this, &ImageThumbnails::updateChecks);
    
    QStringList checkedImages;
    for (int i = 0; i < columnPaths.size(); ++i) {
        if (model->item(0, i)->checkState() == Qt::Checked) {
            checkedImages << columnPaths[i];
        }
    }
    projectData.setImagesSelected(checkedImages);

    connect(&projectData, &ProjectData::selectionChanged, this, &ImageThumbnails::updateChecks);
}

void ImageThumbnails::mousePressEvent(QMouseEvent* e) {
    QTreeView::mousePressEvent(e);
    selectionModel()->setCurrentIndex(indexAt(e->pos()), QItemSelectionModel::ClearAndSelect);
}

