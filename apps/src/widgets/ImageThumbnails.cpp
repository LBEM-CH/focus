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
}

QString ImageThumbnails::getPath(int colId) {
    if (colId >= columnPaths.size()) return "";
    return columnPaths[colId];
}

QStandardItemModel* ImageThumbnails::getModel() {
    return model;
}

int ImageThumbnails::getSlectionCount() {
    return selectionCount;
}

void ImageThumbnails::updateThumbanils() {
    model->clear();
    columnPaths.clear();
    QStringList imageList = projectData.imageList();
    
    QProgressDialog progressDialog;
    progressDialog.setCancelButtonText(tr("&Cancel"));
    progressDialog.setRange(0, imageList.size());
    progressDialog.setWindowTitle(tr("Updating thumbnails..."));
    progressDialog.show();
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
        item->setCheckable(true);
        item->setEditable(false);
        columnPaths.append(imageDir);
        items.append(item);
    }

    QStandardItem* lastitem = new QStandardItem();
    lastitem->setEditable(false);
    lastitem->setSelectable(false);
    items << lastitem;

    model->appendRow(items);    

    for (int i = 0; i < model->columnCount() - 1; i++) resizeColumnToContents(i);
}


void ImageThumbnails::updateChecks(const QStringList& checkedImages) {
    int count = 0;
    for (int i = 0; i < columnPaths.size(); ++i) {
        if (checkedImages.contains(columnPaths[i])) {
            model->item(0, i)->setCheckState(Qt::Checked);
            count++;
        } else model->item(0, i)->setCheckState(Qt::Unchecked);
    }
    if (count != selectionCount) {
        selectionCount = count;
        emit selectionCountChanged(selectionCount);
    }
}

void ImageThumbnails::saveChecks() {
    QFile saveFile(projectData.selectionDirfile());
    if (!saveFile.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    int count = 0;
    for (int i = 0; i < columnPaths.size(); ++i) {
        if (model->item(0, i)->checkState() == Qt::Checked) {
            saveFile.write(QString(projectData.projectDir().relativeFilePath(columnPaths[i]) + '\n').toLatin1());
            count++;
        }
    }
    if (count != selectionCount) {
        selectionCount = count;
        emit selectionCountChanged(selectionCount);
    }
}

void ImageThumbnails::mousePressEvent(QMouseEvent* e) {
    QTreeView::mousePressEvent(e);
    selectionModel()->setCurrentIndex(indexAt(e->pos()), QItemSelectionModel::ClearAndSelect);
}

