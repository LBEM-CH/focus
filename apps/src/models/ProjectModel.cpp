/***************************************************************************
 *   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
 *   HStahlberg@ucdavis.edu                                                *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <cmath>
#include <fstream>

#include <QMessageBox>
#include <QDebug>
#include <QDateTime>

#include "ApplicationData.h"
#include "ProjectData.h"
#include "ProjectModel.h"
#include "ProcessDialog.h"

ProjectModel::ProjectModel(const QString &columnsFile, QObject *parent)
: QStandardItemModel(parent) {
    
    columnsDataFile = columnsFile;

    load();

    projectData.emitLibraryLoaded();
    
    connect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
    connect(&projectData, &ProjectData::selectionChanged, this, &ProjectModel::loadSelectionList);
}

bool ProjectModel::saveColumns(const QString &columnsFile) {
    QFile modelInfo(columnsFile);
    if (!modelInfo.open(QIODevice::WriteOnly | QIODevice::Text)) return false;

    foreach(QString line, columnsFileHeader) {
        line += '\n';
        modelInfo.write(line.toLatin1());
    }

    for (int i = 2; i < columns.size(); i++) {
        QHash<QString, QVariant> column = columns[i];
        QString line;

        if (column["visible"].toBool()) line += '*';

        line += column["uid"].toString() + ' ';
        line += '(';
        line += column["shortname"].toString() + ',';
        line += "mouseover=\"" + column["tooltip"].toString() + '\"' + ',';
        line += column["lockable"].toString() + ',';
        line += column["sortable"].toString();
        if (!column["format"].toString().isEmpty()) {
            line += ',';
            if (column["format"].toString().startsWith('%')) line += '"';
            line += column["format"].toString();
            if (column["format"].toString().startsWith('%')) line += '"';
        }
        line += ')';
        line += '\n';
        modelInfo.write(line.toLatin1());
    }
    modelInfo.close();
    return true;
}

bool ProjectModel::saveColumns() {
    return saveColumns(columnsDataFile);
}

bool ProjectModel::loadColumns(const QString &columnsFile) {
    QString line, parameters;
    QFile modelInfo(columnsFile);
    if (!modelInfo.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
    int i = 0;
    columns[i]["visible"] = true;
    columns[i]["uid"] = "merge";
    columns[i]["shortname"] = "Select";
    columns[i]["tooltip"] = "Add check to select the image or group";
    i++;
    columns[i]["visible"] = true;
    columns[i]["uid"] = "directory";
    columns[i]["shortname"] = "Directory";
    columns[i]["tooltip"] = "Physical Directory";
    i++;

    columnsFileHeader.clear();
    while (!modelInfo.atEnd()) {
        line = modelInfo.readLine().trimmed();
        if (line.contains(QRegExp("^\\s*#"))) {
            columnsFileHeader << line;
        } else if (line.contains(QRegExp("\\S+"))) {
            parameters = line;
            line.remove(QRegExp("\\(.*\\)"));

            if (line.contains("*")) {
                line.remove("*");
                columns[i]["visible"] = true;
            } else {
                columns[i]["visible"] = false;
            }

            columns[i]["uid"] = line.trimmed();
            parameterToColId[line.trimmed()] = i;
            if (parameters.contains(QRegExp("\\(.*\\)"))) {
                QRegExp pattern("\\(([^,]*),mouseover\\s*=\\s*\"?([^,\\\"]*)\"?,([^,]*),([^,]*)?(,.*)?\\)", Qt::CaseInsensitive);
                if (pattern.indexIn(parameters) == -1)
                    qDebug() << "Couldn't parse: " << parameters;

                columns[i]["shortname"] = pattern.cap(1).trimmed();
                columns[i]["tooltip"] = pattern.cap(2).trimmed();
                columns[i]["lockable"] = pattern.cap(3).trimmed();
                columns[i]["sortable"] = pattern.cap(4).trimmed();
                QString format = pattern.cap(5).trimmed().remove(QRegExp("^,|\\\""));

                columns[i]["format"] = format;
                if (columns[i]["shortname"].toString().isEmpty()) columns[i]["shortname"] = projectData.projectParameterData()->get(line.trimmed())->label().trimmed();
            } else
                columns[i]["shortname"] = projectData.projectParameterData()->get(line.trimmed())->label().trimmed();
            i++;
        }
    }
    modelInfo.close();

    return true;
}

bool ProjectModel::loadHidden(const QString &columnsFile) {
    QFile modelInfo(columnsFile);
    if (!modelInfo.open(QIODevice::ReadOnly | QIODevice::Text)) return false;
    QStringList visible, hidden;
    QString line;
    while (!modelInfo.atEnd()) {
        line = modelInfo.readLine().trimmed();
        line.remove(QRegExp("\\(.*\\)"));
        if (line.contains("*")) {
            line.remove('*');
            visible << line.trimmed();
        } else if (!line.trimmed().isEmpty()) {
            hidden << line.trimmed();
        }
    }

    QMutableMapIterator<quint32, QHash<QString, QVariant> > it(columns);
    while (it.hasNext()) {
        it.next();
        if (visible.contains(it.value()["uid"].toString()))
            it.value()["visible"] = true;

        if (hidden.contains(it.value()["uid"].toString()))
            it.value()["visible"] = false;

    }

    modelInfo.close();
    return true;
}

void ProjectModel::addGroup(const QString& group) {
    //Add the group item if it does not already exist
    if (!groupToItems.contains(group)) {
        QList<QStandardItem*> entryItems;
        
        //Group check item
        QStandardItem* groupItem = new QStandardItem();
        groupItem->setData(group, SORT_ROLE);
        groupToItems.insert(group, groupItem);
        
        //Dir item
        QStandardItem* directoryItem = new QStandardItem(group);
        directoryItem->setData(group, SORT_ROLE);
        entryItems << groupItem << directoryItem;
        for(QStandardItem* entryItem : entryItems) {
            entryItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
        }
        
        groupItem->setCheckable(true);
        appendRow(entryItems);
    }
}

void ProjectModel::moveImage(ProjectImage* image) {
    QStandardItem* it = imageToItems[image];
    if(it) {
        QString group = image->group();
        QString directory = image->directory();
        
        //Add the group item if it does not already exist
        addGroup(group);
        
        int newRow = groupToItems[group]->rowCount();
        groupToItems[group]->appendRow(it->parent()->takeRow(it->row()));
        groupToItems[group]->child(newRow, 0)->setData(group + "/" + directory, SORT_ROLE);
        groupToItems[group]->child(newRow, 1)->setText(directory);
        groupToItems[group]->child(newRow, 1)->setData(group + "/" + directory, SORT_ROLE);
    }
}

void ProjectModel::addImage(ProjectImage* image) {
    QString group = image->group();
    QString directory = image->directory();

    addGroup(group);

    //Create image items
    ParametersConfiguration* localData = image->parameters();

    QList<QStandardItem*> entryItems;
    QStandardItem* includeItem = new QStandardItem();
    includeItem->setData(group + "/" + directory, SORT_ROLE);
    QStandardItem* directoryItem = new QStandardItem(directory);
    directoryItem->setData(group + "/" + directory, SORT_ROLE);
    entryItems << includeItem << directoryItem;
    imageToItems.insert(image, includeItem);

    for(QStandardItem* entryItem : entryItems) {
        entryItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
    }
    
    includeItem->setCheckable(true);
    
    for(quint32 c : columns.keys()) {
        if (!columns[c]["uid"].toString().isEmpty() && c != 0 && c != 1) {
            QStandardItem* entryItem = new QStandardItem;
            entryItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
            ParameterElementData* element = localData->get(columns[c]["uid"].toString());
            QVariant value;
            if (element) value = element->value();
            fillData(c, entryItem, value);

            connect(element, &ParameterElementData::dataChanged,[ = ] {
                QVariant value;
                if (element) value = element->value();
                fillData(c, entryItem, value);
            });

            if (columns[c]["lockable"] == "lockable") entryItem->setCheckable(true);
            entryItems << entryItem;
        }
    }

    groupToItems[group]->appendRow(entryItems);
}

void ProjectModel::fillData(quint32 c, QStandardItem* entryItem, QVariant value) {
    QString entryString;
    QVariant sortValue;
    if (!entryItem) return;
    if (!columns[c]["format"].toString().isEmpty()) {
        if (columns[c]["format"].toString().contains(QRegExp("\\%\\S*[cdeEifgGosuxXpn]"))) {
            QString fwpl = columns[c]["format"].toString();
            fwpl.replace(".*(\\%\\S*[cdeEifgGosuxXpn]).*", "\\1");
            if (columns[c]["format"].toString().contains(',')) {
                QString format = columns[c]["format"].toString();
                QStringList formats = format.remove('"').split(',');
                QStringList entries = value.toString().split(',');
                entryString = "";
                int k = 0;

                foreach(QString entry, entries) {
                    if (k < entries.size() && k < formats.size()) {
                        format = formats[k];
                        if (format.contains(QRegExp("[diouxX]$")))
                            entryString += QString().sprintf(format.toLatin1(), entry.toInt());
                        else if (fwpl.contains(QRegExp("[eEfgG]$")))
                            entryString += QString().sprintf(format.toLatin1(), entry.toDouble());
                        else
                            entryString += entry;
                        k++;
                        if (k < entries.size()) entryString += ',';
                    }
                }
                if (format.contains(QRegExp("[diouxX]$"))) sortValue = entries[0].toInt();
                else if (fwpl.contains(QRegExp("[eEfgG]$"))) sortValue = entries[0].toFloat();
                else sortValue = entries[0];
            } else if (fwpl.contains(QRegExp("[diouxX]$"))) {
                entryString = QString().sprintf(columns[c]["format"].toString().toLatin1(), value.toInt());
                sortValue = value.toInt();
            } else if (fwpl.contains(QRegExp("[eEfgG]$"))) {
                entryString = QString().sprintf(columns[c]["format"].toString().toLatin1(), value.toDouble());
                sortValue = value.toDouble();
            } else {
                entryString = value.toString();
                sortValue = entryString;
            }
        } else if (columns[c]["format"].toString().trimmed().toLower() == "checkbox") {
            entryString.clear();
            if (value.toString().trimmed().contains(QRegExp("^[yY]")) && entryItem) entryItem->setIcon(ApplicationData::icon("tick"));
            else if(entryItem) entryItem->setIcon(ApplicationData::icon("cross"));
        } else if (columns[c]["format"].toString().trimmed().toLower() == "time") {
            entryString = value.toString();
            if (value.toString().trimmed() == "-" && entryItem) entryItem->setData(0, SORT_ROLE);
            else {
                QDateTime date = QDateTime::fromString(value.toString(), "dd.MM.yyyy hh:mm");
                sortValue = date.toMSecsSinceEpoch();
            }
        } else if (columns[c]["format"].toString().trimmed().toLower() == "evenodd") {
            entryString.clear();
            sortValue = value.toString().trimmed();
            if (value.toString().trimmed().contains(QRegExp("^[1]")) && entryItem) entryItem->setIcon(ApplicationData::icon("even"));
            else if (value.toString().trimmed().contains(QRegExp("^[2]")) && entryItem) entryItem->setIcon(ApplicationData::icon("odd"));
            else if(entryItem) entryItem->setIcon(ApplicationData::icon("none"));
        } else if (columns[c]["format"].toString().trimmed().toLower() == "flag") {
            entryString.clear();
            sortValue = value.toString().trimmed();
            if(entryItem) entryItem->setIcon(ApplicationData::icon("flag_" + value.toString().trimmed().toLower()));
        } else {
            entryString = value.toString();
        }
    } else {
        entryString = value.toString();
    }
    
    if(entryItem) {
        entryItem->setText(entryString);
        entryItem->setData(sortValue, SORT_ROLE);
    }
}

void ProjectModel::loadSelectionList(const QList<ProjectImage*>& list) {
    disconnect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
    for (ProjectImage* image: list) {
        if (imageToItems.contains(image)) imageToItems[image]->setCheckState(Qt::Checked);
    }
    updateAllParentsCheckState();
    connect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
}

void ProjectModel::load() {
    loadColumns(ApplicationData::configDir().canonicalPath() + "/projectMenu.inf");
    
    if (!QFileInfo(columnsDataFile).exists()) {
        qDebug() << "Initializing columns file: " << columnsDataFile;
        saveColumns(columnsDataFile);
    }
    
    loadHidden(columnsDataFile);
    
    QList<ProjectImage*> imageList = projectData.projectImageList();
    processDialog.setRange(0, imageList.size());
    processDialog.setWindowTitle("Initializing Images (3/3)");
    processDialog.show();
    int progress = 0;
    for(ProjectImage* image : imageList) {
        processDialog.setProgress(progress++);
        processDialog.setLabelText(QString("Putting image in library %1 of %2...").arg(progress).arg(imageList.size()));
        qApp->processEvents();
        addImage(image);
        processDialog.addStatusText("Loaded " + image->toString());
    }
    processDialog.reset();

    quint32 var;
    QStandardItem *item;

    foreach(var, columns.keys()) {
        item = new QStandardItem(columns[var]["shortname"].toString());
        item->setData(columns[var]["tooltip"], Qt::ToolTipRole);
        setHorizontalHeaderItem(var, item);
    }
}

void ProjectModel::reload() {
    clear();
    groupToItems.clear();
    imageToItems.clear();
    
    QList<ProjectImage*> imageList = projectData.projectImageList();
    processDialog.setRange(0, imageList.size());
    processDialog.setWindowTitle("Loading Library");
    processDialog.show();
    int progress = 0;
    for(ProjectImage* image : imageList) {
        processDialog.setProgress(progress++);
        processDialog.setLabelText(QString("Putting image in library %1 of %2...").arg(progress).arg(imageList.size()));
        qApp->processEvents();
        addImage(image);
        processDialog.addStatusText("Loaded " + image->toString());
    }
    processDialog.reset();

    quint32 var;
    QStandardItem *item;

    foreach(var, columns.keys()) {
        item = new QStandardItem(columns[var]["shortname"].toString());
        item->setData(columns[var]["tooltip"], Qt::ToolTipRole);
        setHorizontalHeaderItem(var, item);
    }
}

QString ProjectModel::pathFromIndex(const QModelIndex& index) {
    if (!index.isValid()) return QString();
    return projectData.projectDir().canonicalPath() + "/" + index.sibling(index.row(), 0).data(SORT_ROLE).toString();
}

void ProjectModel::itemSelected(const QModelIndex &index) {
    changeItemCheckedRole(index, true);
}

void ProjectModel::itemDeselected(const QModelIndex &index) {
    changeItemCheckedRole(index, false);
}

void ProjectModel::modifySelection(const QModelIndexList& indexList, bool select) {
    disconnect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
    foreach(QModelIndex i, indexList) {
        if(select) itemSelected(i);
        else itemDeselected(i);
    }
    saveAndUpdateItems();
    connect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
}

void ProjectModel::changeItemCheckedRole(const QModelIndex &index, bool check) {
    QVariant checkedState;
    if (check)
        checkedState = Qt::Checked;
    else
        checkedState = Qt::Unchecked;

    if (index.isValid()) {

        if (index.column() == 0) {
            if (index.data(Qt::CheckStateRole) != checkedState) {
                if (!setData(index, checkedState, Qt::CheckStateRole)) {
                    qDebug() << "Setting CheckStateRole did not work!";
                }
            }
        }
    }
}

QStringList ProjectModel::parentDirs() {
    QStringList dirs;
    for (int i = 0; i < rowCount(); i++)
        dirs << pathFromIndex(index(i, 0));
    return dirs;
}

void ProjectModel::saveAndUpdateItems() {
    updateAllParentsCheckState();
    saveCheckStates();
}

void ProjectModel::onItemChangedSignal(QStandardItem* item) {
    if(item->column() == 0) {
        updateParentsCheckState(item);
        saveCheckStates();
    }
}

void ProjectModel::saveCheckStates() {
    disconnect(&projectData, &ProjectData::selectionChanged, this, &ProjectModel::loadSelectionList);
    projectData.setImagesSelected(getSelectedImagePaths());
    connect(&projectData, &ProjectData::selectionChanged, this, &ProjectModel::loadSelectionList);
}

void ProjectModel::updateAllParentsCheckState() {
    for(int i=0; i< rowCount(); ++i) {
        QStandardItem* parent = item(i, 0);
        if(parent->rowCount() != 0) {
            bool checked = true;
            bool unchecked = true;
            for (int i = 0; i < parent->rowCount(); i++) {
                checked = checked && (parent->child(i)->checkState() == Qt::Checked);
                unchecked = unchecked && (parent->child(i)->checkState() == Qt::Unchecked);
            }

            if (checked && !unchecked) parent->setCheckState(Qt::Checked);
            else if (!checked && unchecked) parent->setCheckState(Qt::Unchecked);
            else parent->setCheckState(Qt::PartiallyChecked);
        }
    }
}

void ProjectModel::updateParentsCheckState(QStandardItem *element) {
    disconnect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
    if (element->checkState() != Qt::PartiallyChecked) {
        for (int i = 0; i < element->rowCount(); i++) element->child(i)->setCheckState(element->checkState());
    }
    
    QStandardItem *parent = element->parent();

    if (parent != NULL) {
        bool checked = true;
        bool unchecked = true;

        for (int i = 0; i < parent->rowCount(); i++) {
            checked = checked && (parent->child(i)->checkState() == Qt::Checked);
            unchecked = unchecked && (parent->child(i)->checkState() == Qt::Unchecked);
        }

        if (checked && !unchecked) parent->setCheckState(Qt::Checked);
        else if (!checked && unchecked) parent->setCheckState(Qt::Unchecked);
        else parent->setCheckState(Qt::PartiallyChecked);
    }
    connect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
}

QList<bool> ProjectModel::visibleColumns() {
    QList<bool> v;

    foreach(quint32 value, columns.keys()) {
        v << columns[value]["visible"].toBool();
    }
    return v;
}

QVariant ProjectModel::getCurrentRowParameterValue(const QString& parameter) {
    return getRowParameterValue(currentIndex, parameter);
}

QString ProjectModel::getCurrentRowPath() {
    return pathFromIndex(currentIndex);
}

QVariant ProjectModel::getRowParameterValue(const QModelIndex &index, const QString& parameter) {
    if (!index.sibling(index.row(), 1).isValid()) return QVariant();
    if (!parameterToColId.keys().contains(parameter)) return QVariant();
    return index.sibling(index.row(), parameterToColId[parameter]).data();
}

bool ProjectModel::isCurrentRowValidImage() {
    if (!QFileInfo(pathFromIndex(currentIndex) + "/2dx_image.cfg").exists() || pathFromIndex(currentIndex).isEmpty()) return false;
    else return true;
}

bool ProjectModel::isRowValidImage(const QModelIndex& index) {
    if (!QFileInfo(pathFromIndex(index) + "/2dx_image.cfg").exists() || pathFromIndex(index).isEmpty()) return false;
    else return true;
}


const QVariant &ProjectModel::getColumnProperty(int i, const QString &property) {
    return columns[i][property.toLower()];
}

void ProjectModel::setColumnProperty(int i, const QString &property, const QVariant &value) {
    columns[i][property.toLower()] = value;
}

void ProjectModel::invertSelection(bool commit) {
    disconnect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
    changeSelection(item(0), rowCount(), "invert");
    if (commit) saveAndUpdateItems();
    connect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
}

void ProjectModel::selectAll(bool commit) {
    disconnect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
    changeSelection(item(0), rowCount(), "selectAll");
    if (commit) saveAndUpdateItems();
    connect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
}

void ProjectModel::autoSelect(int minTilt, int maxTilt, const QString& param, bool useAbsolute, const QStringList& flagList) {
    disconnect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
    autoSelection(item(0), rowCount(), minTilt, maxTilt, param, useAbsolute, flagList);
    saveAndUpdateItems();
    connect(this, &QStandardItemModel::itemChanged, this, &ProjectModel::onItemChangedSignal);
}

void ProjectModel::autoSelection(QStandardItem *currentItem, int itemCount, int minTilt, int maxTilt, const QString& param, bool useAbsolute, const QStringList& flagList) {
    if (currentItem == NULL) return;
    QList<QModelIndex> list = match(currentItem->index(), Qt::CheckStateRole, ".*", itemCount, Qt::MatchRegExp);

    foreach(QModelIndex i, list) {
        if (i.isValid()) {
            if (i.child(0, 0).isValid()) autoSelection(itemFromIndex(i.child(0, 0)), itemFromIndex(i)->rowCount(), minTilt, maxTilt, param, useAbsolute, flagList);
            else if (i.column() == 0) {
                QString confFile = pathFromIndex(i) + QString("/2dx_image.cfg");
                if (QFileInfo().exists(confFile)) {

                    int tiltAng = i.sibling(i.row(), parameterToColId[param]).data(SORT_ROLE).toInt();
                    QString color = i.sibling(i.row(), parameterToColId["image_flag"]).data(SORT_ROLE).toString();
                    if (!useAbsolute) {
                        if (tiltAng >= minTilt && tiltAng <= maxTilt && flagList.contains(color)) {
                            if (i.data(Qt::CheckStateRole) != Qt::Checked) setData(i, Qt::Checked, Qt::CheckStateRole);
                        } else {
                            if (i.data(Qt::CheckStateRole) == Qt::Checked) setData(i, Qt::Unchecked, Qt::CheckStateRole);
                        }
                    } else {
                        if (std::abs(tiltAng) >= minTilt && std::abs(tiltAng) <= maxTilt && flagList.contains(color)) {
                            if (i.data(Qt::CheckStateRole) != Qt::Checked) setData(i, Qt::Checked, Qt::CheckStateRole);
                        } else {
                            if (i.data(Qt::CheckStateRole) == Qt::Checked) setData(i, Qt::Unchecked, Qt::CheckStateRole);
                        }
                    }
                }
            }
        }
    }
}

void ProjectModel::changeSelection(QStandardItem *currentItem, int itemCount, const QString &action) {
    if (currentItem == NULL) return;
    QModelIndex i;
    QString checkAction = action.trimmed().toLower();
    QList<QModelIndex> list = match(currentItem->index(), Qt::CheckStateRole, ".*" /*QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked) + "|" + QString::number(Qt::Unchecked)*/, itemCount, Qt::MatchRegExp);

    foreach(i, list) {
        if (i.isValid()) {
            if (i.child(0, 0).isValid()) changeSelection(itemFromIndex(i.child(0, 0)), itemFromIndex(i)->rowCount(), action);
            else if (i.column() == 0) {
                if (checkAction == "invert") {
                    if (i.data(Qt::CheckStateRole) == Qt::Checked) setData(i, Qt::Unchecked, Qt::CheckStateRole);
                    else if (i.data(Qt::CheckStateRole) == Qt::Unchecked) setData(i, Qt::Checked, Qt::CheckStateRole);
                } else if (checkAction == "selectall") {
                    if (i.data(Qt::CheckStateRole) != Qt::Checked) setData(i, Qt::Checked, Qt::CheckStateRole);
                } else if (checkAction == "extend") {
                    if (i.data(Qt::CheckStateRole) != Qt::Checked) setData(i, Qt::Checked, Qt::CheckStateRole);
                } else if (checkAction == "clear")
                    setData(i, Qt::Unchecked, Qt::CheckStateRole);
                else if (checkAction == "removeitems") {
                    if (i.data(Qt::CheckStateRole) == Qt::Checked) {
                        QString path = pathFromIndex(i);
                        qDebug() << "Removing: " << path;
                        QProcess p;
                        p.setStandardOutputFile(projectData.projectWorkingDir().absolutePath() + "/config/out.out");
                        p.setStandardErrorFile(projectData.projectWorkingDir().absolutePath() + "/config/out.err");
                        p.start("rm -rf " + path);
                        p.waitForFinished(30000);
                    }
                }
            }
        }
    }
}

QList<ProjectImage*> ProjectModel::getSelectedImagePaths() {
    QList<ProjectImage*> selectedFiles;
    if (rowCount() != 0) {
        getSelection(item(0), rowCount(), selectedFiles);
    }
    return selectedFiles;
}

void ProjectModel::getSelection(QStandardItem *currentItem, int itemCount, QList<ProjectImage*>& selected) {
    QModelIndex i;
    QList<QModelIndex> allItems = match(currentItem->index(), Qt::CheckStateRole, QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked), itemCount, Qt::MatchRegExp);

    foreach(i, allItems) {
        if (i.isValid()) {
            if (i.child(0, 0).isValid()) {
                getSelection(itemFromIndex(i.child(0, 0)), itemFromIndex(i)->rowCount(), selected);
            } else {
                if(isRowValidImage(i)) {
                    ProjectImage* image = projectData.projectImage(QDir(pathFromIndex(i)));
                    if(image) selected << image;
                }
            }
        }
    }
}

void ProjectModel::currentRowChanged(const QModelIndex&i, const QModelIndex&) {
    currentIndex = i;
    if (!i.sibling(i.row(), 1).isValid()) return;
    //QString image = pathFromIndex(i) + "/" + i.sibling(i.row(),1).data().toString() + "-p1.mrc";
    QString image = pathFromIndex(i);

    //if selected row is a directory of images then:
    if (QFileInfo(pathFromIndex(i) + "/2dx_master.cfg").exists()) image = "";

    emit currentImage(image);
}

