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

#include "ApplicationData.h"
#include "ProjectData.h"
#include "ProjectModel.h"

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
    columns[i]["shortname"] = "Merge";
    columns[i]["tooltip"] = "Merge";
    i++;
    columns[i]["visible"] = true;
    columns[i]["uid"] = "directory";
    columns[i]["shortname"] = "Directory";
    columns[i]["tooltip"] = "Directory";
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

void ProjectModel::loadImages(const QDir& currentDir, QStringList& imageList, QStandardItem *parent) {
    
    if(imageList.isEmpty()) return;
    
    QDir dir = currentDir;
    dir.setFilter(QDir::NoDotAndDotDot | QDir::Dirs);
    QString image = dir.relativeFilePath(imageList.first());
    if(image.isEmpty()) return;
    
    QString entry = image.split('/').first();
    if(entry.isEmpty()) return;
    
    if(image.contains("../")) {
        dir.cdUp();
        if(parent) loadImages(dir, imageList, parent->parent());
        return;
    }
    
    int curr = loadDialog->maximum() - imageList.size();
    int max = loadDialog->maximum();
    loadDialog->setValue(curr);
    loadDialog->setLabelText(QString("Loading image %1 of %2...\n" + QDir(projectPath).relativeFilePath(imageList.first())).arg(curr).arg(max));
    qApp->processEvents();
    
    QStandardItem *entryItem;

    dir.cd(entry);
    QString configFile = dir.canonicalPath() + "/2dx_image.cfg";
    QString masterFile = dir.canonicalPath() + "/2dx_master.cfg";

    if (QFileInfo(configFile).exists() || QFileInfo(masterFile).exists()) {
        
        uint uid = qHash(dir.canonicalPath());

        QList<QStandardItem*> entryItems;
        QStandardItem* include = new QStandardItem;
        items[uid][columns[0]["uid"].toString()] = include;
        loadImages(dir, imageList, include);
        entryItems << include << new QStandardItem(entry);

        foreach(entryItem, entryItems) {
            entryItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
            entryItem->setData(uid);
            entryItem->setData(entry, SortRole);
            entryItem->setData(dir.canonicalPath(), Qt::UserRole + 5);
        }

        if (QFileInfo(configFile).exists()) {
            ParametersConfiguration* localData = (projectData.parameterData(dir));

            foreach(quint32 c, columns.keys()) {
                if (!columns[c]["uid"].toString().isEmpty() && c != 0 && c != 1) {
                    entryItem = new QStandardItem;
                    entryItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
                    entryItem->setData(uid);
                    ParameterElementData* element = localData->get(columns[c]["uid"].toString());
                    QVariant value;
                    if (element) value = element->value();
                    fillData(c, entryItem, value);

                    connect(element, &ParameterElementData::dataChanged,
                            [ = ]{
                        QVariant value;
                        if (element) value = element->value();
                                fillData(c, entryItem, value);
                        });

                    entryItem->setData(dir.canonicalPath(), Qt::UserRole + 5);
                    if (columns[c]["lockable"] == "lockable")
                        entryItem->setCheckable(true);
                    items[uid][columns[c]["uid"].toString()] = entryItem;
                    entryItems << entryItem;
                }
            }
            
            imageList.removeFirst();
        }

        include->setCheckable(true);
        if (parent != 0) parent->appendRow(entryItems);
        else appendRow(entryItems);
        paths.insert(uid, dir.canonicalPath());
        
        loadImages(currentDir, imageList, parent);
    }
}

void ProjectModel::fillData(quint32 c, QStandardItem* entryItem, QVariant value) {
    QString entryString;
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
                if (format.contains(QRegExp("[diouxX]$")))
                    entryItem->setData(entries[0].toInt(), SortRole);
                else if (fwpl.contains(QRegExp("[eEfgG]$")))
                    entryItem->setData(entries[0].toFloat(), SortRole);
                else
                    entryItem->setData(entries[0], SortRole);
            } else if (fwpl.contains(QRegExp("[diouxX]$"))) {
                entryString = QString().sprintf(columns[c]["format"].toString().toLatin1(), value.toInt());
                entryItem->setData(value.toInt(), SortRole);
            } else if (fwpl.contains(QRegExp("[eEfgG]$"))) {
                entryString = QString().sprintf(columns[c]["format"].toString().toLatin1(), value.toDouble());
                entryItem->setData(value.toDouble(), SortRole);
            } else {
                entryString = value.toString();
                entryItem->setData(entryString, SortRole);
            }
        } else if (columns[c]["format"].toString().trimmed().toLower() == "checkbox") {
            entryString.clear();
            if (value.toString().trimmed().contains(QRegExp("^[yY]")))
                entryItem->setIcon(ApplicationData::icon("tick"));
            else
                entryItem->setIcon(ApplicationData::icon("cross"));
        } else if (columns[c]["format"].toString().trimmed().toLower() == "evenodd") {
            entryString.clear();
            entryItem->setData(value.toString().trimmed(), SortRole);
            if (value.toString().trimmed().contains(QRegExp("^[1]")))
                entryItem->setIcon(ApplicationData::icon("even"));
            else if (value.toString().trimmed().contains(QRegExp("^[2]")))
                entryItem->setIcon(ApplicationData::icon("odd"));
            else
                entryItem->setIcon(ApplicationData::icon("none"));
        } else
            entryString = value.toString();
    } else {
        entryString = value.toString();
    }
    
    entryItem->setText(entryString);
}

bool ProjectModel::loadSelection(const QString &fileName) {
    if (items.isEmpty()) return false;
    QString selectedFile = fileName, line;
    quint32 uid;
    QDir tempDir;
    QStandardItem *item = NULL;
    if (fileName.isEmpty()) selectedFile = saveFileName;

    QFile file(selectedFile);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) return false;

    clearSelection(false);

    disconnect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    while (!file.atEnd()) {
        line = file.readLine().trimmed();
        tempDir = QDir(line);
        //if absolute path is saved in the selection file
        if (tempDir.exists()) {
            uid = qHash(QDir(line).canonicalPath());
        }//or the relative path
        else {
            uid = qHash(QDir(projectPath + "/" + line).canonicalPath());
        }
        item = items[uid][columns[0]["uid"].toString()];
        if (item != NULL) item->setCheckState(Qt::Checked);
        else qDebug() << line << " not found.";
    }
    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    submit();
    file.close();
    return true;
}

ProjectModel::ProjectModel(const QString &path, const QString &columnsFile, QObject *parent)
: QStandardItemModel(parent) {
    resultData = NULL;

    loadDialog = new QProgressDialog();
    loadDialog->setWindowModality(Qt::WindowModal);
    loadDialog->setCancelButton(0);
    
    projectPath = QDir(path).canonicalPath();
    columnsDataFile = columnsFile;

    load();

    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(updateItems(QStandardItem *)));
    //connect(&watcher,SIGNAL(fileChanged(const QString &)),this,SLOT(confChanged(const QString &)));
}

void ProjectModel::load() {
    loadColumns(ApplicationData::configDir().canonicalPath() + "/projectMenu.inf");
    
    if (!QFileInfo(columnsDataFile).exists()) {
        qDebug() << "Initializing columns file: " << columnsDataFile;
        saveColumns(columnsDataFile);
    }
    
    loadHidden(columnsDataFile);
    
    QStringList imageList = projectData.imageList();
    prepareLoadDialog(imageList.size());
    loadImages(QDir(projectPath), imageList);
    loadDialog->reset();
    
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

    QStringList imageList = projectData.imageList();
    prepareLoadDialog(imageList.size());
    loadImages(QDir(projectPath), imageList);
    loadDialog->reset();

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
    return paths[index.sibling(index.row(), 0).data(Qt::UserRole + 1).toUInt()];
}

QString ProjectModel::relativePathFromIndex(const QModelIndex& index) {
    if (!index.isValid()) return QString();
    QString absolutePath = paths[index.sibling(index.row(), 0).data(Qt::UserRole + 1).toUInt()];
    return QDir(projectPath).relativeFilePath(absolutePath);
}

void ProjectModel::itemSelected(const QModelIndex &index) {
    changeItemCheckedRole(index, true);
}

void ProjectModel::itemDeselected(const QModelIndex &index) {
    changeItemCheckedRole(index, false);
}

void ProjectModel::changeItemCheckedRole(const QModelIndex &index, bool check) {
    QVariant checkedState;
    if (check)
        checkedState = Qt::Checked;
    else
        checkedState = Qt::Unchecked;

    if (index.isValid()) {

        if (index.child(0, 0).isValid()) {
            itemSelected(index.child(0, 0));
        } else if (index.column() == 0) {
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

void ProjectModel::setSaveName(const QString &saveName) {
    saveFileName = saveName;
}

void ProjectModel::setEvenImageFileName(const QString& name) {
    evenImageFileName = name;
}

void ProjectModel::setOddImageFileName(const QString& name) {
    oddImageFileName = name;
}

bool ProjectModel::save(QStandardItem *currentItem, int itemCount, QFile &saveFile, SaveOptions option) {
    QModelIndex i;
    if (currentItem == NULL) return false;

    foreach(i, match(currentItem->index(), Qt::CheckStateRole, QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked), itemCount, Qt::MatchRegExp)) {
        if (i.isValid()) {
            if (i.child(0, 0).isValid()) {
                save(itemFromIndex(i.child(0, 0)), itemFromIndex(i)->rowCount(), saveFile, option);
            } else {
                int evenodd = getRowParameterValue(i, "image_evenodd").toInt();
                //qDebug() << evenodd << saveFile.fileName();
                if (option == ALL || (option == EVEN_ONLY && evenodd == 1) || (option == ODD_ONLY && evenodd == 2)) {
                    //qDebug() << "Writing: " << relativePathFromIndex(i);
                    saveFile.write(QString(relativePathFromIndex(i) + '\n').toLatin1());
                }
            }
        }
    }
    return true;
}

bool ProjectModel::submit() {
    emit submitting();
    if (saveFileName.isEmpty()) return false;
    QFile saveFile(saveFileName);
    QFile evenFile(evenImageFileName);
    QFile oddFile(oddImageFileName);
    if (!saveFile.open(QIODevice::WriteOnly | QIODevice::Text)) return false;
    if (!evenFile.open(QIODevice::WriteOnly | QIODevice::Text)) return false;
    if (!oddFile.open(QIODevice::WriteOnly | QIODevice::Text)) return false;
    save(item(0), rowCount(), saveFile, ALL);
    save(item(0), rowCount(), evenFile, EVEN_ONLY);
    save(item(0), rowCount(), oddFile, ODD_ONLY);
    saveFile.close();
    evenFile.close();
    oddFile.close();
    return true;
}

void ProjectModel::updateItems(QStandardItem *element) {
    disconnect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(updateItems(QStandardItem *)));

    if (element->checkState() != Qt::PartiallyChecked)
        for (int i = 0; i < element->rowCount(); i++)
            element->child(i)->setCheckState(element->checkState());

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

    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(updateItems(QStandardItem *)));
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
    return index.sibling(index.row(), parameterToColId[parameter]).data(SortRole);
}

bool ProjectModel::isCurrentRowValidImage() {
    if (!QFileInfo(pathFromIndex(currentIndex) + "/2dx_image.cfg").exists() || pathFromIndex(currentIndex).isEmpty()) return false;
    else return true;
}

const QVariant &ProjectModel::getColumnProperty(int i, const QString &property) {
    return columns[i][property.toLower()];
}

void ProjectModel::setColumnProperty(int i, const QString &property, const QVariant &value) {
    columns[i][property.toLower()] = value;
}

void ProjectModel::setResultsFile(ResultsData *resultsFile) {
    if (resultData != NULL) disconnect(resultData, SIGNAL(saved(bool)), this, SLOT(update()));
    resultData = resultsFile;
    connect(resultData, SIGNAL(saved(bool)), this, SLOT(update()));
}

void ProjectModel::update() {
    if (resultData == NULL) return;

    QStandardItem *item;
    quint32 uid;

    QMapIterator<QString, QMap<QString, QString> > it(resultData->results);
    while (it.hasNext()) {
        it.next();

        uid = qHash(QDir(it.key()).canonicalPath());

        QMapIterator<QString, QString> j(it.value());
        while (j.hasNext()) {
            j.next();
            item = items[uid][j.key()];
            if (item != NULL && !j.key().contains(QRegExp("^##\\w*##$"))) {
                if (item->checkState() != Qt::Checked && item->checkState() != Qt::PartiallyChecked)
                    item->setText(j.value());
            }
        }
    }
}

void ProjectModel::maskResults() {
    resultData->clearMasked();
    maskResults(item(0), rowCount());
}

void ProjectModel::maskResults(QStandardItem *currentItem, int itemCount) {
    QModelIndex i, j;
    if (currentItem == NULL) return;

    foreach(i, match(currentItem->index(), Qt::CheckStateRole, ".*" /*QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked)*/, itemCount, Qt::MatchRegExp)) {
        if (i.isValid()) {
            if (i.child(0, 0).isValid()) maskResults(itemFromIndex(i.child(0, 0)), itemFromIndex(i)->rowCount());
            else
                for (int j = 2; j < columnCount(); j++)
                    if (i.parent().child(i.row(), j).data(Qt::CheckStateRole) == Qt::Checked || i.parent().child(i.row(), j).data(Qt::CheckStateRole) == Qt::PartiallyChecked)
                        resultData->setMasked(pathFromIndex(i), columns[j]["uid"].toString());
        }
    }
}

void ProjectModel::invertSelection(bool commit) {
    disconnect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    changeSelection(item(0), rowCount(), "invert");
    if (commit) submit();
    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
}

void ProjectModel::selectAll(bool commit) {
    disconnect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    changeSelection(item(0), rowCount(), "selectAll");
    if (commit) submit();
    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
}

void ProjectModel::clearSelection(bool commit) {
    disconnect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    changeSelection(item(0), rowCount(), "clear");
    if (commit) submit();
    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
}

bool ProjectModel::removeSelected() {
    disconnect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    changeSelection(item(0), rowCount(), "removeItems");
    //  submit();
    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    emit reloading();
    return true;
}

void ProjectModel::autoSelect(int minTilt, int maxTilt, const QString& param, bool useAbsolute) {
    disconnect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
    autoSelection(item(0), rowCount(), minTilt, maxTilt, param, useAbsolute);
    submit();
    connect(this, SIGNAL(itemChanged(QStandardItem *)), this, SLOT(submit()));
}

void ProjectModel::autoSelection(QStandardItem *currentItem, int itemCount, int minTilt, int maxTilt, const QString& param, bool useAbsolute) {
    if (currentItem == NULL) return;
    QList<QModelIndex> list = match(currentItem->index(), Qt::CheckStateRole, ".*", itemCount, Qt::MatchRegExp);

    foreach(QModelIndex i, list) {
        if (i.isValid()) {
            if (i.child(0, 0).isValid()) autoSelection(itemFromIndex(i.child(0, 0)), itemFromIndex(i)->rowCount(), minTilt, maxTilt, param, useAbsolute);
            else if (i.column() == 0) {
                QString confFile = pathFromIndex(i) + QString("/2dx_image.cfg");
                if (QFileInfo().exists(confFile)) {

                    int tiltAng = i.sibling(i.row(), parameterToColId[param]).data(SortRole).toInt();
                    if (!useAbsolute) {
                        if (tiltAng >= minTilt && tiltAng <= maxTilt) {
                            if (i.data(Qt::CheckStateRole) != Qt::Checked) setData(i, Qt::Checked, Qt::CheckStateRole);
                        } else {
                            if (i.data(Qt::CheckStateRole) == Qt::Checked) setData(i, Qt::Unchecked, Qt::CheckStateRole);
                        }
                    } else {
                        if (std::abs(tiltAng) >= minTilt && std::abs(tiltAng) <= maxTilt) {
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

QStringList ProjectModel::getSelectionNames() {
    QStringList selectedFiles;
    if (rowCount() != 0) {
        getSelection(item(0), rowCount(), selectedFiles);
    }
    return selectedFiles;
}

void ProjectModel::getSelection(QStandardItem *currentItem, int itemCount, QStringList & selected) {
    QModelIndex i;
    QList<QModelIndex> allItems = match(currentItem->index(), Qt::CheckStateRole, ".*" /*QString::number(Qt::Checked) + "|" + QString::number(Qt::PartiallyChecked) + "|" + QString::number(Qt::Unchecked)*/, itemCount, Qt::MatchRegExp);

    foreach(i, allItems) {
        if (i.isValid()) {
            if (i.child(0, 0).isValid()) {
                getSelection(itemFromIndex(i.child(0, 0)), itemFromIndex(i)->rowCount(), selected);
            } else if (i.column() == 0) {
                if (i.data(Qt::CheckStateRole) == Qt::Checked) {
                    QString name = i.data(Qt::DisplayRole).toString();
                    //QString name = item(i)->text();
                    selected << name;
                }
            }
        }
    }
}

QString ProjectModel::getProjectPath() const {
    return projectPath;
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

void ProjectModel::confChanged(const QString &path) {
    qDebug() << path;
}

void ProjectModel::prepareLoadDialog(int max) {
    loadDialog->reset();
    loadDialog->setMinimum(0);
    loadDialog->setMaximum(max);
    loadDialog->setWindowTitle("Loading images..");
    loadDialog->show();
}

