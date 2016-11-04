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

#ifndef PROJECTMODEL_H
#define PROJECTMODEL_H

#include <QStandardItemModel>
#include <QItemSelectionModel>
#include <QProcess>
#include <QDir>
#include <QFileDialog>
#include <QFileSystemWatcher>
#include <QCheckBox>
#include <QStyle>
#include <QDebug>
#include <QModelIndexList>
#include <QProgressDialog>

#include "ParameterConfiguration.h"
#include "ResultsData.h"
#include "ProjectImage.h"
#include "mrcImage.h"

class ProjectModel : public QStandardItemModel {

    Q_OBJECT

public:

    enum ProjectModelSortRole {
        SORT_ROLE = Qt::UserRole + 2
    };

    ProjectModel(const QString &columnsFile, QObject *parent = 0);

    QString pathFromIndex(const QModelIndex &index);
    QList<ProjectImage*> getSelectedImagePaths();
    QStringList parentDirs();
    QList<bool> visibleColumns();

    const QVariant &getColumnProperty(int i, const QString &property);
    void setColumnProperty(int i, const QString &property, const QVariant &value);

    QVariant getRowParameterValue(const QModelIndex &index, const QString& parameter);
    QVariant getCurrentRowParameterValue(const QString& parameter);
    QString getCurrentRowPath();
    bool isCurrentRowValidImage();
    bool isRowValidImage(const QModelIndex &index);

public slots:
    void itemSelected(const QModelIndex &index);
    void itemDeselected(const QModelIndex &index);
    void changeItemCheckedRole(const QModelIndex &index, bool check = true);
    void currentRowChanged(const QModelIndex&, const QModelIndex&);

    void onItemChangedSignal(QStandardItem *item);
    void saveAndUpdateItems();
    void saveCheckStates();
    void updateAllParentsCheckState();
    void updateParentsCheckState(QStandardItem *element);

    void reload();
    void addImage(ProjectImage* image);
    void moveImage(ProjectImage* image);

    void invertSelection(bool commit = true);
    void selectAll(bool commit = true);
    void modifySelection(const QModelIndexList& indexList, bool select = true);
    void changeSelection(QStandardItem *currentItem, int itemCount, const QString &action = QString());
    void autoSelection(QStandardItem *currentItem, int itemCount, int minTilt, int maxTilt, const QString& param, bool useAbsolute, const QStringList& flagList);
    void autoSelect(int minTilt, int maxTilt, const QString& param, bool useAbsolute, const QStringList& flagList);
    void loadSelectionList(const QList<ProjectImage*>& list);
    bool loadHidden(const QString &columnsFile);
    bool saveColumns();
    bool saveColumns(const QString &columnsFile);

signals:
    void currentImage(const QString&);
    void reloading();
    
private:

    bool loadColumns(const QString &columnsFile);
    void fillData(quint32 c, QStandardItem* entryItem, QVariant value);
    void load();
    
    
    uint uid(const QString & path);
    
    void prepareLoadDialog(int max);

    void getSelection(QStandardItem *currentItem, int itemCount, QList<ProjectImage*>& selected);
    
    QMap<quint32, QHash<QString, QVariant> > columns;
    QMap<QString, quint32> parameterToColId;
    QMap<QString, QStandardItem*> groupToItems;
    QMap<ProjectImage*, QStandardItem*> imageToItems;

    QString columnsDataFile;
    QStringList columnsFileHeader;
    QStringList labels;

    QModelIndex currentIndex;
    
    QProgressDialog* loadDialog;
};

#endif

