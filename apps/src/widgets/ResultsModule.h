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

#ifndef RESULTSMODULE_H
#define RESULTSMODULE_H

#include <QGridLayout>
#include <QProcess>
#include <QTreeWidget>
#include <QHeaderView>
#include <QStandardItem>
#include <QDebug>

#include "ResultsData.h"
#include "Translator.h"

class ResultsModule : public QWidget {

    Q_OBJECT

public:
    
    enum Type {
        results, images
    };

    ResultsModule(const QString& workDir, ResultsData *resultsInfo, ResultsModule::Type moduleType = ResultsModule::results, QWidget *parent = NULL);
    QString selectedImagePath();
    QString selectedImageExtenstion();

public slots:
    void load();
    void itemSelected(QTreeWidgetItem *item);
    void itemActivated(QTreeWidgetItem *item);
    void setImportant(int);
    void setImportant(bool);
    void setShowFilenames(int);
    void resetColumnSize();

signals:
    void resultChanged(const QString& path, const QString& extention);

private:
    QString workingDir;
    ResultsData *data;
    QTreeWidget *view;
    Type viewType;

    bool showImportant, showFileNames;

    Translator *editor;

    QHash<QTreeWidgetItem*, QStringList> fullPath; //Contains a list of fullpaths and extensions for each entry

};

#endif
