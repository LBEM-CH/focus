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

#include <QApplication>
#include <QDesktopWidget>
#include <QDebug>

#include "ProjectData.h"
#include "ResultsParser.h"

ResultsParser::ResultsParser(const QString& workDir, QStringList results, ResultsParser::type viewType, QWidget *parent)
: QTableWidget(parent) {
    if (viewType == ResultsParser::images) {
        QAction *openFileAction = new QAction(tr("Open File"), this);
        connect(openFileAction, SIGNAL(triggered()), this, SLOT(openFile()));
        addAction(openFileAction);
        setContextMenuPolicy(Qt::ActionsContextMenu);
    }
    //  int fixedWidth = int(QApplication::desktop()->width()/5.00);
    //  if(fixedWidth > 235) fixedWidth = 235;
    workingDir = workDir;
    resultsList = results;
    view = viewType;
    updateFontInfo();
    QPalette pal(palette());
    pal.setBrush(QPalette::Highlight, QColor(187, 224, 227));
    pal.setBrush(QPalette::HighlightedText, QColor(0, 0, 0));
    //  pal.setBrush(QPalette::Base, QColor(173,217,217));
    setPalette(pal);
    important = false;
    showFilenames = false;
    setWordWrap(false);

    setSelectionMode(QAbstractItemView::SingleSelection);

    //  setFixedWidth(fixedWidth);
    setShowGrid(false);
    //  setTextElideMode(Qt::ElideNone);


    verticalHeader()->hide();
    horizontalHeader()->hide();
    //  if(viewType == resultsParser::results)
    //    setHorizontalHeaderLabels(QStringList()<<"Parameter"<<"Value");
    setContentsMargins(0, 0, 0, 0);
    setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
    //  if(viewType == images) setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
    connect(this, SIGNAL(currentItemChanged(QTableWidgetItem*, QTableWidgetItem*)), this, SLOT(selectImage(QTableWidgetItem*, QTableWidgetItem*)));
    connect(&watcher, SIGNAL(fileChanged(const QString &)), this, SLOT(load()));
    connect(&watcher, SIGNAL(directoryChanged(const QString &)), this, SLOT(refreshWatcher()));
}

ResultsParser::~ResultsParser() {
}

int ResultsParser::loadResults(ResultsParser::type viewType) {
    output.clear();
    for (int i = 0; i < resultsList.size(); i++) {
        if (viewType == initialization) {
            if (QFileInfo(resultsList[i]).exists()) {
                QTableWidgetItem *item;
                if (!titles.contains(resultsList[i])) {
                    titles << resultsList[i];
                    item = new QTableWidgetItem(resultsList[i]);
                    item->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
                    item->setFlags(QFlag(Qt::ItemIsEnabled | Qt::ItemIsUserCheckable | Qt::ItemIsSelectable));
                    //          item->setFont(font());
                    output << item;
                }
            }
        } else {
            QFile data(resultsList[i]);
            if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) return 0;
            QTextStream in(&data);
            QString line;
            QTableWidgetItem *item;

            while (!in.atEnd()) {
                line = in.readLine();
                QRegExp var("^\\s*set\\s*(\\S+)\\s*=\\s*\\\"{0,1}(.*)\\\"{0,1}");
                QRegExp lock("^\\s*#\\s(un)?lock\\s(.*)$", Qt::CaseInsensitive);
                var.setCaseSensitivity(Qt::CaseInsensitive);
                if (viewType == results && (var.indexIn(line) != -1 || lock.indexIn(line) != -1)) {
                    QString variable;
                    QString value;
                    QColor textColor(Qt::black);
                    if (var.indexIn(line) != -1) {
                        variable = var.cap(1).trimmed();
                        value = var.cap(2).simplified();
                        if (projectData.parameterData(QDir(workingDir))->get(variable) 
                                && projectData.parameterData(QDir(workingDir))->get(variable)->locked()) textColor = QColor(198, 25, 10);
                    } else if (lock.indexIn(line) != -1) {
                        bool lockVar = lock.cap(1).toLower() == "un";
                        variable = lock.cap(1).toLower() + "lock";
                        value = lock.cap(2).trimmed();
                    }

                    item = new QTableWidgetItem(variable);
                    item->setFlags(Qt::ItemIsEnabled);
                    item->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
                    item->setTextColor(textColor);
                    item->setToolTip(variable + " " + value);
                    //            item->setFont(font());
                    output << item;
                    item = new QTableWidgetItem(value);
                    item->setFlags(QFlag(Qt::ItemIsEnabled));
                    item->setTextAlignment(Qt::AlignRight | Qt::AlignVCenter);
                    item->setTextColor(textColor);
                    item->setToolTip(variable + " " + value);
                    //            item->setFont(font());
                    output << item;
                }
                if (viewType == images && line.toLower().contains(QRegExp("^\\s*#?\\s*image.*:"))) {
                    bool imageImportant = line.contains(QRegExp("image-important\\s*:", Qt::CaseInsensitive));
                    if ((important && imageImportant) || !important) {
                        //QStringList cell = line.split(':');
                        QString fileName = line.section(':', 1, -1); //ell[1].simplified();
                        QString itemText = fileName.trimmed().replace(QRegExp("^.*\\<(.*)\\>.*$"), "\\1");
                        QString toolTipText;
                        fileName = fileName.remove(QRegExp("\\<.*\\>")).trimmed();
                        if (QFileInfo(workingDir + fileName).exists() && !titles.contains(fileName)) {
                            titles << fileName;
                            if (line.contains(QRegExp("\\<.*\\>")) && !showFilenames) {
                                toolTipText = fileName;
                            } else {
                                if (!itemText.isEmpty()) toolTipText = itemText;
                                else toolTipText = fileName;
                                itemText = fileName;
                            }

                            item = new QTableWidgetItem(itemText);
                            item->setData(Qt::UserRole, fileName);
                            item->setTextAlignment(Qt::AlignLeft | Qt::AlignVCenter);
                            item->setFlags(QFlag(Qt::ItemIsEnabled | Qt::ItemIsUserCheckable | Qt::ItemIsSelectable));
                            item->setToolTip(toolTipText);
                            //            item->setFont(font());
                            if (imageImportant)
                                item->setForeground(QColor(198, 25, 10));
                            output << item;
                        }
                    }
                }
            }
            data.close();
        }
    }
    return 1;
}

void ResultsParser::load() {
    clear();
    titles.clear();
    if (view == results || view == images || view == initialization) {
        output.clear();
        loadResults(view);
        if (view == results) {
            setColumnCount(2);
            setRowCount(output.size() / 2);
            for (int i = 0; i < output.size() / 2; i++) {
                setItem(i, 0, output[2 * i]);
                setItem(i, 1, output[2 * i + 1]);
            }
        } else if (view == images || view == initialization) {
            setColumnCount(1);
            setRowCount(output.size());
            for (int i = 0; i < output.size(); i++)
                setItem(i, 0, output[i]);
        }
    }

    resizeContents();
}

void ResultsParser::updateFontInfo() {
    setFont(QFont("Courier", QApplication::font().pointSize()));
}

void ResultsParser::resizeContents() {
    if (view == results) {
        resizeRowsToContents();
        resizeColumnsToContents();
        setColumnWidth(1, maximumViewportSize().width() - columnWidth(0));
    } else if (view == images || view == initialization) {
        resizeRowsToContents();
        setColumnWidth(0, maximumViewportSize().width());
    }

    update();
}

void ResultsParser::resizeEvent(QResizeEvent *event) {
    QTableWidget::resizeEvent(event);
    resizeContents();

}

void ResultsParser::selectImage(QTableWidgetItem * item, QTableWidgetItem* previous) {
    if (item != NULL) {
        emit imageSelected(workingDir + item->data(Qt::UserRole).toString());
        setItemSelected(item, true);
    } else if (previous != NULL) {
    }
}

void ResultsParser::openFile() {
    emit cellActivated(0, 0);
}

void ResultsParser::setImportant(int value) {
    important = (bool)value;
    load();
}

void ResultsParser::setImportant(bool value) {
    important = value;
    load();
}

void ResultsParser::setShowFilenames(int value) {
    showFilenames = (bool)value;
    load();
}

void ResultsParser::setResult(const QString &result) {
    resultsList.clear();
    resultsList << result;
    if (watcher.directories().isEmpty())
        watcher.addPath(projectData.logsDir(QDir(workingDir)).canonicalPath());
    load();
}

void ResultsParser::refreshWatcher() {
    //qDebug()<<resultsList<<" "<<"This is how often I happen.";
    if (!watcher.files().isEmpty()) watcher.removePaths(watcher.files());
    if (QFileInfo(resultsList.last()).exists())
        watcher.addPath(resultsList.last());
    load();
}

