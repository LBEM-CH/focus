/**************************************************************************
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

#include <QPalette>
#include <QLabel>

#include "ApplicationData.h"
#include "ProjectData.h"
#include "UserPreferenceData.h"
#include "ScriptModuleProperties.h"

#include "ScriptModule.h"

ScriptModule::ScriptModule(const QDir& scriptDirectory, const QDir& workingDirectory, QWidget *parent)
: QWidget(parent) {
    scriptDir = scriptDirectory;
    workingDir = workingDirectory;
    
    runningScript = NULL;
    currentlyRunning = false;
    verbosity = 1;

    QVBoxLayout *layout = new QVBoxLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);
    setLayout(layout);

    setupModule();
    if (view != NULL) {
        if (getModuleSelection() == "single") view->setSelectionMode(QAbstractItemView::SingleSelection);
        else if (getModuleSelection() == "extended") view->setSelectionMode(QAbstractItemView::ExtendedSelection);
     
        layout->addStretch(0);
        layout->addWidget(view, 1);

        connect(view->selectionModel(), SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection&)), this, SLOT(select(const QItemSelection &, const QItemSelection&)));
        connect(&process, SIGNAL(readyReadStandardOutput()), this, SLOT(readStdOut()));
        connect(&process, SIGNAL(readyReadStandardError()), this, SLOT(readStdErr()));
        connect(&process, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(scriptFinished(int)));
        connect(view, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(scriptActivated(QModelIndex)));
    }
    
    view->selectionModel()->clearSelection();
}

void ScriptModule::setupModule() {
    if (!scriptDir.exists()) {
        qDebug() << "The directory " << scriptDir.canonicalPath() << "does not exist";
        return;
    }

    view = new QListView;
    view->setUniformItemSizes(true);
    view->setItemDelegate(new SpinBoxDelegate);
    view->setAttribute(Qt::WA_MacShowFocusRect, 0);
    QPalette p = view->palette();
    p.setColor(QPalette::Highlight, Qt::gray);
    //p.setColor(QPalette::HighlightedText, Qt::darkBlue);
    view->setPalette(p);
    
    model = new QStandardItemModel;

    QString entry;
    QStandardItem *item;
    QMap<int, QStandardItem*> map;
    quint32 sortOrder, uid;
    
    foreach(entry, scriptDir.entryList(QStringList() << "*.script", QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted)) {
        QStringList titleList;
        ScriptData scriptFileData(scriptDir.canonicalPath() + "/" + entry);
        sortOrder = scriptFileData.property("sortOrder").toUInt();

        uid = qHash(scriptDir.canonicalPath() + "/" + entry)^qHash(sortOrder);

        titleList << QString(scriptFileData.property("title")).simplified();

        item = new QStandardItem(titleList.first());

        item->setToolTip(entry);

        addScriptProperty(uid, "path", scriptDir.absolutePath() + "/");
        addScriptProperty(uid, "fileName", entry);
        addScriptProperty(uid, "filePath", scriptDir.canonicalPath() + "/" + entry);
        addScriptProperty(uid, "title", scriptFileData.property("title"));
        addScriptProperty(uid, "displayedVars", QVariant(scriptFileData.propertyList("display")));
        addScriptProperty(uid, "logFile", workingDir.canonicalPath() + "/LOGS/" + entry.section('.', 0, -2) + ".log");
        addScriptProperty(uid, "resultsFile", workingDir.canonicalPath() + "/LOGS/" + entry.section('.', 0, -2) + ".results");

        manual.insert(uid, scriptFileData.manual());
        subScripts.insert(uid, scriptFileData.subScripts().toList());
        resetVars.insert(uid, scriptFileData.resetVariables());
        
        scriptProgress[uid] = 0;

        item->setData(scriptDir.canonicalPath() + "/" + entry, Qt::UserRole + 5);

        item->setData(uid, Qt::UserRole);
        item->setEditable(false);
        item->setIcon(getModuleScriptIcon());
        item->setTextAlignment(Qt::AlignVCenter);

        QFont itemFont;
        itemFont.setBold(true);
        item->setFont(itemFont);

        item->setSizeHint(QSize(200, 30));

        map.insert(sortOrder, item);
    }

    QMapIterator<int, QStandardItem*> it(map);
    while (it.hasNext()) {
        it.next();
        model->appendRow(it.value());
    }

    view->setModel(model);

    selection = view->selectionModel();

    view->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    //view->setAlternatingRowColors(true);
    //view->resizeColumnToContents(0);

    clearSelection();
}

void ScriptModule::addScriptProperty(quint32 uid, const QString &property, const QVariant &value) {
    scriptData[uid][property.toLower()] = value;
}

const QVariant & ScriptModule::getScriptProperty(quint32 uid, const QString &property) {
    return scriptData[uid][property.toLower()];
}

quint32 ScriptModule::getScriptProgress(quint32 uid) {
    return scriptProgress[uid];
}

QStringList ScriptModule::getScriptManual(quint32 uid) {
    return manual[uid];
}

QStringList ScriptModule::getScriptDependents(quint32 uid) {
    return subScripts[uid];
}

QItemSelectionModel* ScriptModule::getSelection() {
    return selection;
}

void ScriptModule::clearSelection() {
    if (selection->hasSelection())
        selection->clearSelection();

}

void ScriptModule::extendSelectionTo(ScriptModule *module) {
    selectionObjects << module;
}

void ScriptModule::clearExtendedSelections() {
    //  if(!selection->hasSelection()) return;
    QListIterator<ScriptModule *> it(selectionObjects);
    while (it.hasNext())
        it.next()->clearSelection();

    QListIterator<ScriptModule *> it2(selectionObjects);
    while (it2.hasPrevious())
        it2.previous()->clearSelection();
}

bool ScriptModule::initializeExecution() {

    if (executionList.isEmpty()) {
        if (selection->selectedIndexes().isEmpty()) {
            return false;
        }
        runningIndex = 0;
        QMap<int, char> s;

        foreach(QModelIndex index, selection->selectedIndexes()) {
            if (!index.parent().isValid()) {
                model->item(index.row())->setForeground(QColor(0, 186, 0));
                s.insert(index.row(), 0);
            } else {
                selection->select(index.parent(), QItemSelectionModel::Current);
                model->item(index.parent().row())->setForeground(QColor(0, 186, 0));
                s.insert(index.parent().row(), 0);
            }
        }
        executionList = s.keys();
    } else if (runningScript == NULL) {
        runningIndex++;
        if (runningIndex >= executionList.size()) {
            currentlyRunning = false;
            emit scriptCompleted(model->item(executionList.last())->index());
            cleanupExecution();
            if (!selection->selectedRows().isEmpty())
                emit currentScriptChanged(selection->selectedRows().last());
            return false;
        }
    }

    runningScript = model->item(executionList[runningIndex]);
    if (runningScriptSelected()) select(runningScript->index());

    return true;
}

void ScriptModule::cleanupExecution() {

    foreach(int i, executionList) {
        model->item(i)->setForeground(Qt::black);
    }
    executionList.clear();
    emit allScriptsCompleted();
    runningScript = NULL;
}

void ScriptModule::execute(bool run) {
    QString scriptPath, scriptName;
    QString scriptHeader(78, '#');
    if (run) {
        QStandardItem *it;

        currentlyRunning = initializeExecution();
        if (!currentlyRunning) {
            return;
        }
        it = runningScript;

        it->setForeground(QColor(0, 0, 186));
        currentUid = it->data(Qt::UserRole).toUInt();
        scriptName = getScriptProperty(currentUid, "fileName").toString();
        scriptPath = getScriptProperty(currentUid, "path").toString();
        QString scriptTitle = scriptName.section('.', 0, -2);

        ScriptParser parser(workingDir);

        if (!clean(currentUid)) qDebug() << "Error removing " << getScriptProperty(currentUid, "logFile").toString() << endl;

        if (runningScriptSelected()) emit scriptLaunched();

        scriptHeader.replace((scriptHeader.size() - scriptTitle.size()) / 2, scriptTitle.size(), scriptTitle);
        scriptHeader = "::" + scriptHeader + "\n";

        if (runningScriptSelected()) emit standardOut(QStringList() << scriptHeader);
        writeToLog(scriptHeader);

        parser.parse(scriptPath + "/" + scriptName, ProjectData::procDir(workingDir).canonicalPath() + "/" + scriptName.section('.', 0, -2) + ".com");
        process.setWorkingDirectory(workingDir.canonicalPath());
        process.start('"' + parser.executionString() + '"', QIODevice::ReadOnly);
    } else if (process.state() == QProcess::Running) {
        scriptName = getScriptProperty(currentUid, "fileName").toString();
        scriptPath = getScriptProperty(currentUid, "path").toString();
        QString scriptTitle = scriptName.section('.', 0, -2) + " Halted by User";

        scriptHeader.replace((scriptHeader.size() - scriptTitle.size()) / 2, scriptTitle.size(), scriptTitle);
        scriptHeader = scriptHeader + "\n";

        if (runningScriptSelected()) emit standardError(scriptHeader.toLatin1());
        writeToLog("<error>" + scriptHeader + "</error>" + '\n');

        disconnect(&process, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(scriptFinished(int)));
        process.kill();
        while (!process.waitForFinished())
            process.kill();
        scriptFinished(-1);
        connect(&process, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(scriptFinished(int)));
    }
}

bool ScriptModule::clean(int uid) {
    bool result = true;
    QFile logFile(getScriptProperty(uid, "logFile").toString());
    if (logFile.exists())
        result = logFile.remove();
    logFile.close();
    return result;
}

void ScriptModule::readStdOut() {
    QStringList text;
    QString line, lineTrimmed, lineLower;
    while (!process.atEnd()) {
        line = process.readLine();
        lineTrimmed = line.trimmed();
        lineLower = lineTrimmed.toLower();
        if (lineLower.startsWith("<<@progress:")) {
            QStringList value = lineTrimmed.split(':');
            value.last().remove('>');
            if (value.last().contains('+')) {
                value.last().remove('+');
                scriptProgress[currentUid] += value.last().toInt();
            } else {             
                scriptProgress[currentUid] = value.last().toInt();
            }
            if(runningScriptSelected()) emit progress(scriptProgress[currentUid]);
        } else if (lineLower.startsWith("<<@evaluate>>")) {
            emit reload();
        } else
            text << line;
    }

    if (runningScriptSelected() && verbosity != 0) emit standardOut(text);

    writeToLog(text.join(""));
}

void ScriptModule::readStdErr() {
    QString text = process.readAllStandardError();

    if (runningScriptSelected() && verbosity != 0) emit standardError(text.toLatin1());

    writeToLog("<error>" + text + "</error>");
}

QStringList ScriptModule::displayedVariables(QModelIndex index) {
    return getScriptProperty(index.data(Qt::UserRole).toUInt(), "displayedVars").toStringList();
}

QMap<QString, QString> ScriptModule::variablesToReset(QModelIndex index) {
    return resetVars[index.data(Qt::UserRole).toUInt()];
}

QString ScriptModule::logFile(QModelIndex index) {
    return getScriptProperty(index.data(Qt::UserRole).toUInt(), "logFile").toString();
}

QString ScriptModule::logFile() {
    return getScriptProperty(uid(), "logFile").toString();
}

QString ScriptModule::resultsFile(QModelIndex index) {
    return getScriptProperty(index.data(Qt::UserRole).toUInt(), "resultsFile").toString();
}

QString ScriptModule::resultsFile() {
    return getScriptProperty(model->item(0)->data(Qt::UserRole).toUInt(), "resultsFile").toString();
}

void ScriptModule::scriptActivated(QModelIndex item) {
    QProcess::startDetached(userPreferenceData.get("scriptEditor") + " " + item.data(Qt::UserRole + 5).toString());
}

bool ScriptModule::runningScriptSelected() {
    if (selection->selectedIndexes().isEmpty()) return false;
    return selection->isSelected(runningScript->index());
}

bool ScriptModule::writeToLog(const QString &logText) {
    QString logFilename = getScriptProperty(currentUid, "logFile").toString();
    QFile log(logFilename);
    if (!log.open(QIODevice::Append | QIODevice::Text)) {
        qDebug() << "Failed to write to " << getScriptProperty(currentUid, "logFile").toString() << endl;
        return false;
    }
    log.write(logText.toLatin1());
    log.close();
    return true;
}

void ScriptModule::scriptFinished(int exitCode) {
    QString scriptTitle = getScriptProperty(currentUid, "fileName").toString().section('.', 0, -2);
    QString scriptHeader(78, '#');
    scriptHeader.replace((scriptHeader.size()-(scriptTitle + " finished.").size()) / 2, (scriptTitle + " finished.").size(), scriptTitle + " finished.");
    scriptHeader = "::" + scriptHeader + "\n";
    if (runningScriptSelected()) emit standardOut(QStringList() << scriptHeader);
    writeToLog(scriptHeader);

    if (runningScript == NULL)
        qDebug() << "Running script is NULL and should not be!";
    else {
        if (runningScript->row() == 0) emit initialized();
    }

    runningScript->setForeground(Qt::black);

    if (executionList.isEmpty()) {
        currentlyRunning = false;
        emit scriptCompleted(runningScript->index());
        emit allScriptsCompleted();
        if (exitCode == -1) cleanupExecution();
        if (!selection->selectedRows().isEmpty())
            emit currentScriptChanged(selection->selectedRows().last());
    } else {
        if (exitCode == -1) {
            cleanupExecution();
            //if(!selection->selectedRows().isEmpty())
            //  emit currentScriptChanged(selection->selectedRows().last());
        } else {
            emit scriptCompleted(runningScript->index());
            runningScript = NULL;
            execute(true);
        }
    }
}

uint ScriptModule::uid() {
    if (selection->hasSelection())
        return selection->selectedIndexes().first().data(Qt::UserRole).toUInt();
    else
        qDebug() << "No selection!" << endl;
    return 0;
}

QString ScriptModule::title(QModelIndex index) {
    return getScriptProperty(index.data(Qt::UserRole).toUInt(), "title").toString();
}

void ScriptModule::initialize() {
    setVerbosity(1);
    select(model->item(0)->index(), true);
    execute(true);
}

void ScriptModule::selectFirst() {
    select(model->item(0)->index(), true);
}

void ScriptModule::select(QModelIndex index, bool shouldResetParam) {
    clearExtendedSelections();
    if (!selection->isSelected(index)) selection->select(index, QItemSelectionModel::Select);
    if (index.parent().isValid()) selection->select(index.parent(), QItemSelectionModel::Current);
    if (selection->selectedIndexes().isEmpty()) qDebug() << "No selection for some reason..." << endl;
    emit currentScriptChanged(index);
    if(shouldResetParam) emit shouldResetParams(index);
}

void ScriptModule::select(const QItemSelection& selected) {
    if (selected.indexes().isEmpty()) return;
    select(selected.indexes().first(), true);
}

void ScriptModule::select(const QItemSelection &selected, const QItemSelection &deselected) {
    select(selected);
}

void ScriptModule::selectAll() {
    select(model->item(0)->index());
    view->selectAll();
}

bool ScriptModule::isRunning() {
    return (currentlyRunning || process.state() == QProcess::Running);
}

void ScriptModule::setVerbosity(int value) {
    verbosity = value;
}

QIcon ScriptModule::getModuleScriptIcon() {
    return ApplicationData::icon(ScriptModuleProperties(scriptDir.absolutePath()).scriptIcon());
}

QIcon ScriptModule::getModuleToolIcon() {
    return ApplicationData::icon(ScriptModuleProperties(scriptDir.absolutePath()).icon());
}

QString ScriptModule::getModuleDescription() {
    return ScriptModuleProperties(scriptDir.absolutePath()).title(); 
}

QString ScriptModule::getModuleSelection() {
    return ScriptModuleProperties(scriptDir.absolutePath()).selection(); 
}
