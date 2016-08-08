/*
 *  scriptModule.h
 *  2dx_merge
 *
 *  Created by Bryant Gipson on 1/8/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef SCRIPTMODULE_H
#define SCRIPTMODULE_H

#include <QWidget>
#include <QListView>
#include <QStringList>
#include <QHeaderView>
#include <QStandardItem>
#include <QGridLayout>
#include <QProcess>
#include <QDebug>
#include <SpinBoxDelegate.h>
#include <confData.h>
#include <scriptParser.h>

class scriptModule : public QWidget {

    Q_OBJECT

public:
    enum moduleType {
        standard, custom, singleparticle, merge2D, merge3D, project
    };

public slots:
    void execute(bool run);
    void clearSelection();
    void clearExtendedSelections();

    void readStdOut();
    void readStdErr();

    void scriptActivated(QModelIndex item);
    void scriptFinished(int exitCode);

    void selectAll();
    void select(QModelIndex index);
    void selectFirst();
    void select(const QItemSelection &selected);
    void select(const QItemSelection &selected, const QItemSelection &deselected);
    void initialize();

    void setVerbosity(int value);

signals:
    void halt();
    void standardOut(const QStringList &text);
    void standardError(const QByteArray &text);

    void progress(int value);
    void reload();

    void currentScriptChanged(QModelIndex index);

    void scriptLaunched();
    void scriptCompleted(QModelIndex index);
    void allScriptsCompleted();

    void initialized();

private:
    QDir scriptDir;
    moduleType scriptType;
    confData *data;
    QList<scriptModule *> selectionObjects;
    QHash<quint32, QHash<QString, QVariant> > scriptData;
    QHash<quint32, quint32> scriptProgress;
    QHash<quint32, confData *> localConf;
    QProcess process;

    quint32 currentUid;
    QStandardItem *runningScript;
    int runningIndex;
    QList<int> executionList;

    QListView *view;
    QStandardItemModel *model;
    QItemSelectionModel *selection;

    bool currentlyRunning;
    
    int verbosity;

    void setupModule();
    void addScriptProperty(quint32 uid, const QString &property, const QVariant &value);
    const QVariant &getScriptProperty(quint32 uid, const QString &property);

    bool clean(int uid);

    bool runningScriptSelected();

    bool writeToLog(const QString &logText);

    bool initializeExecution();
    void cleanupExecution();

public:
    scriptModule(confData *conf, const QDir &directory, scriptModule::moduleType type = scriptModule::standard, QWidget *parent = NULL);
    void extendSelectionTo(scriptModule *module);

    QStringList displayedVariables(QModelIndex index);
    QString logFile(QModelIndex index);
    QString logFile();
    QString resultsFile(QModelIndex index);
    QString resultsFile();
    QString title(QModelIndex index);
    moduleType type();
    uint uid();
    confData *conf(QModelIndex index);
    bool isRunning();
    QItemSelectionModel* getSelection();
    quint32 getScriptProgress(quint32 uid);
    QStringList getScriptManual(quint32 uid);
    QStringList getScriptDependents(quint32 uid);
    
    QIcon getModuleToolIcon();
    QIcon getModuleScriptIcon();
    QString getModuleDescription();

};

#endif
