#ifndef SCRIPTMODULE_H
#define SCRIPTMODULE_H

#include <QWidget>
#include <QListView>
#include <QStringList>
#include <QMap>
#include <QHeaderView>
#include <QStandardItem>
#include <QGridLayout>
#include <QProcess>
#include <QDebug>

#include "SpinBoxDelegate.h"
#include "ScriptData.h"
#include "ScriptParser.h"

class ScriptModule : public QWidget {

    Q_OBJECT
    
public:
    ScriptModule(const QDir& scriptDirectory, const QDir& workingDirectory, QWidget* parent = 0);
    
    void extendSelectionTo(ScriptModule *module);
    QStringList displayedVariables(QModelIndex index);
    QMap<QString, QString> variablesToReset(QModelIndex index);
    
    QString logFile(QModelIndex index);
    QString logFile();
    QString resultsFile(QModelIndex index);
    QString resultsFile();
    
    QString title(QModelIndex index);
    uint uid();
    
    bool isRunning();
    
    QItemSelectionModel* getSelection();
    
    quint32 getScriptProgress(quint32 uid);
    QStringList getScriptManual(quint32 uid);
    QStringList getScriptDependents(quint32 uid);
    
    QIcon getModuleToolIcon();
    QIcon getModuleScriptIcon();
    QString getModuleDescription();
    QString getModuleSelection();

public slots:
    void execute(bool run);
    void clearSelection();
    void clearExtendedSelections();

    void readStdOut();
    void readStdErr();

    void scriptActivated(QModelIndex item);
    void scriptFinished(int exitCode);

    void selectAll();
    void select(QModelIndex index, bool shouldResetParam = false);
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
    void shouldResetParams(QModelIndex index);

    void scriptLaunched();
    void scriptCompleted(QModelIndex index);
    void allScriptsCompleted();

    void initialized();

private:
    QDir scriptDir;
    QDir workingDir;
    QList<ScriptModule *> selectionObjects;
    QMap<quint32, QMap<QString, QVariant> > scriptData;
    QMap<quint32, quint32> scriptProgress;
    QMap<quint32, QMap<QString, QString>> resetVars;
    QMap<quint32, QStringList> manual;
    QMap<quint32, QStringList> subScripts;
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

};

#endif
