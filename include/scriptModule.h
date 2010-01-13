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
#include <QTreeView>
#include <QHeaderView>
#include <QStandardItem>
#include <QGridLayout>
#include <QProcess>
#include <QDebug>
#include <SpinBoxDelegate.h>
#include <confData.h>
#include <scriptParser.h>

class scriptModule : public QWidget
{
  Q_OBJECT

  public:
  enum moduleType {standard, custom};

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
	void select(const QItemSelection &selected, const QItemSelection &deselected);
  void initialize();

  void setVerbosity(int value);

  signals:
  void halt();
  void standardOut(const QStringList &text);
  void standardError(const QByteArray &text);

  void progress(int value);
  void incrementProgress(int inc);
  void reload();

  void currentScriptChanged(QModelIndex index);
  void runningScriptChanged(QModelIndex index);

  void scriptLaunched();
  void scriptCompleted(QModelIndex index);

  void initialized();

  private:
  QDir scriptDir;
  moduleType scriptType;
  confData *data;
  QList<scriptModule *> selectionObjects;
  QHash<quint32,QHash<QString,QVariant> > scriptData;
  QHash<quint32,confData *> localConf;
  QProcess process;

  quint32 currentUid;
  QStandardItem *runningScript;
  int runningIndex;
  QList<int> executionList;

  QTreeView *view;
  QStandardItemModel *model;
  QItemSelectionModel *selection;

  bool currentlyRunning;

  int verbosity;

  QTreeView *setupModule();
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

  QStringList globalVariables(QModelIndex index);
  QString logFile(QModelIndex index);
  QString logFile();
  QString resultsFile(QModelIndex index);
  QString resultsFile();
  QString title(QModelIndex index);
  uint uid();
  confData *conf(QModelIndex index);
  bool isRunning();
};

#endif
