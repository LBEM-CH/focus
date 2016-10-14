#ifndef IMAGESCRIPTPROCESSOR_H
#define IMAGESCRIPTPROCESSOR_H

#include <QObject>
#include <QProcess>
#include <QDir>
#include <QDebug>
#include <QStringList>

#include "ResultsData.h"
#include "ScriptParser.h"
#include "ProjectData.h"

class ImageScriptProcessor : public QObject {

    Q_OBJECT

public:
    ImageScriptProcessor(QObject* parent = 0);

    QDir workingDir();
    bool currentlyExecuting();
    bool execute(const QDir& workingDir, const QStringList& scriptsToBeExecuted);
    void stopExecution();

signals:
    void processFinished();
    void statusChanged(const QString& status, bool withError = false);

private:

    void continueExecution(int exitCode);

    QDir workingDir_;
    QStringList scriptsToBeExecuted_;
    QProcess process_;
    QString scriptExecuting_;

};

#endif /* IMAGESCRIPTPROCESSOR_H */

