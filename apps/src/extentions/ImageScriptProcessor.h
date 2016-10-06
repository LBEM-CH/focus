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
    ImageScriptProcessor(QObject* parent = 0)
    : QObject(parent) {
        connect(&process_, static_cast<void(QProcess::*)(int)> (&QProcess::finished), this, &ImageScriptProcessor::continueExecution);
    }

    QDir workingDir() {
        return workingDir_;
    }

    bool currentlyExecuting() {
        if (process_.state() == QProcess::Running) return true;
        else return false;
    }

    bool execute(const QDir& workingDir, const QStringList& scriptsToBeExecuted) {
        if (currentlyExecuting()) {
            qDebug() << "Processor already processing " << workingDir_.canonicalPath();
            return false;
        }
        workingDir_ = workingDir;
        
        
        
        scriptsToBeExecuted_ = scriptsToBeExecuted;
        emit statusChanged("STARTING ============================");
        continueExecution(0);

        return true;
    }

    void stopExecution() {
        scriptsToBeExecuted_.clear();
        scriptExecuting_ = "";
        workingDir_ = projectData.projectDir();
        
        if (process_.state() == QProcess::Running) {
            disconnect(&process_, static_cast<void(QProcess::*)(int)> (&QProcess::finished), this, &ImageScriptProcessor::continueExecution);
            process_.kill();
            while (!process_.waitForFinished()) process_.kill();
            connect(&process_, static_cast<void(QProcess::*)(int)> (&QProcess::finished), this, &ImageScriptProcessor::continueExecution);
        }
        
        emit statusChanged("$$$$$$$ STOPPED $$$$$$$");
    }

signals:
    void processFinished();
    void statusChanged(const QString& status, bool withError = false);

private:

    void continueExecution(int exitCode) {
        if (exitCode != 0) {
            emit statusChanged("Error in running script: " + scriptExecuting_, true);
        }

        if (!scriptExecuting_.isEmpty()) {
            emit statusChanged("Saving results from script: " + scriptExecuting_);
            ResultsData resultsData(workingDir_);
            if(!resultsData.load(workingDir_.canonicalPath() + "/LOGS/" + scriptExecuting_ + ".results")) {
                emit statusChanged("Error in loading results ", true);
            }
            resultsData.save();
        }

        if (scriptsToBeExecuted_.isEmpty()) {
            emit statusChanged("FINISHED   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
            scriptExecuting_ = "";
            emit processFinished();
            return;
        }

        ScriptParser parser(workingDir_.canonicalPath());
        QString scriptPath = scriptsToBeExecuted_.first();
        scriptsToBeExecuted_.removeFirst();

        QString scriptName = QFileInfo(scriptPath).fileName().remove(QRegExp("\\.script$"));
        scriptExecuting_ = scriptName;
        if (QFileInfo(scriptPath).exists()) {
            emit statusChanged("Executing: " + scriptName);

            if(parser.parse(scriptPath, workingDir_.canonicalPath() + "/proc/" + scriptName + ".com") == 0) {
                process_.setWorkingDirectory(workingDir_.canonicalPath());
                process_.setStandardOutputFile(workingDir_.canonicalPath() + "/LOGS/" + scriptName + ".log");
                process_.start('"' + parser.executionString() + '"', QIODevice::ReadOnly);
            } else {
                emit statusChanged("Error in creating runnable csh file", true);
                continueExecution(-1);
            }
        }
    }

    QDir workingDir_;
    QStringList scriptsToBeExecuted_;
    QProcess process_;
    QString scriptExecuting_;

};

#endif /* IMAGESCRIPTPROCESSOR_H */

