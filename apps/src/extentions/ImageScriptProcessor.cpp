#include "ImageScriptProcessor.h"

ImageScriptProcessor::ImageScriptProcessor(QObject* parent)
: QObject(parent) {
    connect(&process_, static_cast<void(QProcess::*)(int)> (&QProcess::finished), this, &ImageScriptProcessor::continueExecution);
    workingDir_ = projectData.projectDir();
}

QDir ImageScriptProcessor::workingDir() {
    return workingDir_;
}

bool ImageScriptProcessor::currentlyExecuting() {
    if (process_.state() == QProcess::Running) return true;
    else return false;
}

bool ImageScriptProcessor::execute(const QDir& workingDir, const QStringList& scriptsToBeExecuted) {
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

void ImageScriptProcessor::continueExecution(int exitCode) {
    if (exitCode != 0) {
        emit statusChanged("Error in running script: " + scriptExecuting_, true);
    }

    if (!scriptExecuting_.isEmpty()) {
        emit statusChanged("Saving results from script: " + scriptExecuting_);
        ResultsData resultsData(workingDir_);
        if (!resultsData.load(workingDir_.canonicalPath() + "/LOGS/" + scriptExecuting_ + ".results")) {
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

    if (scriptPath.startsWith("cp -f")) {
        process_.setWorkingDirectory(workingDir_.canonicalPath());
        process_.start(scriptPath, QIODevice::ReadOnly);
        QString copyFile;
        if (scriptPath.trimmed().split(' ').size() > 2) copyFile = scriptPath.trimmed().split(' ')[2];
        emit statusChanged("Copying file: " + copyFile);
    } else {
        QString scriptName = QFileInfo(scriptPath).fileName().remove(QRegExp("\\.script$"));
        scriptExecuting_ = scriptName;
        if (QFileInfo(scriptPath).exists()) {
            emit statusChanged("Executing: " + scriptName);

            if (parser.parse(scriptPath, workingDir_.canonicalPath() + "/proc/" + scriptName + ".com") == 0) {
                process_.setWorkingDirectory(workingDir_.canonicalPath());
                process_.setStandardOutputFile(workingDir_.canonicalPath() + "/LOGS/" + scriptName + ".log");
                process_.start('"' + parser.executionString() + '"', QIODevice::ReadOnly);
            } else {
                emit statusChanged("Error in creating runnable csh file", true);
                continueExecution(-1);
            }
        }
    }
}

void ImageScriptProcessor::stopExecution() {
    if (process_.state() == QProcess::Running) {
        disconnect(&process_, static_cast<void(QProcess::*)(int)> (&QProcess::finished), this, &ImageScriptProcessor::continueExecution);
        process_.kill();
        while (!process_.waitForFinished()) process_.kill();
        connect(&process_, static_cast<void(QProcess::*)(int)> (&QProcess::finished), this, &ImageScriptProcessor::continueExecution);
    }

    emit statusChanged("$$$$$$$ STOPPED $$$$$$$");

    scriptsToBeExecuted_.clear();
    scriptExecuting_ = "";
    workingDir_ = projectData.projectDir();
}
