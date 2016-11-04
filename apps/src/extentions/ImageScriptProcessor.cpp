#include "ImageScriptProcessor.h"
#include "ApplicationData.h"

ImageScriptProcessor::ImageScriptProcessor(QObject* parent)
: QObject(parent) {
    connect(&process_, static_cast<void(QProcess::*)(int)> (&QProcess::finished), this, &ImageScriptProcessor::continueExecution);
}

ProjectImage* ImageScriptProcessor::workingImage() {
    return image_;
}

bool ImageScriptProcessor::currentlyExecuting() {
    if (process_.state() == QProcess::Running) return true;
    else return false;
}

bool ImageScriptProcessor::execute(ProjectImage* image, const QStringList& scriptsToBeExecuted) {
    if (currentlyExecuting()) {
        qDebug() << "Processor already processing " << image->toString();
        return false;
    }

    image_ = image;
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
        ResultsData resultsData(image_->workingPath());
        resultsData.load(image_->workingPath() + "/LOGS/" + scriptExecuting_ + ".results");
        resultsData.save();
    }

    if (scriptsToBeExecuted_.isEmpty()) {
        ParametersConfiguration* conf = image_->parameters();
        conf->set("last_processed", ApplicationData::currentDateTimeString());
        emit statusChanged("FINISHED   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
        scriptExecuting_ = "";
        emit processFinished();
        return;
    }

    ScriptParser parser(image_->workingPath());
    QString scriptPath = scriptsToBeExecuted_.first();
    scriptsToBeExecuted_.removeFirst();

    if (scriptPath.startsWith("cp -f")) {
        process_.setWorkingDirectory(image_->workingPath());
        process_.start(scriptPath, QIODevice::ReadOnly);
        QString copyFile;
        if (scriptPath.trimmed().split(' ').size() > 2) copyFile = scriptPath.trimmed().split(' ')[2];
        emit statusChanged("Copying file: " + copyFile);
    } else {
        QString scriptName = QFileInfo(scriptPath).fileName().remove(QRegExp("\\.script$"));
        scriptExecuting_ = scriptName;
        if (QFileInfo(scriptPath).exists()) {
            emit statusChanged("Executing: " + scriptName);

            if (parser.parse(scriptPath, image_->workingPath() + "/proc/" + scriptName + ".com") == 0) {
                process_.setWorkingDirectory(image_->workingPath());
                process_.setStandardOutputFile(image_->workingPath() + "/LOGS/" + scriptName + ".log");
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
}
