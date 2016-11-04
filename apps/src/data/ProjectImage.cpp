#include <QDir>

#include "ProjectData.h"
#include "ProjectImage.h"

ProjectImage::ProjectImage(const QString& group, const QString directory, bool initParams, QObject* parent)
: QObject(parent), group_(group), directory_(directory) {
    initailizeFolder();
    parameters_ = new ParametersConfiguration(projectData.projectParameterData(), cfgFile(), initParams);
}

QString ProjectImage::group() {
    return group_;
}

void ProjectImage::setGroup(const QString& group) {
    group_ = group;
}

QString ProjectImage::directory() {
    return directory_;
}

void ProjectImage::setDirectory(const QString& dir) {
    directory_ = dir;
}

QDir ProjectImage::workingDir() {
    return QDir(projectData.projectDir().canonicalPath() + "/" + group_ + "/" + directory_);
}

QString ProjectImage::workingPath() {
    return workingDir().canonicalPath();
}

QString ProjectImage::cfgFile() {
    return workingPath() + "/2dx_image.cfg";
}

ParametersConfiguration* ProjectImage::parameters() {
    return parameters_;
}

void ProjectImage::reloadParameters() {
    parameters_->reload();
}

void ProjectImage::initailizeFolder() {
    QDir procdirectory(workingPath() + "/proc");
    if (!procdirectory.exists()) procdirectory.mkdir(workingPath() + "/proc");
    QDir logdirectory(workingPath() + "/LOGS");
    if (!logdirectory.exists()) logdirectory.mkdir(workingPath() + "/LOGS");
}

void ProjectImage::resetWithMasterConfig() {
    if (parameters_) {
        //Copy some original values
        QMap<QString, QString> originals;
        QStringList uniqueParams = projectData.uniqueParamList();
        for (QString param : uniqueParams) {
            originals.insert(param, parameters_->getValue(param));
        }

        projectData.projectParameterData()->saveAs(cfgFile(), true);
        parameters_->reload();

        //Reset original values
        for (QString param : uniqueParams) {
            parameters_->set(param, originals[param], false);
        }
        
        //Set that the parameters were modified and it should be saved
        parameters_->setModified(true);
    }
}

void ProjectImage::backup(int position) {
    QFile(cfgFile()).copy(cfgFile() + "-backup" + QString::number(position));
}

QString ProjectImage::toString() {
    return group() + " -> " + directory();
}

bool ProjectImage::cfgFileExist(const QString& group, const QString& directory) {
    if(QFileInfo(projectData.projectDir().canonicalPath() + "/" + group + "/" + directory + "/2dx_image.cfg").exists()) return true;
    else return false;
}


