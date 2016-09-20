
#include <QDebug>
#include <QMessageBox>

#include "ApplicationData.h"
#include "ParameterMaster.h"

#include "ProjectData.h"

ProjectData& ProjectData::Instance() {
    static ProjectData instance_;
    return instance_;
}

void ProjectData::initiailze(const QDir& projectDir) {
    projectDir_ = projectDir;
    registerParameterMaster(ApplicationData::masterCfgFile());
    registerParameterMaster(ApplicationData::userCfgFile());
    projectParameters_ = new ParametersConfiguration(ApplicationData::masterCfgFile(), projectWorkingDir().canonicalPath() + "/2dx_merge.cfg");
    indexImages();
}

void ProjectData::registerParameterMaster(const QString& fileName) {
    if(!QFileInfo(fileName).exists()) {
        qDebug() << "Configuration File Missing!" << fileName << " does not exist. Will quit now.";
        exit(0);
    }
    
    QFile data(fileName);
    if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Configuration file read error!" << fileName << " was not read. Will quit now.";
        exit(0);
    }

    QStringList valueSearch;
    valueSearch << "LABEL" << "LEGEND" << "EXAMPLE" << "HELP" << "TYPE" << "RELATION" << "USERLEVEL" << "SYNC_WITH_UPPER_LEVEL";
    
    QMap<QString, QString> propertiesRead;
    QString lineData;
    qint64 pos = -1;
    while (!data.atEnd() && pos != data.pos()) {
        pos = data.pos();
        lineData = data.readLine().trimmed();
        lineData.remove('#');
        lineData = lineData.trimmed();

        for (int i = 0; i < valueSearch.size(); i++) {
            if (lineData.startsWith(valueSearch[i] + ':')) {
                lineData.remove(0, valueSearch[i].size() + 1);
                propertiesRead.insert(valueSearch[i].simplified().toLower(), lineData);
            }
        }

        if (lineData.toLower().startsWith("set ")) {
            int k = lineData.indexOf('=');
            if (k > 0) {
                QStringList val = lineData.split('=');

                val[0].remove(0, 4);
                val[1].remove('"');

                val[0] = val[0].simplified();
                val[1] = val[1].simplified();
                
                parameterMaster.registerParameter(val[0], propertiesRead);
                propertiesRead.clear();
            }
        }
    }

    data.close();
}

QStringList ProjectData::imageList() {
    return ProjectPreferences(projectDir()).imageList();
}

void ProjectData::addImage(const QDir& imageDir) {
    if(!QFileInfo(imageDir.canonicalPath() + "/2dx_image.cfg").exists()) {
        qDebug() << "Error while adding image: " << imageDir.canonicalPath() << "No config files found.";
        return;
    }
    ProjectPreferences(projectDir()).addImage(imageDir.canonicalPath());
    qDebug() << "Added image: " << imageDir.canonicalPath();
    emit imageDirsChanged();
}


void ProjectData::indexImages() {
    imageToParameterData_.clear();
    QStringList imageList;
    QProgressDialog progressDialog;
    progressDialog.setCancelButton(0);
    progressDialog.setRange(0, projectDir().entryList().count() - 1); // -1 because a merge dir is present in project folder!!);
    progressDialog.setWindowTitle("Scanning images");
    progressDialog.show();
    initializeImageParameters(projectDir(), imageList, progressDialog);
    progressDialog.reset();
    ProjectPreferences(projectDir()).resetImageList(imageList);
    emit imageDirsChanged();
}

void ProjectData::initializeImageParameters(const QDir& currDir, QStringList& imageList, QProgressDialog& dialog) {
    QDir dir = currDir;
    dir.setFilter(QDir::NoDotAndDotDot | QDir::Dirs);

    if (dir.relativeFilePath(projectDir().canonicalPath()) == "../") {
        dialog.setValue(dialog.value()+1);
    }
    dialog.setLabelText("Scanning folder for images:\n" + currDir.canonicalPath());
    qApp->processEvents();
    
    foreach(QString entry, dir.entryList()) {
        QString configFile = dir.canonicalPath() + "/" + entry + "/" + "2dx_image.cfg";
        QString masterFile = dir.canonicalPath() + "/" + entry + "/" + "2dx_master.cfg";

        if (QFileInfo(configFile).exists() || QFileInfo(masterFile).exists()) {
            initializeImageParameters(QDir(dir.canonicalPath() + "/" + entry + "/"), imageList, dialog);

            if (QFileInfo(configFile).exists()) {
                imageList.append(QFileInfo(configFile).canonicalPath());
            }
        }
    }
}

ParametersConfiguration* ProjectData::parameterData(const QDir& workDir) {
    if (workDir.canonicalPath() == projectWorkingDir().canonicalPath()) return projectParameters_;
    if (!imageToParameterData_.keys().contains(workDir.canonicalPath())) {
        QString configFile = workDir.canonicalPath() + "/2dx_image.cfg";
        if (QFileInfo(configFile).exists()) {
            ParametersConfiguration* localData = new ParametersConfiguration(ApplicationData::masterCfgFile(), configFile, projectParameters_);
            imageToParameterData_.insert(QFileInfo(configFile).canonicalPath(), localData);
        }
        else {
            qDebug() << "CRITICAL: " << configFile << "requested, but not present, program may crash!";
            return 0;
        }
    }
    
    return imageToParameterData_[workDir.canonicalPath()];
}

void ProjectData::reloadParameterData(const QDir& workDir) {
    if(parameterData(workDir)) parameterData(workDir)->reload();
}

ParametersConfiguration* ProjectData::projectParameterData() {
    return projectParameters_;
}

void ProjectData::reloadProjectParameters() {
    projectParameters_->reload();
}

QDir ProjectData::projectDir() const {
    return projectDir_;
}

QDir ProjectData::projectWorkingDir() const {
    return QDir(projectDir_.canonicalPath() + "/merge");
}

void ProjectData::saveAsProjectDefault(const QDir& workingDir) {
    if (QMessageBox::question(NULL,
            tr("Save as default?"), QString("Saving as project default will change master config file and set default values for all other new imported images in this project.\n\n.") + 
            QString("NOTE that to change the parameters of already imported images, you will have to run RESET IMAGE CONFIGS script from PROJECT TOOLS tab.\n\n Proceed?"),
            tr("Yes"),
            tr("No"),
            QString(), 0, 1) == 0) {
        parameterData(workingDir)->saveAs(projectWorkingDir().canonicalPath() + "/2dx_merge.cfg", false);
        reloadProjectParameters();
    }
    
    
}

QString ProjectData::projectName() {
    return ProjectPreferences(projectDir()).projectName();
}

void ProjectData::setProjectName(const QString& projectName) {
    ProjectPreferences(projectDir()).setProjectName(projectName);
}

void ProjectData::toggleAutoSave() {
    autoSave_ = !autoSave_;

    if (autoSave_) {
        QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched on"));
    } else {
        QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched off"));
    }
}

bool ProjectData::isAutoSave() {
    return autoSave_;
}

QStringList ProjectData::imagesOpen() {
    return ProjectPreferences(projectDir()).imagesOpen();
}

bool ProjectData::imageOpen(const QString path) {
    QStringList imagesOp = imagesOpen();
    if(imagesOp.contains(path)) return true;
    else return false;
}


void ProjectData::setImagesOpen(const QStringList& paths) {
    ProjectPreferences(projectDir()).setImagesOpen(paths);
}

QDir ProjectData::logsDir(const QDir& workingDir) {
    QDir dir = QDir(workingDir.canonicalPath() + "/LOGS");
    if (!dir.exists()) dir.mkpath(workingDir.canonicalPath() + "/LOGS");
    return dir;
}

QDir ProjectData::procDir(const QDir& workingDir) {
    QDir dir = QDir(workingDir.canonicalPath() + "/proc");
    if (!dir.exists()) dir.mkpath(workingDir.canonicalPath() + "/proc");
    return dir;
}

QString ProjectData::selectionDirfile() {
    return projectWorkingDir().canonicalPath() + "/2dx_merge_dirfile.dat";
}

QString ProjectData::evenSelectionDirfile() {
    return projectWorkingDir().canonicalPath() + "/2dx_merge_dirfile_even.dat";
}

QString ProjectData::oddSelectionDirfile() {
    return projectWorkingDir().canonicalPath() + "/2dx_merge_dirfile_odd.dat";
}

