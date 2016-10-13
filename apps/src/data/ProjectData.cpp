
#include <QDebug>
#include <QtConcurrent>
#include <QtWidgets>
#include <QtGui/qprogressdialog.h>

#include "ApplicationData.h"
#include "ParameterMaster.h"

#include "ProjectData.h"
#include "UserPreferences.h"

ProjectData& ProjectData::Instance() {
    static ProjectData instance_;
    return instance_;
}

void ProjectData::initiailze(const QDir& projectDir) {
    autoSave_ = UserPreferences().autoSaveConfigs();
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
    valueSearch << "LABEL" << "LEGEND" << "EXAMPLE" << "HELP" << "TYPE" << "RELATION" << "USERLEVEL" << "LOCKED" << "SYNC_WITH_UPPER_LEVEL";
    
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
    QProgressDialog* progressDialog = new QProgressDialog();
    progressDialog->setCancelButton(0);
    progressDialog->setRange(0, projectDir().entryList().count() - 1); // -1 because a merge dir is present in project folder!!);
    progressDialog->setWindowTitle("Scanning images");
    initializeImageParameters(projectDir(), imageList, progressDialog);
    ProjectPreferences(projectDir()).resetImageList(imageList);

    progressDialog->reset();
    progressDialog->setCancelButton(0);
    progressDialog->setRange(0, imageList.size()+1);
    progressDialog->setWindowTitle("Loading image parameters");
    progressDialog->setMinimumDuration(0);
    progressDialog->setValue(0);
    QMutex* mutex = new QMutex();
    QFuture<void> future = QtConcurrent::map(imageList, [=](const QString& imPath) {
        QString configFile = imPath + "/2dx_image.cfg";
        if (QFileInfo(configFile).exists()) {
            ParametersConfiguration* localData = new ParametersConfiguration(ApplicationData::masterCfgFile(), configFile, projectParameters_);
            mutex->lock();
            imageToParameterData_.insert(QFileInfo(configFile).canonicalPath(), localData);
            progressDialog->setLabelText("Working on image " + QString::number(progressDialog->value()) + " of " + QString::number(imageList.size()) + "...");
            progressDialog->setValue(progressDialog->value()+1);
            mutex->unlock();
        }
    });
    future.waitForFinished();
    
    progressDialog->close();
    
    delete mutex;
    delete progressDialog;
    emit imageDirsChanged();
}

void ProjectData::initializeImageParameters(const QDir& currDir, QStringList& imageList, QProgressDialog* dialog) {
    QDir dir = currDir;
    dir.setFilter(QDir::NoDotAndDotDot | QDir::Dirs);

    if (dir.relativeFilePath(projectDir().canonicalPath()) == "../") {
        dialog->setValue(dialog->value()+1);
    }
    dialog->setLabelText("Scanning folder for images:\n" + currDir.canonicalPath());
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

    QDir procdirectory(workDir.canonicalPath() + "/proc");
    if (!procdirectory.exists()) procdirectory.mkdir(workDir.canonicalPath() + "/proc");
    QDir logdirectory(workDir.canonicalPath() + "/LOGS");
    if (!logdirectory.exists()) logdirectory.mkdir(workDir.canonicalPath() + "/LOGS");
    
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
            tr("Save as default?"), QString("Saving as project default will change master config file and set default values for all other new imported images in this project.\n\n") + 
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
    emit projectNameChanged(projectName);
}

void ProjectData::changeProjectName() {
    bool ok;
    QString projectName = QInputDialog::getText(NULL, "Project Name", "Enter a name for the project", QLineEdit::Normal,
            projectData.projectName(), &ok);

    if (ok && !projectName.isEmpty()) {
        projectData.setProjectName(projectName);
    }
}

void ProjectData::toggleAutoSave() {
    autoSave_ = !autoSave_;

    if (autoSave_) {
        QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched on"));
    } else {
        QMessageBox::information(NULL, tr("Automatic Saving"), tr("Automatic Saving is now switched off"));
    }
}

void ProjectData::setAutoSave(bool save) {
    autoSave_ = save;
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

QStringList ProjectData::imagesSelected() {
    return loadSelection(selectionDirfile());
}

QStringList ProjectData::loadSelection(const QString& dirFileName) {
    QFile s(dirFileName);
    
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Dirfile read failed: " << dirFileName;
        return QStringList();
    }

    QStringList selectedImages;
    QString projectDir = projectData.projectDir().canonicalPath();
    while (!s.atEnd()) selectedImages << projectDir + '/' + s.readLine().simplified();
    s.close();
    
    return selectedImages;
}

void ProjectData::saveSelection(const QString& saveName) {
    if (QFileInfo(saveName).exists()) QFile::remove(saveName);
    QFile::copy(selectionDirfile(), saveName);
}


void ProjectData::setImagesSelected(const QStringList& paths) {
    QFile saveFile(selectionDirfile());
    QFile evenFile(evenSelectionDirfile());
    QFile oddFile(oddSelectionDirfile());
    if (!saveFile.open(QIODevice::WriteOnly | QIODevice::Text)) return;
    if (!evenFile.open(QIODevice::WriteOnly | QIODevice::Text)) return;
    if (!oddFile.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    for (int i = 0; i < paths.size(); ++i) {
        if(parameterData(QDir(paths[i]))) {
            QString toBeWritten = QString(projectData.projectDir().relativeFilePath(paths[i]) + '\n');
            saveFile.write(toBeWritten.toLatin1());
            if(parameterData(QDir(paths[i]))->get("image_evenodd")->value().toInt() == 1) evenFile.write(toBeWritten.toLatin1());
            if(parameterData(QDir(paths[i]))->get("image_evenodd")->value().toInt() == 2) oddFile.write(toBeWritten.toLatin1());
        }
    }
    
    saveFile.close();
    evenFile.close();
    oddFile.close();
    
    emit selectionChanged(paths);
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

void ProjectData::renumberImages() {
    if(sureDialog("Renumber Images?", "This will renumber all the images.\n\nProceed?")){
        QStringList list = imageList();
        int number = 1;
        for(QString image : list) {
            if(parameterData(QDir(image))) {
                parameterData(QDir(image))->setForce("imagenumber", commitIntToStringLength(number++, 8)+"00");
            }
        }
        
        projectParameterData()->setForce("import_imagenumber", commitIntToStringLength(number, 4));
    }
}

void ProjectData::assignEvenOdd() {
    if(sureDialog("Assign even odd?", "This will assign all the images to either even or odd group.\n\nProceed?")){
        QStringList list = imageList();
        int number = 1;
        for(QString image : list) {
            if(parameterData(QDir(image))) {
                parameterData(QDir(image))->setForce("image_evenodd", QString::number(number%2 + 1));
                number++;
            }
        }
    }
}

void ProjectData::repairLinks() {
    if(sureDialog("Repair Project Links", "This will look for all possible image folders and will correctly link them so that 2DX can read these images.\n\nProceed?")) {
        linkProjectConfig("merge/2dx_merge.cfg", projectDir().absolutePath() + "/2dx_master.cfg");
        foreach(QString entry, projectDir().entryList(QDir::NoDotAndDotDot | QDir::Dirs)) {
            if(entry != "merge") linkProjectConfig("../2dx_master.cfg", projectDir().absolutePath() + '/' + entry + "/2dx_master.cfg");
        }
        
        indexImages();
    }
} 

void ProjectData::resetImageConfigs() {
    if(sureDialog("Reset image parameters with project?", "This will replace all the parameters in selected images with that of project.\n\nCAREFUL: You will loose all the processed results.\n\nARE YOU SURE TO Proceed?")) {
        
        QStringList selectedList = imagesSelected();
        
        QProgressDialog progressDialog;
        progressDialog.setRange(0, selectedList.size()); // -1 because a merge dir is present in project folder!!);
        progressDialog.setWindowTitle("Reseting Images");
        progressDialog.show();
        
        
        for(int i=0; i< selectedList.size(); ++i) {
            
            progressDialog.setValue(i+1);
            progressDialog.setLabelText(tr("Reseting image %1 of %2...").arg(i+1).arg(selectedList.size()));
            qApp->processEvents();

            if (progressDialog.wasCanceled()) break;
            
            QString selected = selectedList[i];
            
            QFile(selected + "/2dx_image.cfg").copy(selected + "/2dx_image.cfg-backup4"); 
            
            ParametersConfiguration* conf  = parameterData(selected);
            
            if(conf) {
                //Copy some original values
                QString imageName = conf->getValue("imagename");
                QString nonMaskName = conf->getValue("nonmaskimagename");
                QString imageNumber = conf->getValue("imagenumber");
                QString origName = conf->getValue("imagename_original");
                QString stackName = conf->getValue("movie_stackname");
                QString rawName = conf->getValue("movie_stackname_raw");
                QString importstackName = conf->getValue("import_rawstack");
                QString importgainref   = conf->getValue("import_gainref");
                QString importgaincorrectedstack = conf->getValue("import_gaincorrectedstack");

                projectParameterData()->saveAs(selected + "/2dx_image.cfg", true);
                conf->reload();
                
                //Reset orig values
                conf->set("imagename", imageName, false);
                conf->set("nonmaskimagename", nonMaskName, false);
                conf->set("imagenumber", imageNumber, false);
                conf->set("imagename_original", origName, false);
                conf->set("movie_stackname_raw", rawName, false);
                conf->set("movie_stackname", stackName, false);
                conf->set("import_rawstack", importstackName, false);
                conf->set("import_gainref", importgainref, false);
                conf->set("import_gaincorrectedstack", importgaincorrectedstack, false);
                
                conf->setModified(true);
            }
        }
        
        progressDialog.reset();
        QMessageBox::information(NULL, "Parameters were reset", "All image parameter databases in the files 2dx_image.cfg were reset to the default parameters for this project. If this was a mistake, you can still use the Backup or Restore Databases script to recover the last versions.");
    }
}


bool ProjectData::sureDialog(const QString& title, const QString& text) {
    if(QMessageBox::question(NULL, title, text, "Yes", "No", QString(), 0, 1) == 0){
        return true;
    }
    else return false;
}

QString ProjectData::commitIntToStringLength(int num, int length) {
    QString value = QString::number(num);
    int diff = length - value.size();
    for (int i = 0; i < diff; ++i) {
        value.push_front('0');
    }
    return value;
}

void ProjectData::linkProjectConfig(const QString& sourceName, const QString& targetLinkName) {
    if(QFileInfo(targetLinkName).exists()) {
        QFile(targetLinkName).remove();
    }
    
    QFile::link(sourceName, targetLinkName);
}





