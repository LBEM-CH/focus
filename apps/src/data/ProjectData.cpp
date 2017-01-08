
#include <QDebug>
#include <QtConcurrent>
#include <QtWidgets>

#include "ApplicationData.h"
#include "ParameterMaster.h"

#include "ProjectData.h"
#include "UserPreferences.h"
#include "ScriptSelectorDialog.h"
#include "ProcessDialog.h"
#include "UserPreferenceData.h"

ProjectData& ProjectData::Instance() {
    static ProjectData instance_;
    return instance_;
}

void ProjectData::initiailze(const QDir& projectDir) {
    autoSave_ = UserPreferences().autoSaveConfigs();
    projectDir_ = projectDir;
    ParameterMaster::registerParameterMaster(ApplicationData::masterCfgFile());
    ParameterMaster::registerParameterMaster(ApplicationData::userCfgFile());
    emit parametersRegistered();
    projectParameters_ = new ParametersConfiguration(ApplicationData::masterCfgFile(), projectWorkingDir().canonicalPath() + "/2dx_merge.cfg", this);
    indexImages(true);
    
    connect(&statusUploadTimer_, &QTimer::timeout, this, &ProjectData::uploadStatusData);
    
    if(userPreferenceData.get("status_refresh_rate").toInt() != 0) {
        statusUploadTimer_.start(userPreferenceData.get("status_refresh_rate").toInt()*60*1000);
    } else {
        statusUploadTimer_.stop();
    }
}

ProjectImage* ProjectData::addImage(const QString& group, const QString& directory) {
    QMap<QString, ProjectImage*> groupImages;
    if (projectImages_.contains(group)) {
        groupImages = projectImages_[group];
    }
    ProjectImage* image = new ProjectImage(group, directory, true, this);
    groupImages.insert(directory, image);
    projectImages_.insert(group, groupImages);
    emit imageAdded(image);
    emit imageCountChanged(projectImageList().count());
    return projectImage(group, directory);
}

void ProjectData::moveImage(ProjectImage* image, const QString& newPath) {
    if(image) {
        //Remove from the original path
        QString group, directory;
        group = image->group();
        directory = image->directory();
        QMap<QString, ProjectImage*> groupImages;
        if (projectImages_.contains(group)) {
            groupImages = projectImages_[group];
        }
        groupImages.remove(directory);
        if(groupImages.isEmpty()) projectImages_.remove(group);

        QStringList cells = newPath.split("/");
        directory = cells.last();
        cells.removeLast();
        group = cells.last();
        image->setGroup(group);
        image->setDirectory(directory);
        QMap<QString, ProjectImage*> newGroupImages;
        if (projectImages_.contains(group)) {
            newGroupImages = projectImages_[group];
        }
        newGroupImages.insert(directory, image);
        projectImages_.insert(group, newGroupImages);
        emit imageMoved(image);
        
    } else {
        qDebug() << "Wanted to move the image in library but the image was not found in project";
    }
}

void ProjectData::indexImages(bool init) {
    
    //Backup current images
    QMap<QString, QMap<QString, ProjectImage*>> currentProjectImages = projectImages_;
    projectImages_.clear();
    
    //Get a list of all possible groups
    QStringList groups = projectDir().entryList(QDir::NoDotAndDotDot | QDir::Dirs);
    foreach(QString group, groups) {
        if (!QFileInfo(projectDir().canonicalPath() + "/" + group + "/2dx_master.cfg").exists()) groups.removeAll(group);
    }
    
    //Get a list of all the images
    QList<ProjectImage*> uninitializedImages;
    int progress = 0;
    for(QString group : groups) {
        if(init) emit groupsInitializationStatus(QString::number(progress) + "/" + QString::number(groups.size()) + " groups done ...");
        QStringList directories = QDir(projectDir().canonicalPath() + "/" + group).entryList(QDir::NoDotAndDotDot | QDir::Dirs);
        for (QString directory : directories) {
            if (ProjectImage::cfgFileExist(group, directory)) {
                ProjectImage* localImage;
                if (currentProjectImages.contains(group) && currentProjectImages.value(group).contains(directory)) {
                    localImage = currentProjectImages[group][directory];
                } else {
                    //Do not read the cfg file for now, do it later in parallel
                    localImage = new ProjectImage(group, directory, false, this);
                    uninitializedImages.append(localImage);
                }
                QMap<QString, ProjectImage*> groupImages;
                if (projectImages_.contains(group)) {
                    groupImages = projectImages_[group];
                }
                groupImages.insert(directory, localImage);
                projectImages_.insert(group, groupImages);
            }
        }
        progress++;
    }
    
    if(init) emit groupsInitialized(uninitializedImages.size());

    QProgressDialog progressDialog;
    progressDialog.setRange(0, uninitializedImages.size());
    progressDialog.setValue(0);
    progressDialog.setWindowTitle("Initializing images");
    progressDialog.setLabelText("Initializing image parameters from " + QString::number(uninitializedImages.size()) + " images...");
    progressDialog.setCancelButton(0);
    
    //Load the parameters for the uninitialized images
    QFutureWatcher<void> futureWatcher;
    connect(&futureWatcher, &QFutureWatcher<void>::finished, &progressDialog, &QProgressDialog::reset);
    connect(&futureWatcher, &QFutureWatcher<void>::progressValueChanged, &progressDialog, &QProgressDialog::setValue);
    connect(&futureWatcher, &QFutureWatcher<void>::progressValueChanged, [&](int value){
        if(init) emit imageInitializationStatus(QString::number(value) + " images loaded");
    });

    // Start the loading.
    futureWatcher.setFuture(QtConcurrent::map(uninitializedImages, [=](ProjectImage* image) {
        //Read the cfg files and reset the parameters
        image->reloadParameters();
    }));
    
    progressDialog.exec();
    futureWatcher.waitForFinished();
    
    if(init) emit imagesInitialized();
    
    for(ProjectImage* image : projectImageList()) image->setParent(this);
    
    emit imagesReindexed();
    emit imageCountChanged(projectImageList().count());
}

QList<ProjectImage*> ProjectData::projectImageList() {
    QList<ProjectImage*> list;
    for(QString& group : projectImages_.keys()) {
        for(QString dir : projectImages_[group].keys()) {
            list.append(projectImages_[group][dir]);
        }
    }
    return list;
}


ProjectImage* ProjectData::projectImage(const QString& group, const QString& directory) {
    ProjectImage* localImage = 0;
    if (projectImages_.contains(group)) {
        QMap<QString, ProjectImage*> groupImages = projectImages_[group];
        if(groupImages.contains(directory)) localImage = groupImages[directory];
    }
    
    return localImage;
}

ProjectImage* ProjectData::projectImage(const QDir& workingDir) {
    QStringList cells = workingDir.canonicalPath().split("/");
    QString directory, group;
    if(cells.size() > 1) {
        directory = cells.last();
        cells.removeLast();
        group = cells.last();
    }
    return projectImage(group, directory);
}

ParametersConfiguration* ProjectData::parameterData(const QDir& workDir) {
    if (workDir.canonicalPath() == projectWorkingDir().canonicalPath()) return projectParameters_;
    
    ProjectImage* localImage = projectImage(workDir);
    
    if(!localImage) {
        qDebug() << "CRITICAL: Image" << workDir.canonicalPath() << "requested, but not present, program will crash!";
        return 0;
    }
    
    return localImage->parameters();
}

void ProjectData::reloadParameterData(const QDir& workDir) {
    ProjectImage* image = projectImage(workDir);
    if(image) image->reloadParameters();
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

void ProjectData::saveAsProjectDefault(ProjectImage* image) {
    if (QMessageBox::question(NULL,
            tr("Save as default?"), QString("Saving as project default will change master config file and set default values for all other new imported images in this project.\n\n") + 
            QString("NOTE that to change the parameters of already imported images, you will have to run RESET IMAGE CONFIGS script from PROJECT TOOLS tab.\n\n Proceed?"),
            tr("Yes"),
            tr("No"),
            QString(), 0, 1) == 0) {
        image->parameters()->saveAs(projectWorkingDir().canonicalPath() + "/2dx_merge.cfg", false);
        reloadProjectParameters();
    }  
}

QString ProjectData::projectName() {
    return ProjectPreferences().projectName();
}

void ProjectData::setProjectName(const QString& projectName) {
    ProjectPreferences().setProjectName(projectName);
    emit projectNameChanged(projectName);
}

ProjectMode ProjectData::projectMode() {
    return ProjectMode(ProjectPreferences().projectMode());
}

void ProjectData::changeProjectMode() {
    if(sureDialog("Change Project Mode?", QString("Are you sure to change the mode of the project?\n\n") +
            QString("This might delete some of the processed parameters if they are not present in the new mode\n\n") + 
            QString("After changing the project program will RESTART!\nProceed?"))
            ) {
        
        QList<ProjectMode> modes = ProjectMode::availableModes();
        QStringList modeStrings;
        for (ProjectMode mode : modes) modeStrings << mode.toString();
        
        bool ok;
        QString modeSelected = QInputDialog::getItem(NULL, "Select Project Mode", "Select the new mode from the list", modeStrings, projectMode().toInt(), false, &ok);
        if(ok && !modeSelected.isEmpty()) {
            if(modeSelected == projectMode().toString()) {
                QMessageBox::information(NULL, "Change Project Mode", "Project mode was same as before, nothing changed.");
                return;
            }
            
            ProjectMode newMode = projectMode();
            newMode.setModeFromString(modeSelected);
            ProjectPreferences().setProjectMode(newMode.toInt());
            QProcess::startDetached(ApplicationData::mainApp() + " " + projectData.projectDir().canonicalPath());
            qApp->closeAllWindows();
        }
    }
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

QList<ProjectImage*> ProjectData::imagesOpen() {
    QStringList imagePaths = ProjectPreferences().imagesOpen();
    QList<ProjectImage*> images;
    for(QString imPath : imagePaths) {
        ProjectImage* image = projectImage(QDir(imPath));
        if(image) images.append(image);
    }
    return images;
}

bool ProjectData::imageOpen(ProjectImage* image) {
    if(image && imagesOpen().contains(image)) return true;
    else return false;
}


void ProjectData::setImagesOpen(const QList<ProjectImage*>& images) {
    QStringList paths;
    for(ProjectImage* image : images) if(image) paths.append(image->group() + "/" + image->directory());
    ProjectPreferences().setImagesOpen(paths);
}

QList<ProjectImage*> ProjectData::imagesSelected() {
    return loadSelection(selectionDirfile());
}

QList<ProjectImage*> ProjectData::loadSelection(const QString& dirFileName) {
    QFile s(dirFileName);
    
    QList<ProjectImage*> selectedImages;
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Dirfile read failed: " << dirFileName;
    } else {
        while (!s.atEnd()) {
            QStringList cells = QString(s.readLine().simplified()).split('/');
            if(cells.size() > 1) {
                QString directory = cells.last();
                cells.removeLast();
                QString group = cells.last();
                ProjectImage* image = projectImage(group, directory);
                if(image) selectedImages.append(image);
            }
        }
        s.close();
    }
    return selectedImages;
}

void ProjectData::saveSelection(const QString& saveName) {
    if (QFileInfo(saveName).exists()) QFile::remove(saveName);
    QFile::copy(selectionDirfile(), saveName);
}

void ProjectData::setImagesSelected(const QList<ProjectImage*>& images) {
    QFile saveFile(selectionDirfile());
    QFile evenFile(evenSelectionDirfile());
    QFile oddFile(oddSelectionDirfile());
    if (!saveFile.open(QIODevice::WriteOnly | QIODevice::Text)) return;
    if (!evenFile.open(QIODevice::WriteOnly | QIODevice::Text)) return;
    if (!oddFile.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    for (ProjectImage* image : images) {
        QString toBeWritten = QString(image->group() + "/" + image->directory() + '\n');
        saveFile.write(toBeWritten.toLatin1());
        int evenOdd = image->parameters()->getVariant("image_evenodd").toInt();
        if(evenOdd == 1) evenFile.write(toBeWritten.toLatin1());
        if(evenOdd == 2) oddFile.write(toBeWritten.toLatin1());
    }
    
    saveFile.close();
    evenFile.close();
    oddFile.close();
    emit selectionChanged(images);
}

QStringList ProjectData::readParamList(const QString& file, bool convertToLower) {
    QFile s(ApplicationData::configDir().canonicalPath() + "/" + file);
    
    if (!s.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Params file read failed: " << ApplicationData::configDir().canonicalPath() + "/" + file;
        return QStringList();
    }

    QStringList params;
    while (!s.atEnd()) {
        QString line = s.readLine().simplified().trimmed();
        if(convertToLower) line = line.toLower();
        if(!line.isEmpty()) params << line;
    }
    s.close();
    
    return params;
}

QStringList ProjectData::fileNameParamList() {
    return readParamList("filename.params.list", false);
}

QStringList ProjectData::uniqueParamList() {
    return readParamList("unique.params.list", true);
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
        QList<ProjectImage*> list = projectImageList();
        int number = 1;
        for(ProjectImage* image : list) {
            image->parameters()->setForce("imagenumber", commitIntToStringLength(number++, 8)+"00");
        }
        
        projectParameterData()->setForce("import_imagenumber", commitIntToStringLength(number, 4));
    }
}

void ProjectData::assignEvenOdd() {
    if(sureDialog("Assign even odd?", "This will assign all the images to either even or odd group.\n\nProceed?")){
        QList<ProjectImage*> list = projectImageList();
        int number = 1;
        for(ProjectImage* image : list) {
            image->parameters()->setForce("image_evenodd", QString::number(number%2 + 1));
            number++;
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
        
        QList<ProjectImage*> selectedList = imagesSelected();
        
        QProgressDialog progressDialog;
        progressDialog.setRange(0, selectedList.size()); // -1 because a merge dir is present in project folder!!);
        progressDialog.setWindowTitle("Reseting Images");
        progressDialog.show();
        
        
        for(int i=0; i< selectedList.size(); ++i) {
            progressDialog.setValue(i+1);
            progressDialog.setLabelText(tr("Reseting image %1 of %2...").arg(i+1).arg(selectedList.size()));
            qApp->processEvents();

            if (progressDialog.wasCanceled()) break;
                    
            ProjectImage* image = selectedList[i];
            image->backup(4);
            image->resetWithMasterConfig();
        }
        
        progressDialog.reset();
        QMessageBox::information(NULL, "Parameters were reset", "All image parameter databases in the parameter files were reset to the default parameters for this project. If this was a mistake, you can still use the Backup or Restore Databases script to recover the last versions.");
    }
}

void ProjectData::writeStatisticsToStatusFolder(const QString& fileName, long timeInterval, long currentTime) {
    //Write to status folder if required
    if (userPreferenceData.get("status_folder_update") == "y" && QFileInfo(userPreferenceData.get("status_folder")).isDir()) {
        long currentMSecs = currentTime;

        //Write the time stamp in the last hour processed data
        QFile timesFile(userPreferenceData.get("status_folder") + "/" + fileName);
        QList<long> lastTimes;
        if (currentMSecs > QDateTime::currentMSecsSinceEpoch() - timeInterval) lastTimes << currentMSecs;

        if (timesFile.exists()) {
            if (timesFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
                while (!timesFile.atEnd()) {
                    long timeRead = QString(timesFile.readLine().simplified()).toLong();
                    if (timeRead > QDateTime::currentMSecsSinceEpoch() - timeInterval) lastTimes << timeRead;
                }
                timesFile.close();
            }
            timesFile.remove();
        }

        if (timesFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
            for (long lastTime : lastTimes) timesFile.write(QString(QString::number(lastTime) + "\n").toLatin1());
        }
        timesFile.close();
    }
}


void ProjectData::addSelectedToQueue(bool prioritize) {
    QList<ProjectImage*> selected = imagesSelected();
    
    if(scriptSelectorDialog.exec()) {
        QStringList scripts = scriptSelectorDialog.selectedScriptPaths();
        QMap<ProjectImage*, QStringList> imageAndScripts;
        for(ProjectImage* image : selected) {
            imageAndScripts.insert(image, scripts);
        }
        emit toBeAddedToProcessingQueue(imageAndScripts, prioritize);
        emit focusProcessingWindow();
    }
}

void ProjectData::addImageToQueue(ProjectImage* image, QStringList scripts, bool prioritize) {
    QMap<ProjectImage*, QStringList> imageAndScripts;
    imageAndScripts.insert(image, scripts);
    emit toBeAddedToProcessingQueue(imageAndScripts, prioritize);
}

void ProjectData::openImage(ProjectImage* image) {
    emit imageToBeOpened(image);
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

void ProjectData::emitStartupFinished() {
    emit startupFinished();
}

void ProjectData::emitLibraryLoaded() {
    emit libraryLoaded();
}

void ProjectData::uploadStatusData() {
    if(statusUploadProcess_.state() == QProcess::Running) {
        qDebug() << "Already uploading data, skipping this upload";
        return;
    }
    
    if(userPreferenceData.get("status_refresh_rate").toInt() == 0) return;
    
    if (userPreferenceData.get("status_folder_update") == "y" && QFileInfo(userPreferenceData.get("status_folder")).isDir()) {
        QString executionString = ApplicationData::webScriptsDir().canonicalPath() + "/upload_status ";
        executionString += " " + userPreferenceData.get("app_cadaver");
        executionString += " " + userPreferenceData.get("status_webdav");
        executionString += " " + userPreferenceData.get("status_microscope");
        executionString += " " + userPreferenceData.get("status_folder");
        statusUploadProcess_.start('"' + executionString + '"', QIODevice::ReadOnly);
    }
}
