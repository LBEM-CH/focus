
#ifndef PROJECT_DATA_H
#define PROJECT_DATA_H

#include <QString>
#include <QStringList>
#include <QDebug>
#include <QDir>
#include <QFileInfo>
#include <QApplication>
#include <QProgressDialog>
#include <QMap>

#include "ProjectPreferences.h"
#include "ParameterConfiguration.h"

#define projectData (ProjectData::Instance())

class ProjectData {
public:

    static ProjectData& Instance();

    void initiailze(const QDir& projectDir);
    
    ParametersConfiguration* projectParameterData();
    ParametersConfiguration* parameterData(const QDir& workDir);
    
    void indexImages();
    QStringList imageList();
    
    QStringList imagesOpen();
    void setImagesOpen(const QStringList& paths);
    
    QDir projectDir() const;
    QDir projectWorkingDir() const;

    QString projectName();
    void setProjectName(const QString& projectName);
    
    void saveAsProjectDefault(const QDir& workingDir);

    static QDir logsDir(const QDir& workingDir);
    static QDir procDir(const QDir& workingDir);
    
    QString selectionDirfile();
    QString evenSelectionDirfile();
    QString oddSelectionDirfile();


private:

    ProjectData() {
    }
    
    void registerParameterMaster(const QString& cfgFileName);

    void initializeImageParameters(const QDir& currDir, QStringList& imageList, QProgressDialog& dialog);

    QDir projectDir_;
    ParametersConfiguration* projectParameters_;
    QMap<QString, ParametersConfiguration*> imageToParameterData_;

};


#endif /* PROJECT_DATA_H */

