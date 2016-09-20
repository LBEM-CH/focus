
#ifndef PROJECT_DATA_H
#define PROJECT_DATA_H

#include <QObject>
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

class ProjectData : public QObject {
    
    Q_OBJECT
    
public:

    static ProjectData& Instance();

    void initiailze(const QDir& projectDir);
    
    ParametersConfiguration* projectParameterData();
    void reloadProjectParameters();
    ParametersConfiguration* parameterData(const QDir& workDir);
    void reloadParameterData(const QDir& workDir);
    
    void indexImages();
    void addImage(const QDir& imageDir);
    QStringList imageList();
    
    QStringList imagesOpen();
    bool imageOpen(const QString path);
    void setImagesOpen(const QStringList& paths);
    
    QDir projectDir() const;
    QDir projectWorkingDir() const;

    QString projectName();
    void setProjectName(const QString& projectName);
    
    void toggleAutoSave();
    bool isAutoSave();
    
    void saveAsProjectDefault(const QDir& workingDir);

    static QDir logsDir(const QDir& workingDir);
    static QDir procDir(const QDir& workingDir);
    
    QString selectionDirfile();
    QString evenSelectionDirfile();
    QString oddSelectionDirfile();
    
signals:
    void imageDirsChanged();

private:

    ProjectData() {
    }
    
    void registerParameterMaster(const QString& cfgFileName);

    void initializeImageParameters(const QDir& currDir, QStringList& imageList, QProgressDialog& dialog);

    QDir projectDir_;
    ParametersConfiguration* projectParameters_;
    QMap<QString, ParametersConfiguration*> imageToParameterData_;
    bool autoSave_=true;
    
};


#endif /* PROJECT_DATA_H */

