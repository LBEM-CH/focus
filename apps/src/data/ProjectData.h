
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
    
    QStringList imagesSelected();
    QStringList loadSelection(const QString& dirFileName);
    void saveSelection(const QString& newDirFileName);
    void setImagesSelected(const QStringList& paths);
    
    QDir projectDir() const;
    QDir projectWorkingDir() const;

    QString projectName();
    void setProjectName(const QString& projectName);
    
    void toggleAutoSave();
    bool isAutoSave();
    
    void saveAsProjectDefault(const QDir& workingDir);

    static QDir logsDir(const QDir& workingDir);
    static QDir procDir(const QDir& workingDir);
    
    //Tasks
    void renumberImages();
    void AssignEvenOdd();
    
signals:
    void imageDirsChanged();
    void selectionChanged(const QStringList& paths);

private:

    ProjectData() {
    }
    
    void registerParameterMaster(const QString& cfgFileName);
    void initializeImageParameters(const QDir& currDir, QStringList& imageList, QProgressDialog& dialog);
    
    bool sureDialog(const QString& title, const QString& text);
    
    QString commitIntToStringLength(int num, int length);
    
    QString selectionDirfile();
    QString evenSelectionDirfile();
    QString oddSelectionDirfile();

    QDir projectDir_;
    ParametersConfiguration* projectParameters_;
    QMap<QString, ParametersConfiguration*> imageToParameterData_;
    bool autoSave_=true;
    
};


#endif /* PROJECT_DATA_H */
