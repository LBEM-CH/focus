
#ifndef PROJECT_IMAGES_H
#define PROJECT_IMAGES_H

#include <iostream>

#include <QDir>
#include <QSettings>
#include <QString>
#include <QStringList>
#include <QFileInfo>

class ProjectPreferences : public QSettings {
public:

    ProjectPreferences();
    
    ProjectPreferences(const QDir& projectDir);
    
    void setImagesOpen(const QStringList& paths);
    QStringList imagesOpen();

    void setProjectName(const QString& name);
    QString projectName();
    
    void setOverviewIndex(int jobs);
    int overviewIndex();
    
    void setImportRestartCheck(bool check);
    bool importRestartCheck();
    
    void setImportDeleteCheck(bool check);
    bool importDeleteCheck();
    
    void setImportSafeInterval(int seconds);
    int importSafeInterval();
        
    void setProcessScripts(const QStringList& scripts);
    QStringList processScripts();
    
    void setProcessJobs(int jobs);
    int processJobs();
    
    void setProcessAutoCheck(bool check);
    bool processAutoCheck();
    
};

#endif /* PROJECT_IMAGES_H */
