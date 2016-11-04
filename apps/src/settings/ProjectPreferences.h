
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

    ProjectPreferences(const QDir& projectDir)
    : QSettings(projectDir.canonicalPath() + "/merge/config/project.preferences.ini", QSettings::Format::IniFormat) {
    }
    
    void setImagesOpen(const QStringList& paths) {
        remove("open");
        beginWriteArray("open");
        for (int i = 0; i < paths.size(); ++i) {
            setArrayIndex(i);
            setValue("path", paths.at(i));
        }
        endArray();
    }

    QStringList imagesOpen() {
        QStringList paths;
        int size = beginReadArray("open");
        for (int i = 0; i < size; ++i) {
            setArrayIndex(i);
            if (value("path").toString() != "") paths << value("path").toString();
        }
        endArray();
        return paths;
    }

    void setProjectName(const QString& name) {
        beginGroup("project");
        setValue("name", name);
        endGroup();
    }

    QString projectName() {
        QString name;
        beginGroup("project");
        name = value("name", "Unnamed Project").toString();
        endGroup();
        return name;
    }
    
    void setOverviewIndex(int jobs) {
        beginGroup("project");
        setValue("overviewIndex", jobs);
        endGroup();
    }
    
    int overviewIndex() {
        int val;
        beginGroup("project");
        val = value("overviewIndex", 0).toInt();
        endGroup();
        return val;
    }
    
    void setImportContinuousCheck(bool check) {
        beginGroup("import");
        setValue("continuous", check);
        endGroup();
    }
    
    bool importContinuousCheck() {
        bool val;
        beginGroup("import");
        val = value("continuous", false).toBool();
        endGroup();
        return val;
    }
    
    void setImportRestartCheck(bool check) {
        beginGroup("import");
        setValue("onRestart", check);
        endGroup();
    }
    
    bool importRestartCheck() {
        bool val;
        beginGroup("import");
        val = value("onRestart", false).toBool();
        endGroup();
        return val;
    }
        
    void setProcessScripts(const QStringList& scripts) {
        beginGroup("process");
        remove("scripts");
        beginWriteArray("scripts");
        for (int i = 0; i < scripts.size(); ++i) {
            setArrayIndex(i);
            setValue("name", scripts.at(i));
        }
        endArray();
        endGroup();
    }
    
    QStringList processScripts() {
        beginGroup("process");
        QStringList paths;
        int size = beginReadArray("scripts");
        for (int i = 0; i < size; ++i) {
            setArrayIndex(i);
            if (value("name").toString() != "") paths << value("name").toString();
        }
        endArray();
        endGroup();
        return paths;
    }
    
    void setProcessJobs(int jobs) {
        beginGroup("process");
        setValue("jobs", jobs);
        endGroup();
    }
    
    int processJobs() {
        int val;
        beginGroup("process");
        val = value("jobs", 2).toInt();
        endGroup();
        return val;
    }
    
};


#endif /* PROJECT_IMAGES_H */

