
#ifndef PROJECT_IMAGES_H
#define PROJECT_IMAGES_H

#include <iostream>

#include <QSettings>
#include <QString>
#include <QStringList>

#include "confData.h"

class ProjectPreferences : public QSettings {
public:
    
    ProjectPreferences(const QString& projectPath) 
    :QSettings(projectPath + "/merge/config/project.preferences.ini", QSettings::Format::IniFormat) { 
    }

    ProjectPreferences(confData* data)
    : QSettings(data->getDir("project") + "/merge/config/project.preferences.ini", QSettings::Format::IniFormat) {
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

};


#endif /* PROJECT_IMAGES_H */

