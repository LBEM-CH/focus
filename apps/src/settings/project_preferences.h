
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
    : QSettings(projectPath + "/merge/config/project.preferences.ini", QSettings::Format::IniFormat) {
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

    void setImportImageDir(const QString& path) {
        beginGroup("import");
        setValue("image", path);
        endGroup();
    }

    QString importImageDir() {
        beginGroup("import");
        QString val = value("image").toString();
        endGroup();
        return val;
    }

    void setImportMoiveDir(const QString& path) {
        beginGroup("import");
        setValue("movie", path);
        endGroup();
    }

    QString importMovieDir() {
        beginGroup("import");
        QString val = value("movie").toString();
        endGroup();
        return val;
    }

    void setImportGroup(const QString& group) {
        beginGroup("import");
        setValue("group", group);
        endGroup();
    }

    QString importGroup() {
        beginGroup("import");
        QString val = value("group").toString();
        endGroup();
        return val;
    }

    void setImportIgnorePattern(const QString& ignore) {
        beginGroup("import");
        setValue("ignore", ignore);
        endGroup();
    }

    QString importIgnorePattern() {
        beginGroup("import");
        QString val = value("ignore").toString();
        endGroup();
        return val;
    }

    void setImportImageLength(int length) {
        beginGroup("import");
        setValue("length", length);
        endGroup();
    }

    int importImageLength() {
        beginGroup("import");
        int val = value("length").toInt();
        endGroup();
        return val;
    }
    
    void setLastImageNumber(int number) {
        beginGroup("import");
        setValue("lastnumber", number);
        endGroup();
    }

    int lastImageNumber() {
        beginGroup("import");
        int val = value("lastnumber", 0).toInt();
        endGroup();
        return val;
    }

};


#endif /* PROJECT_IMAGES_H */

