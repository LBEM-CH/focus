
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
    
    void resetImageList(const QStringList& paths) {
        remove("images");
        beginWriteArray("images");
        int index = 0;
        for (int i = 0; i < paths.size(); ++i) {
            if (QFileInfo(paths.at(i) + "/2dx_image.cfg").exists()) {
                setArrayIndex(index);
                setValue("path", paths.at(i));
                index++;
            }
        }
        endArray();
    }
    
    void addImage(const QString& path) {
        QStringList paths = imageList();
        paths.push_front(QDir(path).absolutePath());
        paths.removeDuplicates();
        resetImageList(paths);
    }
        
    QStringList imageList() {
        QStringList paths;
        int size = beginReadArray("images");
        for (int i = 0; i < size; ++i) {
            setArrayIndex(i);
            if (value("path").toString() != "") paths << value("path").toString();
        }
        endArray();
        return paths;
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
        setValue("mainDir", path);
        endGroup();
    }

    QString importImageDir() {
        beginGroup("import");
        QString val = value("mainDir", QFileInfo(fileName()).filePath()).toString();
        endGroup();
        return val;
    }

    void setImportRawFolder(const QString& path) {
        beginGroup("import");
        setValue("raw", path);
        endGroup();
    }

    QString importRawFolder() {
        beginGroup("import");
        QString val = value("raw", "raw").toString();
        endGroup();
        return val;
    }
    
    void setImportAlignedFolder(const QString& path) {
        beginGroup("import");
        setValue("aligned", path);
        endGroup();
    }

    QString importAlignedFolder() {
        beginGroup("import");
        QString val = value("aligned", "aligned").toString();
        endGroup();
        return val;
    }
    
    void setImportAveragedFolder(const QString& path) {
        beginGroup("import");
        setValue("averaged", path);
        endGroup();
    }

    QString importAveragedFolder() {
        beginGroup("import");
        QString val = value("averaged", "averaged").toString();
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
        QString val = value("group", "auto").toString();
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
        int val = value("length", 5).toInt();
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
    
    void setImportScripts(const QStringList& scripts) {
        beginGroup("import");
        remove("scripts");
        beginWriteArray("scripts");
        for (int i = 0; i < scripts.size(); ++i) {
            setArrayIndex(i);
            setValue("name", scripts.at(i));
        }
        endArray();
        endGroup();
    }
    
    QStringList importScripts() {
        beginGroup("import");
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

};


#endif /* PROJECT_IMAGES_H */

