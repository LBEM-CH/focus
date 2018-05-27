#ifndef IMPORTFOLDERSETTINGS_H
#define IMPORTFOLDERSETTINGS_H

#include <QSettings>
#include <QApplication>
#include <QFont>
#include <QString>
#include <QStringList>
#include <QDir>
#include <QFileInfo>

#include "ApplicationData.h"
#include "ProjectData.h"

class ImportFolderSettings : public QSettings {
public:

    ImportFolderSettings(const QDir& importFolder)
    : QSettings(projectData.projectDir().canonicalPath() + "/merge/config/project.imported.ini", QSettings::Format::IniFormat), importDir_(importFolder) {
    }

    void addImportedImage(const QString& image, const QString& importedDir, bool hadAveraged, bool hadAligned, bool hadRaw, bool hadXML) {
        beginGroup(importDir_.canonicalPath());
        beginGroup(image);
        setValue("directory", importedDir);
        setValue("hadAveraged", hadAveraged);
        setValue("hadAligned", hadAligned);
        setValue("hadRaw", hadRaw);
        setValue("hadXML", hadXML);
        endGroup();
        endGroup();
    }

    QStringList importedNames() {
        beginGroup(importDir_.canonicalPath());
        QStringList names = childGroups();
        // qDebug()<<" names = "<<names;
        endGroup();
        return names;
    }

    QStringList importedFullNames() {
        beginGroup(importDir_.canonicalPath());
        QStringList names = allKeys();
        // qDebug()<<" names = "<<names;
        endGroup();
        return names;
    }

    QString linkedDirectory(const QString& imageName) {
        beginGroup(importDir_.canonicalPath());
        beginGroup(imageName);
        QString val = value("directory").toString();
        endGroup();
        endGroup();
        return val;
    }

    bool hadAveraged(const QString& imageName) {
        beginGroup(importDir_.canonicalPath());
        beginGroup(imageName);
        bool val = value("hadAveraged", false).toBool();
        endGroup();
        endGroup();
        return val;
    }

    bool hadAligned(const QString& imageName) {
        beginGroup(importDir_.canonicalPath());
        beginGroup(imageName);
        bool val = value("hadAligned", false).toBool();
        endGroup();
        endGroup();
        return val;
    }

    bool hadRaw(const QString& imageName) {
        beginGroup(importDir_.canonicalPath());
        beginGroup(imageName);
        bool val = value("hadRaw", false).toBool();
        endGroup();
        endGroup();
        return val;
    }

    bool hadXML(const QString& imageName) {
        beginGroup(importDir_.canonicalPath());
        beginGroup(imageName);
        bool val = value("hadXML", false).toBool();
        endGroup();
        endGroup();
        return val;
    }

private:
    QDir importDir_;

};

#endif /* IMPORTFOLDERSETTINGS_H */

