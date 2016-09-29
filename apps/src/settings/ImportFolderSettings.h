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

class ImportFolderSettings : public QSettings {
public:
    
    ImportFolderSettings(const QDir& importFolder)
    : QSettings(importFolder.absolutePath() + "/.2dx.imported.ini", QSettings::Format::IniFormat) {
    }

    void addImportedImage(const QString& image, const QString& importedDir, bool hadAligned, bool hadRaw) {
        beginGroup("imported");
        beginGroup(image);
        setValue("directory", importedDir);
        setValue("hadAligned", hadAligned);
        setValue("hadRaw", hadRaw);
        endGroup();
        endGroup();
    }

    QStringList importedNames() {
        beginGroup("imported");
        QStringList names = childGroups();
        endGroup();
        return names;
    }
    
    QString linkedDirectory(const QString& imageName) {
        beginGroup("imported");
        beginGroup(imageName);
        QString val = value("directory").toString();
        endGroup();
        endGroup();
        return val;
    }
    
    bool hadAligned(const QString& imageName) {
        beginGroup("imported");
        beginGroup(imageName);
        bool val = value("hadAligned", false).toBool();
        endGroup();
        endGroup();
        return val;
    }
    
    bool hadRaw(const QString& imageName) {
        beginGroup("imported");
        beginGroup(imageName);
        bool val = value("hadRaw", false).toBool();
        endGroup();
        endGroup();
        return val;
    }


};

#endif /* IMPORTFOLDERSETTINGS_H */

