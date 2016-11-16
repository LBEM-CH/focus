
#ifndef SCRIPT_MODULE_PROPS_H
#define SCRIPT_MODULE_PROPS_H

#include <QSettings>
#include "ProjectPreferences.h"

class ScriptModuleProperties : public QSettings {
public:

    ScriptModuleProperties(const QString& directory)
    : QSettings(directory + "/module.ini", QSettings::Format::IniFormat) {
    }

    QString title() {
        QString val;
        beginGroup("module");
        val = value("title").toString();
        endGroup();
        return val;
    }
    
    QString icon() {
        QString val;
        beginGroup("module");
        val = value("icon").toString();
        endGroup();
        return val;
    }
    
    QString scriptIcon() {
        QString val;
        beginGroup("module");
        val = value("scriptIcon", "scriptIcon").toString();
        endGroup();
        return val;
    }
    
    QString selection() {
        QString val;
        beginGroup("module");
        val = value("selection", "single").toString();
        endGroup();
        return val;
    }
    
    bool defaultActivated() {
        bool val;
        beginGroup("module");
        val = value("defaultActivated", false).toBool();
        endGroup();
        return val;
    }
    
    QStringList subfolders() {
        QStringList paths;
        int size = beginReadArray("subfolders");
        for (int i = 0; i < size; ++i) {
            setArrayIndex(i);
            QString projectMode = QString::number(ProjectPreferences().projectMode());
            if(value("mode").toString() == "ALL" || value("mode").toString().contains(projectMode)) {
                QString path = QFileInfo(fileName()).path() + '/' + value("path").toString();
                if (QFileInfo(path + "/module.ini").exists() && !(path.isEmpty())) {
                    paths << path;
                }
            }
        }
        endArray();
        return paths;
    }

};

#endif /* USER_PREFERENCES_H */

