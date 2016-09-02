
#ifndef SCRIPT_MODULE_PROPS_H
#define SCRIPT_MODULE_PROPS_H

#include <QSettings>

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

};

#endif /* USER_PREFERENCES_H */

