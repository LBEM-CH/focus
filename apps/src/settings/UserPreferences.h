
#ifndef USER_PREFERENCES_H
#define USER_PREFERENCES_H

#include <QSettings>
#include <QApplication>
#include <QFont>
#include <QString>
#include <QStringList>
#include <QSize>
#include <QPoint>
#include <QMainWindow>
#include <QDialog>

#include "ApplicationData.h"

class UserPreferences : public QSettings {
public:

    UserPreferences()
    : QSettings(ApplicationData::homeDir().absolutePath() + "/preferences.ini", QSettings::Format::IniFormat) {
    }

    void setFontSize(int value) {
        beginGroup("appearance");
        beginGroup("font");
        setValue("size", value);
        endGroup();
        endGroup();
        loadAllFontSettings();
    }

    void setFontWeight(const QString& value) {
        beginGroup("appearance");
        beginGroup("font");
        setValue("weight", value.toInt());
        endGroup();
        endGroup();
        loadAllFontSettings();
    }

    void saveCurrentFontSettings() {
        beginGroup("appearance");
        beginGroup("font");
        QFont font = QApplication::font();
        setValue("size", font.pointSize());
        setValue("weight", font.weight());
        endGroup();
        endGroup();
    }

    void loadAllFontSettings() {
        QFont font = QApplication::font();
        beginGroup("appearance");
        beginGroup("font");
        font.setPointSize(value("size", font.pointSize()).toInt());
        font.setWeight(value("weight", font.weight()).toInt());
        endGroup();
        endGroup();
        QApplication::setFont(font);
    }

    void saveMainWindowPreferences(QMainWindow* window) {
        beginGroup("window");
        setValue("size", window->size());
        setValue("position", window->pos());
        endGroup();
    }

    void loadMainWindowPreferences(QMainWindow* window) {
        beginGroup("window");
        window->resize(value("size", QSize(896, 504)).toSize());
        window->move(value("position", QPoint(200, 200)).toPoint());
        endGroup();
    }
    
    void saveStarupDialogPreferences(QDialog* window) {
        beginGroup("startup-dialog");
        setValue("position", window->pos());
        endGroup();
    }

    void loadStarupDialogPreferences(QDialog* window) {
        beginGroup("startup-dialog");
        window->move(value("position", QPoint(200, 200)).toPoint());
        endGroup();
    }
    
    void setRemindUpdate(const QString& update) {
        beginGroup("software");
        setValue("remindUpdate", update);
        endGroup();
    }
    
    QString getRemindUpdate() {
        QString update;
        beginGroup("software");
        update = value("remindUpdate", "y").toString();
        endGroup();
        return update;
    }
    
    void setAutoSaveConfigs(bool save) {
        beginGroup("gui-general");
        setValue("autosave", save);
        endGroup();
    }
    
    bool autoSaveConfigs() {
        bool save;
        beginGroup("gui-general");
        save = value("autosave", true).toBool();
        endGroup();
        return save;
    }
    
    void setShowAdvanced(bool show) {
        beginGroup("gui-general");
        setValue("advanced", show);
        endGroup();
    }
    
    bool showAdvanced() {
        bool save;
        beginGroup("gui-general");
        save = value("advanced", false).toBool();
        endGroup();
        return save;
    }
    
    void setUserLevel(int level) {
        beginGroup("gui-general");
        setValue("userLevel", level);
        endGroup();
    }
    
    int userLevel() {
        int level;
        beginGroup("gui-general");
        level = value("userLevel", 1).toInt();
        endGroup();
        return level;
    }
    

};

#endif /* USER_PREFERENCES_H */

