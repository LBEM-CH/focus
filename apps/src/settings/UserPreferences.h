
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

    void saveWindowPreferences(QMainWindow* window) {
        beginGroup("window");
        setValue("size", window->size());
        setValue("position", window->pos());
        endGroup();
    }

    void loadWindowPreferences(QMainWindow* window) {
        beginGroup("window");
        window->resize(value("size", QSize(896, 504)).toSize());
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

};

#endif /* USER_PREFERENCES_H */
