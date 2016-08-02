/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   user_preferences.h
 * Author: biyanin
 *
 * Created on August 2, 2016, 8:43 PM
 */

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

#include "confData.h"

class UserPreferences : public QSettings {
public:

    UserPreferences(confData* data)
    : QSettings(data->getDir("home_2dx") + "/preferences.ini", QSettings::Format::IniFormat) {
    }

    void setFontSize(const QString& value) {
        beginGroup("appearance");
        beginGroup("font");
        setValue("size", value.toInt());
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

    void setFontFamily(const QString& value) {
        beginGroup("appearance");
        beginGroup("font");
        setValue("family", value);
        endGroup();
        endGroup();
        loadAllFontSettings();
    }

    void saveCurrentFontSettings() {
        beginGroup("appearance");
        beginGroup("font");
        QFont font = QApplication::font();
        setValue("size", font.pointSize());
        setValue("family", font.family());
        setValue("weight", font.weight());
        endGroup();
        endGroup();
    }

    void loadAllFontSettings() {
        QFont font = QApplication::font();
        beginGroup("appearance");
        beginGroup("font");
        font.setPointSize(value("size", font.pointSize()).toInt());
        font.setFamily(value("family", font.family()).toString());
        font.setWeight(value("weight", font.weight()).toInt());
        endGroup();
        endGroup();
        QApplication::setFont(font);
    }

    void addApplication(const QString& appTag, const QString& appPath) {
        beginGroup("application");
        setValue(appTag, appPath);
        endGroup();
    }

    QString getApplication(const QString& appTag) {
        beginGroup("application");
        QString val = value(appTag).toString();
        endGroup();
        return val;
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

};

#endif /* USER_PREFERENCES_H */

