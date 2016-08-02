/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   user_projects.h
 * Author: biyanin
 *
 * Created on August 2, 2016, 8:40 PM
 */

#ifndef USER_PROJECTS_H
#define USER_PROJECTS_H

#include <QSettings>
#include <QApplication>
#include <QFont>
#include <QString>
#include <QStringList>
#include <QDir>

#include "confData.h"

class UserProjects : public QSettings {
public:

    UserProjects(confData* data)
    : QSettings(data->getDir("home_2dx") + "/projects.ini", QSettings::Format::IniFormat) {
    }

    void addProjectPath(const QString& path) {
        QStringList paths = projectPaths();
        paths.push_front(QDir(path).absolutePath());
        paths.removeDuplicates();
        clear();
        beginWriteArray("recents");
        for (int i = 0; i < paths.size(); ++i) {
            setArrayIndex(i);
            setValue("path", paths.at(i));
        }
        endArray();
    }

    QStringList projectPaths() {
        QStringList paths;
        int size = beginReadArray("recents");
        for (int i = 0; i < size; ++i) {
            setArrayIndex(i);
            if (value("path").toString() != "") paths << value("path").toString();
        }
        endArray();
        return paths;
    }


};

#endif


