#ifndef USER_PROJECTS_H
#define USER_PROJECTS_H

#include <QSettings>
#include <QApplication>
#include <QFont>
#include <QString>
#include <QStringList>
#include <QDir>
#include <QFileInfo>

#include "ApplicationData.h"

class UserProjects : public QSettings {
public:
    
    UserProjects()
    : QSettings(ApplicationData::homeDir().absolutePath() + "/projects.ini", QSettings::Format::IniFormat) {
    }

    void addProjectPath(const QString& path) {
        QStringList paths = projectPaths();
        paths.push_front(QDir(path).canonicalPath());
        paths.removeDuplicates();
        clear();
        beginWriteArray("recents");
        int index = 0;
        for (int i = 0; i < paths.size(); ++i) {
            if (QFileInfo(paths.at(i) + "/merge/" + "2dx_merge.cfg").exists()) {
                setArrayIndex(index);
                setValue("path", paths.at(i));
                index++;
            }
        }
        endArray();
    }

    QStringList projectPaths() {
        QStringList paths;
        int size = beginReadArray("recents");
        for (int i = 0; i < size; ++i) {
            setArrayIndex(i);
            QString path = value("path").toString();
            if (QFileInfo(path + "/merge/" + "2dx_merge.cfg").exists() && !(path.isEmpty())) {
                paths << path;
            }
        }
        endArray();
        return paths;
    }


};

#endif


