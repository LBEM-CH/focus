/***************************************************************************
 *   Copyright (C) 2006 by UC Davis Stahlberg Laboratory                   *
 *   HStahlberg@ucdavis.edu                                                *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <QApplication>
#include <QtPlugin>
#include <QDesktopWidget>
#include <QDebug>
#include <iostream>
#include <QCommandLineParser>
#include <QStringList>

#include "mainWindow.h"

using namespace std;

#ifdef _USE_STATIC_PLUGINS_

Q_IMPORT_PLUGIN(qjpeg)
Q_IMPORT_PLUGIN(qgif)
#endif

QString getAppDir() {
    QString appDir = QApplication::applicationDirPath();
    QString sep = "/../";
#ifdef Q_OS_MAC
    appDir += "/../../../";
#endif
    int tries = 0;
    while (!QFileInfo(appDir + sep + "config/2dx_master.cfg").exists() && tries < 3) {
        qDebug() << (appDir + sep + "config/2dx_master.cfg") << " does not exist!";
        sep += "../";
        tries++;
    }
    if (QFileInfo(appDir + sep + "config/2dx_master.cfg").exists()) {
        return QString(appDir + sep);
    } else
        return QString();

}

void initializeProject(const QString &appDir, const QString &workingDir) {

    QDir dir(workingDir);
    dir.mkdir("merge");
    dir.mkpath("merge/proc");
    dir.mkpath("merge/LOGS");
    confData data(workingDir + "/merge/" + "2dx_merge.cfg", appDir + "config/2dx_master.cfg");
    data.setSymLink("merge/2dx_merge.cfg", workingDir + "/2dx_master.cfg");
    data.save();
}

int main(int argc, char *argv[]) {

    QApplication app(argc, argv);
    QCoreApplication::setApplicationName("2DX");
    QCoreApplication::setOrganizationName("C-CINA");
    QCoreApplication::setOrganizationDomain("c-cina.org");

    QString appDir = getAppDir();
    if (appDir.isEmpty()) exit(0);

    QString configDir = appDir + "/config";

    // read user config to get the latest image dir
    confData* userData = new confData(QDir::homePath() + "/.2dx/" + "2dx_merge-user.cfg", configDir + "/" + "2dx_merge-user.cfg");
    userData->save();
    QString userDirPath = userData->get("workingDir", "value");
    //qDebug() << "The last used working dir is: " << userDirPath;
    if (userDirPath.isEmpty())
        userDirPath = QDir::homePath();


    QString workingDir = QFileDialog::getExistingDirectory(0, "Select a Project Directory", userDirPath);
    if (workingDir.isEmpty() || !QDir(workingDir).exists()) exit(0);
    //save the selected dir as working dir in 2dx_merge-user.cfg
    userData->set("workingDir", workingDir);
    userData->save();

    if (!QFileInfo(workingDir + "/merge/" + "2dx_merge.cfg").exists()) {
        quint32 choice = QMessageBox::question(NULL, "Confirm Create new Project?", "No configuration data found in:\n" + workingDir + "\n\nCreate new config files in this directory?", "Create", "Cancel", QString(), 0, 1);
        if (choice) return 0;
        else {
            initializeProject(appDir, workingDir);
        }
    }

    mainWindow *win = new mainWindow(workingDir);
    win->show();
    win->raise(); // raises the window on top of the parent widget stack
    win->activateWindow(); // activates the window an thereby putting it on top-level

    return app.exec();
}
