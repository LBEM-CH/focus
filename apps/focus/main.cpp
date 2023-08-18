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

#include "StartupWizard.h"
#include "ProjectData.h"
#include "UserPreferences.h"

using namespace std;

#ifdef _USE_STATIC_PLUGINS_
Q_IMPORT_PLUGIN(qjpeg)
Q_IMPORT_PLUGIN(qgif)
#endif

void loadMainWindow(const QString& projectPath) {
    projectData.initiailze(QDir(projectPath));
    MainWindow *win = new MainWindow(projectPath);
    
    UserProjects().addProjectPath(projectData.projectDir().canonicalPath());
    
    win->show();
    win->raise(); // raises the window on top of the parent widget stack
    win->activateWindow(); // activates the window an thereby putting it on top-level
}

bool openProject() {
    StartupWizard* wiz = new StartupWizard();
    wiz->setAttribute(Qt::WA_DeleteOnClose);
    bool wizExec = wiz->exec();
    return wizExec;
}

int main(int argc, char *argv[]) {

    QApplication app(argc, argv);
    qApp->setAttribute(Qt::AA_UseHighDpiPixmaps);
    QCoreApplication::setApplicationName("Focus");
    QCoreApplication::setOrganizationName("LBEM");
    QCoreApplication::setOrganizationDomain("lbem.ch");

    QCommandLineParser parser;
    parser.setApplicationDescription("Focus Software Suite: Main Graphical User Interface & Project Manager");
    parser.addHelpOption();
    parser.addVersionOption();
    parser.addPositionalArgument("project_dir", "Path of the focus Project to be opened.");
    parser.process(app);
    
    UserPreferences().loadAllFontSettings();

    if (!parser.positionalArguments().isEmpty()) {
        loadMainWindow(parser.positionalArguments().first());
    } else {
        if(!openProject()) {
            QApplication::quit();
            return 0;
        }
    }

    return app.exec();
}
