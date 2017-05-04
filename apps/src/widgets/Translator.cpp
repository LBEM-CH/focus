/**************************************************************************
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
#include "ImageNavigator.h"
#include "ApplicationData.h"
#include "ProjectData.h"
#include "UserPreferenceData.h"

#include "Translator.h"

Translator::Translator(const QString& workDir, QObject *parent)
: QObject(parent) {
    workingDir = workDir;
    proc = new QProcess(this);
    proc->setWorkingDirectory(projectData.projectWorkingDir().canonicalPath() + "/" + "SCRATCH");
    
    getAvailableTranslators();
    
    connect(proc, &QProcess::readyReadStandardOutput, [=]{
        while (!proc->atEnd()) {
            qDebug() << "Translator Output: " << proc->readLine();
        }
    });
    connect(proc, &QProcess::readyReadStandardError, [=]{
        qDebug() << "Translator ERROR: " << proc->readAllStandardError();
        
    });
    
}

bool Translator::getAvailableTranslators() {
    
    if (!ApplicationData::translatorsDir().exists()) return false;
    
    QString entry, ext;

    foreach(entry, ApplicationData::translatorsDir().entryList(QStringList() << "*.tr", QDir::Files)) {
        ext = entry;
        translators.insert(ext.remove(QRegExp("\\.tr$")).trimmed().toLower(), ApplicationData::translatorsDir().canonicalPath() + "/" + entry);
    }
    return true;
}

QStringList Translator::getEnvironment() {
    QStringList env = QProcess::systemEnvironment();

    foreach(ParameterElementData *e, userPreferenceData.data()->getLookupTable()) {
        env << e->name() + "_focus_app=" + e->value().toString();
    }

    
    return env;
}


void Translator::open(const QString &fileName, const QString& extention) {
    if(fileName.isEmpty() || !QFileInfo(fileName).exists()) return;
    QString ext = extention.trimmed();
    
    if(ext.isEmpty()) ext = QFileInfo(fileName).suffix().toLower();

    proc->setEnvironment(getEnvironment());
    
    if (translators.contains(ext))
        proc->start(translators[ext], QStringList() << fileName);
    else if (ext == "ps")
        proc->start(userPreferenceData.get("psViewer").trimmed() + " " + fileName);
    else if (ext == "pdf")
        proc->start(userPreferenceData.get("pdfViewer").trimmed() + " " + fileName);
    else if (ext == "mrc") {
        new ImageNavigator(workingDir, new mrcImage(fileName), NULL);
    } else {
        proc->start(userPreferenceData.get("scriptEditor").trimmed() + " " + fileName);
    }
}
