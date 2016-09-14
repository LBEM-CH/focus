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

#ifndef SCRIPT_DATA_H
#define SCRIPT_DATA_H

#include <QApplication>
#include <QList>
#include <QMultiMap>
#include <QSet>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QString>
#include <QIcon>
#include <QMap>

class ScriptData : public QObject {
    
    Q_OBJECT

public:
    ScriptData(const QString& filename, QObject* parent = 0);

    QString property(const QString &propertyName);
    QList<QString> propertyList(const QString &propertyName);
    QMap<QString, QString> resetVariables();
    QStringList &manual();
    const QSet<QString> &subScripts();
    const QString & initializationScript(bool fullName = true);
    bool parseDataFile(const QString& fileName);
    QString& parseVariables(QString& line);

private:
    QSet<QString> subScript;
    QMultiMap<QString, QString> properties;
    QMap<QString, QString> resetVars;
    QStringList manualData;
    QString initializationScriptBaseName;
    QString initializationScriptName;

};

#endif
