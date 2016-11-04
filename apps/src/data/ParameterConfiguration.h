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

#ifndef PARAMETERS_CONFIGURAION_H
#define PARAMETERS_CONFIGURAION_H

#include <QApplication>
#include <QList>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QString>
#include <QIcon>
#include <QMap>
#include <QReadWriteLock>

#include "ParameterElementData.h"
#include "ParameterSectionData.h"

class ParametersConfiguration : public QObject {
    
    Q_OBJECT

public:
    
    /**
     * Construction using a reference master config file.
     * @param propertiesFile
     * @param valuesFile
     * @param parent
     */
    ParametersConfiguration(const QString& propertiesFile, const QString& valuesFile, QObject* parent = 0);
    
    /**
     * Construction using a parent config already set.
     * @param parentData
     * @param valuesFile
     */
    ParametersConfiguration(ParametersConfiguration* parentData, const QString& valuesFile, bool resetValues = true);
    
    ParameterSectionData* operator[](unsigned int i);
    const ParameterSectionData* operator[](unsigned int i) const;
    
    bool isEmpty();
    bool isModified();
    bool hasParent();
    
    QString dataFileName();

    int set(QString element, QString value, bool saveOnDisk=true);
    int setForce(QString element, QString value, bool saveOnDisk=true);
    
    unsigned int size();
    QMap<QString, ParameterElementData *> getLookupTable();

public slots:
    void save();
    void saveAs(QString fileName, bool saveSyncronized);
    void reload();
    
    void setModified(bool isModified);

    ParameterElementData* get(QString element);
    QString getValue(QString element);

signals:
    void dataModified(bool);
    void loading();
    void saving();

private:
    static QReadWriteLock lock_;
    
    ParametersConfiguration* parentData_;  
    QList<ParameterSectionData *> sections_;
    QMap<QString, ParameterElementData *> lookup_;
    QString saveFileName_;

    bool empty_;
    bool modified_;
    
    bool parseDataFile(const QString& fileName);
    bool resetUserValues(const QString& fileName);
    
    void printElements();

};

#endif
