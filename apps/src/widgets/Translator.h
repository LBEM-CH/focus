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

#ifndef TRANSLATOR_H
#define TRANSLATOR_H

#include <QProcess>
#include <QDebug>
#include <QMap>

#include "ParameterConfiguration.h"

class Translator : public QObject {
    
    Q_OBJECT
    
public:
    Translator(const QString& workingDir, const QString &translatorDir, QObject *parent = NULL);
    bool getAvailableTranslators(const QString &translatorDir);

public slots:
    void open(const QString &fileName, const QString& extension);

private:
    QString workingDir;
    QProcess* proc;
    QMap<QString, QString> translators;

};

#endif

