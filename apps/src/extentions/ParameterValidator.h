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

#ifndef CONFVALIDATOR_H
#define CONFVALIDATOR_H

#include <QString>
#include <QStringList>
#include <QDir>
#include <QFileInfo>
#include <QVariant>

#include "ParameterElementData.h"

class ParameterValidator {
public:

    static QStringList valueErrors(ParameterTypeInfo info, const QString& val) {
        QStringList errorMsg_;
        QString value = val.trimmed();
        if (info.type == ParameterTypeInfo::Type::DIRECTORY && !QDir(value).exists()) {
            errorMsg_ << QString("The directory used does not exist: ") + value + QString("\n");
        } else if (info.type == ParameterTypeInfo::Type::FILE && !QFileInfo(value).exists()) {
            errorMsg_ << QString("The file used does not exist: ") + value + QString("\n");
        } else if (info.type == ParameterTypeInfo::Type::DROP_DOWN) {
            if(QVariant(value).canConvert<int>() && value.toInt() >= info.properties.size() && !info.properties.contains(value)) {
                errorMsg_ << QString("The drop down index exceeds it's size \n");
            }
            else if(!QVariant(value).canConvert<int>() && !info.properties.contains(value)) {
                errorMsg_ << QString("The value ") + value + (" specified is not contained in drop down\n");
            }
        } else if (info.type == ParameterTypeInfo::Type::FLOAT && !(QVariant(value).canConvert<float>())) {
            errorMsg_ << QString("The provided value cannot be converted to float: ") + value + QString("\n");
        } else if (info.type == ParameterTypeInfo::Type::INT && !(QVariant(value).canConvert<int>())) {
            errorMsg_ << QString("The provided value cannot be converted to int: ") + value + QString("\n");
        } else if (info.type == ParameterTypeInfo::Type::BOOL) {
            value = value.toLower();
            if (value != "y" && value != "n" && value != "yes" && value != "no" && value != "0" && value != "1") {
                errorMsg_ << QString("The provided value cannot be converted to bool: ") + value + QString("\n");
            }
        }

        return errorMsg_;
    }
};

#endif
