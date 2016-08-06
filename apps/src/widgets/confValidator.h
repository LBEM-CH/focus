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

#include "confElement.h"

class confValidator {
public:

    static QStringList valueErrors(confElement::TypeInfo info, const QString& value) {
        QStringList errorMsg_;
        if (info.type == confElement::Type::DIRECTORY && !QDir(value).exists()) {
            errorMsg_ << QString("The directory used does not exit: ") + value + QString("\n");
        } else if (info.type == confElement::Type::FILE && !QFileInfo(value).exists()) {
            errorMsg_ << QString("The file used does not exit: ") + value + QString("\n");
            ;
        } else if (info.type == confElement::Type::DROP_DOWN) {
            if (!(QVariant(value).canConvert<int>())) {
                errorMsg_ << QString("The drop down index can't be converted to int\n");
            } else if (value.toInt() >= info.properties.size()) {
                errorMsg_ << QString("The drop down index exceeds it's size \n");
            }
        } else if (info.type == confElement::Type::FLOAT && !(QVariant(value).canConvert<float>())) {
            errorMsg_ << QString("The provided value cannot be converted to float: ") + value + QString("\n");
        } else if (info.type == confElement::Type::INT && !(QVariant(value).canConvert<int>())) {
            errorMsg_ << QString("The provided value cannot be converted to int: ") + value + QString("\n");
        } else if (info.type == confElement::Type::BOOL) {
            if (value != "y" || value != "n" || value != "yes" || value != "no" || value != "0" || value != "1") {
                errorMsg_ << QString("The provided value cannot be converted to bool: ") + value + QString("\n");
            }
        }

        return errorMsg_;
    }
};

#endif
