#include <QDebug>

#include "ParameterTypeInfo.h"

ParameterTypeInfo::ParameterTypeInfo() {
}

ParameterTypeInfo::ParameterTypeInfo(QString typeStr) {
    type = getType(typeStr);
    count = getCount(typeStr);
    properties = getProperties(typeStr);
}

int ParameterTypeInfo::getCount(const QString& typeStr) {
    QString typeStr_ = typeStr.trimmed().toLower();
    if (typeStr_.startsWith("two")) return 2;
    else if (typeStr_.startsWith("thr")) return 3;
    else if (typeStr_.startsWith("fou")) return 4;
    else return 1;
}

QStringList ParameterTypeInfo::getProperties(const QString& typeStr) {
    QStringList options = typeStr.trimmed().split('"');
    if (options.count() < 2) return QStringList();
    else {
        return options[1].split(';');
    }
}

ParameterTypeInfo::Type ParameterTypeInfo::getType(const QString& typeStr) {
    QString typeStr_ = typeStr.trimmed().toLower();
    if (typeStr_ == "") return Type::NONE;
    else if (typeStr_.startsWith("dir")) return Type::DIRECTORY;
    else if (typeStr_.startsWith("fil")) return Type::FILE;
    else if (typeStr_.startsWith("dro")) return Type::DROP_DOWN;
    else if (typeStr_.startsWith("flo")) return Type::FLOAT;
    else if (typeStr_.startsWith("two")) return Type::FLOAT;
    else if (typeStr_.startsWith("thr")) return Type::FLOAT;
    else if (typeStr_.startsWith("fou")) return Type::FLOAT;
    else if (typeStr_.startsWith("boo")) return Type::BOOL;
    else if (typeStr_.startsWith("int")) return Type::INT;
    else if (typeStr_.startsWith("tex")) return Type::TEXT_EDIT;
    else {
        qDebug() << "CRITICAL: Parameter type not understood: \n" << typeStr;
        return Type::NONE;
    }
}

QMap<int, QStringList> ParameterTypeInfo::deduceMinMaxPairs(QStringList fields) {
    QMap<int, QStringList> minMaxPairs;
    bool minDefined = false;
    bool maxDefined = false;
    bool currentMaxDefined, currentMinDefined;
    int k = 0;

    foreach(QString field, fields) {
        if (minDefined && field.contains("min", Qt::CaseInsensitive)) {
            k++;
            minDefined = false;
            maxDefined = false;
        }
        if (maxDefined && field.contains("max", Qt::CaseInsensitive)) {
            k++;
            minDefined = false;
            maxDefined = false;
        }
        currentMaxDefined = field.contains("max", Qt::CaseInsensitive);
        currentMinDefined = field.contains("min", Qt::CaseInsensitive);

        if (currentMinDefined) {
            minMaxPairs[k].insert(0, field.section('=', -1, -1).trimmed());
            minDefined = true;
        }
        if (currentMaxDefined) {
            minMaxPairs[k].insert(1, field.section('=', -1, -1).trimmed());
            maxDefined = true;
        }
    }

    return minMaxPairs;
}

