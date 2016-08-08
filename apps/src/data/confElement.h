/*
 *  confElement.h
 *  2DX-Mod
 *
 *  Created by Bryant Gipson on 2/22/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef _CONFELEMENT_H_
#define _CONFELEMENT_H_

#include <iostream>
#include <QHash>
#include <QStringList>
#include <QPoint>
#include <QVariant>
#include <QPair>

class confElement : public QObject {

    Q_OBJECT

public:
    enum class Type {
        NONE,
        TEXT_EDIT,
        DIRECTORY,
        FILE,
        DROP_DOWN,
        FLOAT,
        BOOL,
        INT
    };

    class TypeInfo {
    public:

        Type type;
        int count;
        QStringList properties;

        TypeInfo() {
        };

        TypeInfo(QString typeStr) {
            type = getType(typeStr);
            count = getCount(typeStr);
            properties = getProperties(typeStr);
        }

        static confElement::Type getType(const QString& typeStr) {
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
                std::cerr << "CRITICAL: Parameter type not understood: \n" << typeStr.toStdString() << "\n";
                return Type::NONE;
            }
        }

        static int getCount(const QString& typeStr) {
            QString typeStr_ = typeStr.trimmed().toLower();
            if (typeStr_.startsWith("two")) return 2;
            else if (typeStr_.startsWith("thr")) return 3;
            else if (typeStr_.startsWith("fou")) return 4;
            else return 1;
        }

        static QStringList getProperties(const QString& typeStr) {
            QStringList options = typeStr.trimmed().split('"');
            if (options.count() < 2) return QStringList();
            else {
                return options[1].split(';');
            }
        }

        static QMap<int, QStringList> deduceMinMaxPairs(QStringList fields) {
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

    };

public:
    confElement(QObject *parent = NULL);

    bool set(QString label, QString value);
    QString get(QString label);

    bool concerns(QString concern);

    const QHash<QString, QString> &propertyList();

    QPoint toQPoint(int group = 0, bool *ok = NULL);
    QPointF toQPointF(int group = 0, bool *ok = NULL);
    bool toBool(bool *ok = NULL);
    float toFloat(bool *ok = NULL);
    QVariant data();
    QPair<float, float> range();

    confElement &operator=(const confElement &e);
    QString toString();

    TypeInfo typeInfo() {
        return TypeInfo(get("type"));
    }

    bool locked() {
        bool yup = false;
        if (get("LOCKED").trimmed().toLower() == "yes") yup = true;
        return yup;
    };

    void setLock(bool lock) {
        if (lock) set("LOCKED", "YES");
        else set("LOCKED", "NO");
    };

    bool isWrong() {
        bool yup = false;
        if (get("ISWRONG").trimmed().toLower() == "yes") yup = true;
        return yup;
    }

    void setIsWrong(bool iswrong) {
        if (iswrong) set("ISWRONG", "YES");
        else set("ISWRONG", "NO");
    }
    
    int userLevel() {
        QString userLevelString = get("USERLEVEL").trimmed().toLower();
        if(userLevelString == "hidden") return 2;
        else if(userLevelString == "advanced") return 1;
        else return 0;
    }

signals:
    void dataChanged();
    void syncPropertiesWithUpper(QString);

private:
    QHash<QString, QString> properties;


};

Q_DECLARE_METATYPE(confElement*);

#endif
