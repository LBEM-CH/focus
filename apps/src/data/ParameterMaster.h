#ifndef PARAMETERPROPERTIES_H
#define PARAMETERPROPERTIES_H

#include <QMap>
#include <QString>
#include <QStringList>
#include <QFileInfo>
#include <QFile>

#include "ParameterTypeInfo.h"

#define parameterMaster (ParameterMaster::Instance())

class ParameterMaster {
public:

    static ParameterMaster& Instance();

    void registerParameter(const QString& paramName, const QMap<QString, QString>& properties);
    bool containsParameter(const QString& paramName);

    QString label(const QString& parameterName);
    QString legend(const QString& parameterName);
    ParameterTypeInfo typeInfo(const QString& parameterName);
    int userLevel(const QString& parameterName);
    QString example(const QString& parameterName);
    QString helpUrl(const QString& parameterName);
    QString relation(const QString& parameterName);
    bool lockable(const QString& parameterName);
    bool syncWithUpperLevel(const QString& parameterName);

    void printRegisteredParameters();

    static void registerParameterMaster(const QString& fileName);

private:

    class PropertySet {
    public:
        QString label;
        QString legend;
        ParameterTypeInfo typeInfo;
        int userLevel = 0;
        QString example;
        QString helpUrl;
        QString relation;
        bool lockable = false;
        bool syncWithUpperLevel = false;

        PropertySet() = default;

        PropertySet(const PropertySet& other);
        PropertySet& operator=(const PropertySet& right);
    };

    ParameterMaster();

    PropertySet propertySet(const QString& parameter);

    QMap<QString, PropertySet> properties_;
};


#endif /* PARAMETERPROPERTIES_H */

