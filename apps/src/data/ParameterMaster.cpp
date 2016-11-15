#include "ParameterMaster.h"
#include "ProjectData.h"

ParameterMaster::PropertySet::PropertySet(const PropertySet& other) :
label(other.label),
legend(other.legend),
typeInfo(other.typeInfo),
userLevel(other.userLevel),
example(other.example),
helpUrl(other.helpUrl),
relation(other.relation),
lockable(other.lockable),
syncWithUpperLevel(other.syncWithUpperLevel) {
}

ParameterMaster::PropertySet& ParameterMaster::PropertySet::operator=(const PropertySet& right) {
    // Check for self-assignment!
    if (this == &right) // Same object?
        return *this; // Yes, so skip assignment, and just return *this.

    label = right.label;
    legend = right.legend;
    typeInfo = right.typeInfo;
    userLevel = right.userLevel;
    example = right.example;
    helpUrl = right.helpUrl;
    relation = right.relation;
    lockable = right.lockable;
    syncWithUpperLevel = right.syncWithUpperLevel;

    return *this;
}

void ParameterMaster::registerParameter(const QString& paramName, const QMap<QString, QString>& properties) {
    PropertySet set;
    for (int i = 0; i < properties.keys().size(); ++i) {
        QString prop = properties.keys()[i].toLower().trimmed();
        QString valueStr = properties[properties.keys()[i]].simplified();
        if (prop == "label") set.label = valueStr;
        else if (prop == "legend") set.legend = valueStr;
        else if (prop == "example") set.example = valueStr;
        else if (prop == "help") set.helpUrl = valueStr;
        else if (prop == "relation") set.relation = valueStr;
        else if (prop == "type") set.typeInfo = ParameterTypeInfo(valueStr);
        else if (prop == "locked") set.lockable = true;
        else if (prop == "userlevel") {
            QString userLevelString = valueStr.toLower().trimmed();
            if (userLevelString == "hidden") set.userLevel = 2;
            else if (userLevelString == "advanced") set.userLevel = 1;
            else set.userLevel = 0;
        } else if (prop == "sync_with_upper_level") {
            QString valueStrLower = valueStr.toLower().trimmed();
            if (valueStrLower == "yes" || valueStrLower == "y") set.syncWithUpperLevel = true;
            else set.syncWithUpperLevel = false;
        } else {
            qDebug() << "WARNING: Did not understand property: " << prop << " set for parameter " << paramName;
        }
    }
    properties_.insert(paramName, set);
}

void ParameterMaster::registerParameterMaster(const QString& fileName) {
    if (!QFileInfo(fileName).exists()) {
        qDebug() << "Configuration File Missing!" << fileName << " does not exist. Will quit now.";
        exit(0);
    }

    QFile data(fileName);
    if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "Configuration file read error!" << fileName << " was not read. Will quit now.";
        exit(0);
    }

    QStringList valueSearch;
    valueSearch << "LABEL" << "LEGEND" << "EXAMPLE" << "HELP" << "TYPE" << "RELATION" << "USERLEVEL" << "LOCKED" << "SYNC_WITH_UPPER_LEVEL" << "MODE";

    QMap<QString, QString> propertiesRead;
    QString lineData;
    qint64 pos = -1;
    while (!data.atEnd() && pos != data.pos()) {
        pos = data.pos();
        lineData = data.readLine().trimmed();
        lineData.remove('#');
        lineData = lineData.trimmed();

        for (int i = 0; i < valueSearch.size(); i++) {
            if (lineData.startsWith(valueSearch[i] + ':')) {
                lineData.remove(0, valueSearch[i].size() + 1);
                propertiesRead.insert(valueSearch[i].simplified().toLower(), lineData);
            }
        }

        if (lineData.toLower().startsWith("set ")) {

            //Check if the mode is correct
            QString mode = "ALL";
            if (propertiesRead.keys().contains("mode"))  {
                mode = propertiesRead["mode"];
                propertiesRead.remove("mode");
            }
            
            if (mode == "ALL" || mode.contains(QString::number(projectData.projectMode().toInt()))) {

                int k = lineData.indexOf('=');
                if (k > 0) {
                    QStringList val = lineData.split('=');

                    val[0].remove(0, 4);
                    val[1].remove('"');

                    val[0] = val[0].simplified();
                    val[1] = val[1].simplified();

                    parameterMaster.registerParameter(val[0], propertiesRead);
                }
            }

            propertiesRead.clear();
        }
    }

    data.close();
}

bool ParameterMaster::containsParameter(const QString& paramName) {
    return properties_.keys().contains(paramName);
}


void ParameterMaster::printRegisteredParameters() {
    for (int i = 0; i < properties_.keys().size(); ++i) {
        QString name = properties_.keys()[i];
        qDebug() << name << label(name) << typeInfo(name).properties;
    }
}


ParameterMaster& ParameterMaster::Instance() {
    static ParameterMaster instance_;
    return instance_;
}

ParameterMaster::ParameterMaster() {
}

ParameterMaster::PropertySet ParameterMaster::propertySet(const QString& parameter) {
    if (properties_.keys().contains(parameter)) return properties_[parameter];
    else {
        qDebug() << "No properties found for: " << parameter;
        return PropertySet();
    }
}

QString ParameterMaster::example(const QString& parameterName) {
    return propertySet(parameterName).example;
}

QString ParameterMaster::helpUrl(const QString& parameterName) {
    return propertySet(parameterName).helpUrl;
}

QString ParameterMaster::label(const QString& parameterName) {
    return propertySet(parameterName).label;
}

QString ParameterMaster::legend(const QString& parameterName) {
    return propertySet(parameterName).legend;
}

bool ParameterMaster::lockable(const QString& parameterName) {
    return propertySet(parameterName).lockable;
}

QString ParameterMaster::relation(const QString& parameterName) {
    return propertySet(parameterName).relation;
}

bool ParameterMaster::syncWithUpperLevel(const QString& parameterName) {
    return propertySet(parameterName).syncWithUpperLevel;
}

ParameterTypeInfo ParameterMaster::typeInfo(const QString& parameterName) {
    return propertySet(parameterName).typeInfo;
}

int ParameterMaster::userLevel(const QString& parameterName) {
    return propertySet(parameterName).userLevel;
}