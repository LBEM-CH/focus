#ifndef PARAMETERPROPERTIES_H
#define PARAMETERPROPERTIES_H

#include <QMap>
#include <QString>
#include <QStringList>

#include "ParameterTypeInfo.h"

#define parameterMaster (ParameterMaster::Instance())

class ParameterMaster {
    
public:
    
    static ParameterMaster& Instance() {
        static ParameterMaster instance_;
        return instance_;
    }
    
    void registerParameter(const QString& paramName, const QMap<QString, QString>& properties) {
        PropertySet set;
        for(int i=0; i< properties.keys().size(); ++i) {
            QString prop = properties.keys()[i].toLower().trimmed();
            QString valueStr = properties[properties.keys()[i]].simplified();
            if(prop == "label") set.label =  valueStr;
            else if(prop == "legend") set.legend = valueStr;
            else if(prop == "example") set.example = valueStr;
            else if(prop == "help") set.helpUrl = valueStr;
            else if(prop == "relation") set.relation = valueStr;
            else if(prop == "type") set.typeInfo = ParameterTypeInfo(valueStr);
            else if(prop == "userlevel") {
                QString userLevelString = valueStr.toLower().trimmed();
                if (userLevelString == "hidden") set.userLevel = 2;
                else if (userLevelString == "advanced") set.userLevel = 1;
                else set.userLevel = 0;
            }
            else if(prop == "sync_with_upper_level") {
                QString valueStrLower = valueStr.toLower().trimmed();
                if (valueStrLower == "yes" || valueStrLower == "y") set.syncWithUpperLevel = true;
                else set.syncWithUpperLevel = false;
            }
            else {
                qDebug() << "WARNING: Did not understand property: " << prop << " set for parameter " << paramName;
            }
        }
        properties_.insert(paramName, set);
    }
    
    QString label(const QString& parameterName) {
        return propertySet(parameterName).label;
    }
    
    QString legend(const QString& parameterName) {
        return propertySet(parameterName).legend;
    }
    
    ParameterTypeInfo typeInfo(const QString& parameterName) {
        return propertySet(parameterName).typeInfo;
    }
    
    int userLevel(const QString& parameterName) {
        return propertySet(parameterName).userLevel;
    }
    
    QString example(const QString& parameterName) {
        return propertySet(parameterName).example;
    }
    
    QString helpUrl(const QString& parameterName) {
        return propertySet(parameterName).helpUrl;
    }
    
    QString relation(const QString& parameterName) {
        return propertySet(parameterName).relation;
    }
    
    bool syncWithUpperLevel(const QString& parameterName) {
        return propertySet(parameterName).syncWithUpperLevel;
    }
    
    void printRegisteredParameters() {
        for(int i=0; i< properties_.keys().size(); ++i) {
            QString name = properties_.keys()[i];
            qDebug() << name << label(name) << typeInfo(name).properties;
        }
    }
    
private:
    
    class PropertySet {
    public:
        QString label;
        QString legend;
        ParameterTypeInfo typeInfo;
        int userLevel;
        QString example;
        QString helpUrl;
        QString relation;
        bool syncWithUpperLevel = false;
        
        PropertySet() = default;
        
        PropertySet(const PropertySet& other) :
        label(other.label), 
        legend(other.legend), 
        typeInfo(other.typeInfo), 
        userLevel(other.userLevel), 
        example(other.example), 
        helpUrl(other.helpUrl), 
        relation(other.relation), 
        syncWithUpperLevel(other.syncWithUpperLevel) {
        }

        
        PropertySet& operator=(const PropertySet& right) {
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
            syncWithUpperLevel = right.syncWithUpperLevel;
            
            return *this;
        }

    };
    
    ParameterMaster() {};
    
    PropertySet propertySet(const QString& parameter) {
        if(properties_.keys().contains(parameter)) return properties_[parameter];
        else return PropertySet();
    }
    
    QMap<QString, PropertySet> properties_;    
};


#endif /* PARAMETERPROPERTIES_H */

