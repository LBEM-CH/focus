#ifndef PARAMETERTYPEINFO_H
#define PARAMETERTYPEINFO_H

#include <QString>
#include <QStringList>

class ParameterTypeInfo{
    
public:
    
    enum class Type {
        NONE, TEXT_EDIT, DIRECTORY, FILE, DROP_DOWN, FLOAT, BOOL, INT
    };
    
    Type type;
    int count;
    QStringList properties;

    ParameterTypeInfo();
    ParameterTypeInfo(QString typeStr);
    
    ParameterTypeInfo(const ParameterTypeInfo& other) :
    type(other.type), count(other.count), properties(other.properties) {
    }
    
    ParameterTypeInfo& operator=(const ParameterTypeInfo& right) {
        // Check for self-assignment!
        if (this == &right) // Same object?
            return *this; // Yes, so skip assignment, and just return *this.
        
        type = right.type;
        count = right.count;
        properties = right.properties;
        
        return *this;
    }
    
    static ParameterTypeInfo::Type getType(const QString& typeStr);
    static int getCount(const QString& typeStr);
    static QStringList getProperties(const QString& typeStr);
    static QMap<int, QStringList> deduceMinMaxPairs(QStringList fields);
};

#endif /* PARAMETERTYPEINFO_H */

