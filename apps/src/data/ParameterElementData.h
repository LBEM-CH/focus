#ifndef PARAMETER_ELEMENT_DATA
#define PARAMETER_ELEMENT_DATA

#include <QMap>
#include <QStringList>
#include <QPoint>
#include <QVariant>
#include <QObject>

#include "ParameterTypeInfo.h"

class ParameterSectionData;

class ParameterElementData : public QObject {

    Q_OBJECT

public:
    
    ParameterElementData(const QString& name, ParameterSectionData* parent);
    ParameterElementData &operator=(const ParameterElementData &e);
    
    ParameterSectionData* getSection();
    
    //Getters
    QString name() const;
    bool locked() const;
    bool isWrong() const;
    
    QVariant value() const;
    QPoint toQPoint(int group = 0, bool *ok = NULL) const;
    QPointF toQPointF(int group = 0, bool *ok = NULL) const;
    bool toBool(bool *ok = NULL) const;
    float toFloat(bool *ok = NULL) const;
    
    //Getters from properties
    QString legend() const;
    QString label() const;
    ParameterTypeInfo typeInfo() const;
    int userLevel() const;
    QString example() const;
    QString helpUrl() const;
    bool lockable() const;
    bool syncWithUpperLevel() const;
    QString relation() const;
    
    //Setters
    void setValue(QString value);
    void setLock(bool lock);
    void setIsWrong(bool iswrong);  
    
signals:
    void dataChanged();
    void valueChanged(const QVariant value);
    void lockChanged(bool lock);

private:
    QString name_;
    QVariant value_;
    bool lock_ = false;
    bool isWrong_ = false;
};

Q_DECLARE_METATYPE(ParameterElementData*);

#endif
