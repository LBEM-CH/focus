#include <QDebug>

#include "ParameterMaster.h"
#include "ParameterSectionData.h"
#include "ParameterElementData.h"
#include "ParameterConfiguration.h"


ParameterElementData::ParameterElementData(const QString& name, ParameterSectionData* parent)
: QObject(parent), name_(name) {
};

ParameterElementData& ParameterElementData::operator=(const ParameterElementData &e) {
    name_ = e.name_;
    value_ = e.value_;
    lock_ = e.lock_;
    isWrong_ = e.isWrong_;
    return *this;
}

ParameterSectionData* ParameterElementData::getSection() {
    return static_cast<ParameterSectionData*>(parent());
}


QPoint ParameterElementData::toQPoint(int group, bool *ok) const {

    QStringList cell = value().toString().split(',');
    int index = 2 * group;
    if (cell.size() >= index + 2) {
        bool oneOk, twoOk;
        QPoint p(cell[index].toInt(&oneOk), cell[index + 1].toInt(&twoOk));
        if (ok != NULL)
            *ok = oneOk && twoOk;
        return p;
    } else {
        if (ok != NULL)
            *ok = false;
        return QPoint();
    }
}

QPointF ParameterElementData::toQPointF(int group, bool *ok) const {
    QStringList cell = value().toString().split(',');
    int index = 2 * (group);
    if (cell.size() >= index + 2) {
        bool oneOk, twoOk;
        QPointF p(cell[index].toFloat(&oneOk), cell[index + 1].toFloat(&twoOk));
        if (ok != NULL)
            *ok = oneOk && twoOk;
        return p;
    } else {
        if (ok != NULL)
            *ok = false;
        return QPointF();
    }
}

bool ParameterElementData::toBool(bool *ok) const {
    QString val = value().toString().trimmed().toLower();
    if (val == "y" || val == "yes") return true;
    else if (val == "n" || val == "no") return false;
    else if (ok != NULL) *ok = false;
    return false;
}

float ParameterElementData::toFloat(bool *ok) const {
    return value().toFloat(ok);
}

QString ParameterElementData::name() const {
    return name_;
}

QVariant ParameterElementData::value() const {
    return value_;
}

bool ParameterElementData::locked() const {
    return lock_;
}

bool ParameterElementData::isWrong() const {
    return isWrong_;
}

ParameterTypeInfo ParameterElementData::typeInfo() const{
    return parameterMaster.typeInfo(name());
}

QString ParameterElementData::legend() const {
    return parameterMaster.legend(name());
}

QString ParameterElementData::label() const {
    return parameterMaster.label(name());
}

QString ParameterElementData::example() const {
    return parameterMaster.example(name());
}

QString ParameterElementData::helpUrl() const {
    return parameterMaster.helpUrl(name());
}

bool ParameterElementData::lockable() const {
    return parameterMaster.lockable(name());
}


bool ParameterElementData::syncWithUpperLevel() const {
    return parameterMaster.syncWithUpperLevel(name());
}

int ParameterElementData::userLevel() const {
    return parameterMaster.userLevel(name());
}

QString ParameterElementData::relation() const {
    return parameterMaster.relation(name());
}

void ParameterElementData::setValue(QString value) {
    if(typeInfo().count > 1) {
        if (value.split(',').size() != typeInfo().count) {
            qDebug() << "Trying to set:" << name() << "didn't get expected" << typeInfo().count << "values";
            qDebug() << "Check: " << name() << " = " << value;
        }
    }
    if (value_.toString() != value) {
        value_ = QVariant(value);
        emit dataChanged();
        emit valueChanged(value);
    }
}

void ParameterElementData::setLock(bool lock) {
    if(lockable()) {
        if(lock_ != lock) {
            lock_ = lock;
            emit dataChanged();
            emit lockChanged(lock);
        }
    }
}

void ParameterElementData::setIsWrong(bool iswrong) {
    bool changed = false;
    if(isWrong_ != iswrong) changed = true;
    isWrong_ = iswrong;
    if (changed) {
        emit dataChanged();
    }
}

