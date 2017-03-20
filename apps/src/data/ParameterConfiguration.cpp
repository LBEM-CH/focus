#include <QDebug>

#include "ProjectData.h"
#include "ApplicationData.h"

#include "ParameterElementData.h"
#include "ParameterSectionData.h"
#include "ParameterConfiguration.h"
#include "ParameterMaster.h"

QReadWriteLock ParametersConfiguration::lock_;

ParametersConfiguration::ParametersConfiguration(const QString& referenceFileName, const QString& valuesFileName, QObject* parent)
: QObject(parent), parentData_(0), saveFileName_(valuesFileName) {
    empty_ = false;
    if (!parseDataFile(referenceFileName)) {
        empty_ = true;
        qDebug() << "Datafile read error: " << referenceFileName;
    }
    setModified(false);

    resetUserValues(valuesFileName);
}

ParametersConfiguration::ParametersConfiguration(ParametersConfiguration* parentData, const QString& valuesFile, bool resetValues) :
QObject(parentData), parentData_(parentData), saveFileName_(valuesFile){
    empty_ = false;
    if(!parentData) {
        empty_ = true;
        qDebug() << "CRITICAL: Cannot construct from empty cfg data.";
        return;
    }
    
    sections_.clear();
    for (int i = 0; i < parentData_->sections_.size(); i++) {
        ParameterSectionData* parentSection = parentData_->sections_[i];
        ParameterSectionData* localSection = new ParameterSectionData(parentSection->title(), this);
        sections_ << localSection;
        for (unsigned int j = 0; j < parentSection->size(); j++) {
            ParameterElementData* parentElement = parentData_->get((*parentSection)[j]->name());
            ParameterElementData* localElement;
            if (parentElement->syncWithUpperLevel() && parentData_->lookup_.contains(parentElement->name().toLower())) {
                localElement = parentElement;
            } else {
                localElement = new ParameterElementData(parentElement->name(), localSection);
                localElement->setValue(parentElement->value().toString());
                localElement->setLock(parentElement->locked());
                localElement->setIsWrong(parentElement->isWrong());
            }

            localSection->append(localElement);
            lookup_.insert(localElement->name().toLower(), localElement);
        }
    }
    
    setModified(false);
    if(resetValues) resetUserValues(valuesFile);
}


bool ParametersConfiguration::parseDataFile(const QString& fileName) {
    QFile data(fileName);
    if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) return false;

    sections_.clear();

    QString lineData;
    ParameterSectionData *section = 0;
    qint64 pos = -1;
    bool lock = false;
    bool isWrong = false;
    bool skipLine = false;
    while (!data.atEnd() && pos != data.pos()) {
        pos = data.pos();
        lineData = data.readLine().trimmed();
        lineData.remove('#');
        lineData = lineData.trimmed();
        
        
#ifdef Q_OS_MAC
        if(lineData.startsWith("BEGIN_LINUX")) skipLine = true;
        else if (lineData.startsWith("END_LINUX")) skipLine = false;
#else
        if(lineData.startsWith("BEGIN_MACOS")) skipLine = true;
        else if (lineData.startsWith("END_MACOS")) skipLine = false;
#endif
        
        if(skipLine) continue;

        if (lineData.toLower().startsWith("section:")) {
            lineData.remove(0, 8);
            section = new ParameterSectionData(lineData.simplified(), this);
            sections_ << section;
        }

        if (lineData.startsWith("LOCKED:")) {
            lineData.remove(0, QString("LOCKED:").size() + 1);
            if (lineData.trimmed().toLower() == "yes" || lineData.trimmed().toLower() == "y") lock = true;
        }
        
        if (lineData.startsWith("ISWRONG:")) {
            lineData.remove(0, QString("ISWRONG:").size() + 1);
            if (lineData.trimmed().toLower() == "yes" || lineData.trimmed().toLower() == "y") isWrong = true;
        }

        if (lineData.toLower().startsWith("set ")) {
            int k = lineData.indexOf('=');
            if (k > 0) {
                QStringList val = lineData.split('=');

                val[0].remove(0, 4);
                val[1].remove('"');

                val[0] = val[0].simplified();
                val[1] = val[1].simplified();
  
                if(parameterMaster.containsParameter(val[0])) {
                    ParameterElementData* element = new ParameterElementData(val[0], section);
                    if (element->syncWithUpperLevel() && parentData_ && parentData_->lookup_.contains(val[0].toLower())) {
                        delete element;
                        element = parentData_->get(val[0]);
                    } else {
                        element->setValue(val[1]);
                        element->setLock(lock);
                        element->setIsWrong(isWrong);
                    }

                    section->append(element);
                    lookup_.insert(element->name().toLower(), element);
                }
                
                //Reset properties
                lock = false;
                isWrong = false;
            }
        }
    }

    data.close();
    return true;
}

bool ParametersConfiguration::resetUserValues(const QString& fileName) {
    QFile data(fileName);
    if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) {
        qDebug() << "CRITICAL: File: " << fileName << " does not exist!";
        return false;
    }

    QString lineData;
    qint64 pos = -1;
    bool lock = false;
    bool isWrong = false;
    bool skipLine = false;
    while (!data.atEnd() && pos != data.pos()) {
        pos = data.pos();
        lineData = data.readLine().trimmed();
        lineData.remove('#');
        lineData = lineData.trimmed();
        
        
#ifdef Q_OS_MAC
        if(lineData.startsWith("BEGIN_LINUX")) skipLine = true;
        else if (lineData.startsWith("END_LINUX")) skipLine = false;
#else
        if(lineData.startsWith("BEGIN_MACOS")) skipLine = true;
        else if (lineData.startsWith("END_MACOS")) skipLine = false;
#endif
        
        if(skipLine) continue;

        if (lineData.startsWith("LOCKED:")) {
            lineData.remove(0, QString("LOCKED:").size() + 1);
            if (lineData.trimmed().toLower() == "yes" || lineData.trimmed().toLower() == "y") lock = true;
        }
        
        if (lineData.startsWith("ISWRONG:")) {
            lineData.remove(0, QString("ISWRONG:").size() + 1);
            if (lineData.trimmed().toLower() == "yes" || lineData.trimmed().toLower() == "y") isWrong = true;
        }

        if (lineData.toLower().startsWith("set ")) {
            int k = lineData.indexOf('=');
            if (k > 0) {
                QStringList val = lineData.split('=');

                val[0].remove(0, 4);
                val[1].remove('"');

                val[0] = val[0].simplified();
                val[1] = val[1].simplified();

                if (lookup_.contains(val[0].toLower())) {
                    ParameterElementData* element = get(val[0]);
                    if (!element->syncWithUpperLevel() || !parentData_) {
                        element->setValue(val[1]);
                        element->setLock(lock);
                        element->setIsWrong(isWrong);
                    }
                }
                lock = false;
                isWrong = false;
            }
        }
    }

    data.close();

    return true;
}

bool ParametersConfiguration::isEmpty() {
    return empty_;
}

bool ParametersConfiguration::hasParent() {
    if (parentData_ == 0) return false;
    else return true;
}

QString ParametersConfiguration::dataFileName() {
    return saveFileName_;
}


unsigned int ParametersConfiguration::size() {
    return sections_.size();
}

QMap<QString, ParameterElementData*> ParametersConfiguration::getLookupTable() {
    return lookup_;
}


ParameterSectionData* ParametersConfiguration::operator[](unsigned int i) {
    return sections_[i];
}

void ParametersConfiguration::reload() {
    resetUserValues(saveFileName_);
    emit loading();
    setModified(false);
}

void ParametersConfiguration::save() {
    saveAs(saveFileName_, true);
}

void ParametersConfiguration::saveAs(QString fileName, bool saveSyncronized) {
    //qDebug() << "Saving: " << fileName;
    ParametersConfiguration::lock_.lockForWrite();
    QFile data(fileName);
    if (!data.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    for (int i = 0; i < sections_.size(); i++) {
        for (unsigned int j = 0; j < sections_[i]->size(); j++) {
            ParameterElementData *e = get((*sections_[i])[j]->name());
            if (!parentData_ || !saveSyncronized || !e->syncWithUpperLevel()) {
                if(e->locked()) data.write(QString("# LOCKED: YES\n").toLatin1());
                if(e->isWrong()) data.write(QString("# ISWRONG: YES\n").toLatin1());
                data.write(QString("set " + e->name() + " = " + '"' + e->value().toString() + '"' + "\n").toLatin1());
            }
        }
    }

    data.write("#\n#=============================================================================\n#\n", 83);
    setModified(false);
    emit saving();
    data.close();
    ParametersConfiguration::lock_.unlock();
}

ParameterElementData* ParametersConfiguration::get(QString element) {
    if (!lookup_.contains(element.toLower())) {
        qDebug() << element.toLower() << " was asked, but not found";
    }
    return lookup_[element.toLower()];
}

QString ParametersConfiguration::getValue(QString element) {
    QString val;
    if(get(element)) val = get(element)->value().toString();
    return val;
}

QString ParametersConfiguration::getRoundedValue(QString element, int decimalDigits) {
    QVariant val;
    if(get(element)) val = get(element)->value();
    QString str = val.toString().trimmed();
    if(val.canConvert<double>() && str != "--" && str != "-") {
        double num = val.toDouble();
        return QString::number(num, 'f', decimalDigits);
    } else {
        return val.toString();
    }
}


QVariant ParametersConfiguration::getVariant(QString element) {
    QVariant val;
    if(get(element)) val = get(element)->value();
    return val;
}

QPointF ParametersConfiguration::getQPointF(QString element) {
    QPointF val;
    if(get(element)) val = get(element)->toQPointF();
    return val;
}


QString ParametersConfiguration::getLabel(QString element) {
    QString val;
    if(get(element)) val = get(element)->label();
    return val;
}

bool ParametersConfiguration::elementExist(QString element) {
    if(get(element)) return true;
    else return false;
}

int ParametersConfiguration::set(QString element, QString value, bool saveOnDisk) {
    if (lookup_[element.toLower()] == NULL) return 0;

    if (!lookup_[element.toLower()]->locked()) {
        if (lookup_[element.toLower()]->isWrong()) {
            lookup_[element.toLower()]->setIsWrong(false);
        }
        
        if (lookup_[element.toLower()]->value().toString() != value) {
            lookup_[element.toLower()]->setValue(value);
            if(saveOnDisk) setModified(true);
        }
    }
    
    return 1;
}

int ParametersConfiguration::setForce(QString element, QString value, bool saveOnDisk) {
    if (lookup_[element.toLower()] == NULL) return 0;

    if (lookup_[element.toLower()]->isWrong()) {
        lookup_[element.toLower()]->setIsWrong(false);
    }
    
    if (lookup_[element.toLower()]->value().toString() != value) {
        lookup_[element.toLower()]->setValue(value);
        if(saveOnDisk) setModified(true);
    }

    return 1;
}

void ParametersConfiguration::setModified(bool isModified) {
    modified_ = isModified;
    if (modified_ && projectData.isAutoSave()) save();

    emit dataModified(modified_);
}

bool ParametersConfiguration::isModified() {
    return modified_;
}

void ParametersConfiguration::printElements() {
    
}
