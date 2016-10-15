#include <QDebug>

#include "ProjectData.h"
#include "ApplicationData.h"

#include "ParameterElementData.h"
#include "ParameterSectionData.h"
#include "ParameterConfiguration.h"

QReadWriteLock ParametersConfiguration::lock_;

ParametersConfiguration::ParametersConfiguration(const QString& referenceFileName, const QString& valuesFileName, ParametersConfiguration* parentData, QObject* parent)
: QObject(parent), parentData_(parentData), saveFileName(valuesFileName) {
    empty = false;
    if (!parseDataFile(referenceFileName)) {
        empty = true;
        qDebug() << "Datafile read error: " << referenceFileName;
    }
    setModified(false);

    resetUserValues(valuesFileName);
}

bool ParametersConfiguration::parseDataFile(const QString& fileName) {
    QFile data(fileName);
    if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) return false;

    sections.clear();

    QString lineData;
    ParameterSectionData *section = 0;
    qint64 pos = -1;
    bool lock = false;
    bool isWrong = false;
    while (!data.atEnd() && pos != data.pos()) {
        pos = data.pos();
        lineData = data.readLine().trimmed();
        lineData.remove('#');
        lineData = lineData.trimmed();

        if (lineData.toLower().startsWith("section:")) {
            lineData.remove(0, 8);
            section = new ParameterSectionData(lineData.simplified(), this);
            sections << section;
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
  
                ParameterElementData* element = new ParameterElementData(val[0], section);
                if (element->syncWithUpperLevel() && parentData_ && parentData_->lookup.contains(val[0].toLower())) {
                    //qDebug() << "Syncing with upper: " << val[0];
                    delete element;
                    element = parentData_->get(val[0]);
                } else {
                    element->setValue(val[1]);
                    element->setLock(lock);
                    element->setIsWrong(isWrong);
                }

                section->append(element);
                lookup.insert(element->name().toLower(), element);
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
    if (!data.open(QIODevice::ReadOnly | QIODevice::Text)) return false;

    QString lineData;
    ParameterSectionData *section = 0;
    qint64 pos = -1;
    bool lock = false;
    bool isWrong = false;
    while (!data.atEnd() && pos != data.pos()) {
        pos = data.pos();
        lineData = data.readLine().trimmed();
        lineData.remove('#');
        lineData = lineData.trimmed();

        if (lineData.toLower().startsWith("section:")) {
            lineData.remove(0, 8);
            section = new ParameterSectionData(lineData.simplified(), this);
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

                if (lookup.contains(val[0].toLower())) {
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
    return empty;
}

bool ParametersConfiguration::hasParent() {
    if (parentData_ == 0) return false;
    else return true;
}

QString ParametersConfiguration::dataFileName() {
    return saveFileName;
}


unsigned int ParametersConfiguration::size() {
    return sections.size();
}

QMap<QString, ParameterElementData*> ParametersConfiguration::getLookupTable() {
    return lookup;
}


ParameterSectionData* ParametersConfiguration::operator[](unsigned int i) {
    return sections[i];
}

void ParametersConfiguration::reload() {
    resetUserValues(saveFileName);
    emit loading();
    setModified(false);
}

void ParametersConfiguration::save() {
    saveAs(saveFileName, true);
}

void ParametersConfiguration::saveAs(QString fileName, bool saveSyncronized) {
    qDebug() << "Saving: " << fileName;
    ParametersConfiguration::lock_.lockForWrite();
    QFile data(fileName);
    if (!data.open(QIODevice::WriteOnly | QIODevice::Text)) return;

    for (int i = 0; i < sections.size(); i++) {
        for (unsigned int j = 0; j < sections[i]->size(); j++) {
            ParameterElementData *e = get((*sections[i])[j]->name());
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
    if (!lookup.contains(element.toLower())) {
        qDebug() << element.toLower() << " was asked, but not found";
    }
    return lookup[element.toLower()];
}

QString ParametersConfiguration::getValue(QString element) {
    QString val;
    if(get(element)) val = get(element)->value().toString();
    return val;
}


int ParametersConfiguration::set(QString element, QString value, bool saveOnDisk) {
    if (lookup[element.toLower()] == NULL) return 0;

    if (!lookup[element.toLower()]->locked()) {
        if (lookup[element.toLower()]->isWrong()) {
            lookup[element.toLower()]->setIsWrong(false);
        }
        
        if (lookup[element.toLower()]->value().toString() != value) {
            lookup[element.toLower()]->setValue(value);
            if(saveOnDisk) setModified(true);
        }
    }
    
    return 1;
}

int ParametersConfiguration::setForce(QString element, QString value, bool saveOnDisk) {
    if (lookup[element.toLower()] == NULL) return 0;

    if (lookup[element.toLower()]->isWrong()) {
        lookup[element.toLower()]->setIsWrong(false);
    }
    
    if (lookup[element.toLower()]->value().toString() != value) {
        lookup[element.toLower()]->setValue(value);
        if(saveOnDisk) setModified(true);
    }

    return 1;
}

void ParametersConfiguration::setModified(bool isModified) {
    modified = isModified;
    if (modified && projectData.isAutoSave()) save();

    emit dataModified(modified);
}

bool ParametersConfiguration::isModified() {
    return modified;
}

void ParametersConfiguration::printElements() {
    
}