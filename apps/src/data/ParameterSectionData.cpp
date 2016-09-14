#include "ParameterSectionData.h"

ParameterSectionData::ParameterSectionData(QString title, QObject *parent)
: QObject(parent) {
    sectionTitle_ = title;
}

void ParameterSectionData::append(ParameterElementData *e) {
    elements_ << e;
    e->setParent(this);
}

QString ParameterSectionData::title() {
    return sectionTitle_;
}

unsigned int ParameterSectionData::size() {
    return elements_.size();
}

ParameterSectionData& ParameterSectionData::operator<<(ParameterElementData *e) {
    append(e);
    return *this;
}

ParameterElementData* ParameterSectionData::operator[](unsigned int i) {
    return elements_[i];
}

bool ParameterSectionData::operator==(const ParameterSectionData& section) {
    return sectionTitle_ == section.sectionTitle_;
}
