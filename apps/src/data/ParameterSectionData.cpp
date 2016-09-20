#include "ParameterSectionData.h"
#include "ParameterElementData.h"
#include "ParameterConfiguration.h"

ParameterSectionData::ParameterSectionData(QString title, ParametersConfiguration* parent)
: QObject(parent) {
    sectionTitle_ = title;
}

void ParameterSectionData::append(ParameterElementData *e) {
    elements_ << e;
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

ParametersConfiguration* ParameterSectionData::getConf() {
    return static_cast<ParametersConfiguration*>(parent());
}

