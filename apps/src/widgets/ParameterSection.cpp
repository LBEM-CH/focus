#include <iostream>

#include <QFont>
#include <QPalette>
#include <QColor>
#include <QDebug>

#include "ParameterSection.h"

ParameterSection::ParameterSection(QString sectionTitle, QWidget *parent)
: GroupContainer(parent) {
    setTitle(sectionTitle);

    formLayout_ = new QFormLayout();
    formLayout_->setRowWrapPolicy(QFormLayout::DontWrapRows);
    formLayout_->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    formLayout_->setFormAlignment(Qt::AlignLeft | Qt::AlignVCenter);
    formLayout_->setLabelAlignment(Qt::AlignLeft);
    formLayout_->setVerticalSpacing(0);

    setAutoFillBackground(true);
}

void ParameterSection::addParameter(ParameterElementData* element) {
    ParameterInput* inputWidget = new ParameterInput(element, this);
    connect(element, SIGNAL(dataChanged()), inputWidget, SLOT(load()));
    inputWidget->setFixedHeight(24);
    int paramUserLevel = element->userLevel();

    int index = formLayout_->rowCount();
    parameterRowLevelLookup_.insert(element->name(), index);
    parameterUserLevelLookup_.insert(element->name(), paramUserLevel);
    parameterInputLookup_.insert(element->name(), inputWidget);
    
    QLabel* label = new QLabel(element->label());
    label->setToolTip(getWhatsThis(element));
    if (paramUserLevel == 1) {
        QFont f = label->font();
        f.setItalic(true);
        label->setFont(f);
    }
    
    formLayout_->addRow(label, inputWidget);
}

void ParameterSection::loadFromConfig() {
    for (int i = 0; i < parameterInputLookup_.keys().size(); ++i) {
        QString parameterName = parameterInputLookup_.keys()[i];
        parameterInputLookup_[parameterName]->load();
    }
}

void ParameterSection::resetParameters(const QMap<QString, QString>& toBeReset) {
    for (int i = 0; i < parameterInputLookup_.keys().size(); ++i) {
        QString parameterName = parameterInputLookup_.keys()[i];
        if(toBeReset.keys().contains(parameterName)) {
            parameterInputLookup_[parameterName]->saveValue(toBeReset[parameterName]);
            parameterInputLookup_[parameterName]->load();
        }
    }
}

void ParameterSection::finishAddingParameters() {
    setContainerLayout(formLayout_);
}

void ParameterSection::changeDisplayedParameters(int userLevel, QStringList parametersDisplayed) {
    int shown = parameterInputLookup_.keys().size();
    for (int i = 0; i < parameterInputLookup_.keys().size(); ++i) {
        QString parameterName = parameterInputLookup_.keys()[i];
        int index = parameterRowLevelLookup_[parameterName];
        if (parameterUserLevelLookup_[parameterName] <= userLevel && (parametersDisplayed.contains(parameterName) || parametersDisplayed.contains("*"))) {
            if(formLayout_->itemAt(index, QFormLayout::LabelRole) != nullptr) formLayout_->itemAt(index, QFormLayout::LabelRole)->widget()->show();
            if(formLayout_->itemAt(index, QFormLayout::FieldRole) != nullptr) formLayout_->itemAt(index, QFormLayout::FieldRole)->widget()->show();
            if(formLayout_->itemAt(index, QFormLayout::SpanningRole) != nullptr) formLayout_->itemAt(index, QFormLayout::SpanningRole)->widget()->show();
        } else {
            if(formLayout_->itemAt(index, QFormLayout::LabelRole) != nullptr) formLayout_->itemAt(index, QFormLayout::LabelRole)->widget()->hide();
            if(formLayout_->itemAt(index, QFormLayout::FieldRole) != nullptr) formLayout_->itemAt(index, QFormLayout::FieldRole)->widget()->hide();
            if(formLayout_->itemAt(index, QFormLayout::SpanningRole) != nullptr) formLayout_->itemAt(index, QFormLayout::SpanningRole)->widget()->hide();
            shown --;
        }
    }

    if(shown <= 0) hide();
    else show();
    
    formLayout_->invalidate();
    formLayout_->update();
    update();
    updateGeometry();
}

QString ParameterSection::getWhatsThis(ParameterElementData* element) {
    QString whatsthis = element->name() + "<br><br>" + element->legend() + "<br><br>";

    ParameterTypeInfo info = element->typeInfo();
    QMap<int, QStringList> widgetRange = info.deduceMinMaxPairs(info.properties);

    if (!widgetRange.isEmpty()) whatsthis += "Range: <br>";

    foreach(int i, widgetRange.keys()) {
        if (widgetRange.size() > 1) {
            whatsthis += QString::number(i + 1) + " : ";
        }
        if (!widgetRange.value(i)[0].isEmpty()) {
            whatsthis += " Min=";
            whatsthis += widgetRange.value(i)[0];
        }
        if (widgetRange.value(i).size() > 1 && !widgetRange.value(i)[1].isEmpty()) {
            whatsthis += " and ";
            whatsthis += " Max=";
            whatsthis += widgetRange.value(i)[1];
            whatsthis += " ";
        }
        whatsthis += "<br>";
    }
    whatsthis += "Example: " + element->example();

    return whatsthis;

}