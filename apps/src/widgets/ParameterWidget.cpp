#include <QVBoxLayout>
#include <QFrame>
#include <QDebug>

#include "ParameterWidget.h"
#include "ParameterValidator.h"

ParametersWidget::ParametersWidget(ParametersConfiguration* conf, QWidget* parent)
: QScrollArea(parent), data(conf) {
    initialize(0, QStringList());
}

ParametersWidget::ParametersWidget(ParametersConfiguration* conf, int userLevel, QWidget *parent)
: QScrollArea(parent), data(conf) {
    initialize(userLevel, QStringList());
}

ParametersWidget::ParametersWidget(ParametersConfiguration* conf, QStringList parametersDisplayed, int userLevel, QWidget *parent)
: QScrollArea(parent), data(conf) {
    initialize(userLevel, parametersDisplayed);
}

void ParametersWidget::setSelectionUserLevel(int level){
    if(level == userLevel_) return;
    userLevel_ = level;
    changeFormWidget();
}

void ParametersWidget::changeParametersDisplayed(const QStringList& toBeDisplayed) {
    if(toBeDisplayed.contains("*")) parametersDisplayed_ = data->getLookupTable().keys();
    else parametersDisplayed_ = toBeDisplayed;
    
    parametersActuallyShown_ = parametersDisplayed_;
    changeFormWidget();
}

void ParametersWidget::initialize(int userLevel, const QStringList& toBeDisplayed) {
    userLevel_ = userLevel;
    parametersDisplayed_ = toBeDisplayed;
    parametersActuallyShown_ = toBeDisplayed;

    setWidgetResizable(true);
    setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    setWidget(formWidget());
    
    changeFormWidget();
    
}

void ParametersWidget::resetConf(ParametersConfiguration* conf) {
    data = conf;
    setWidget(formWidget());
    
    changeFormWidget();
}


void ParametersWidget::changeFormWidget() {
    for(int i=0; i< sections_.size(); ++i) {
        sections_[i]->changeDisplayedParameters(userLevel_, parametersActuallyShown_);
    }
    update();
    updateGeometry();
}

QWidget* ParametersWidget::formWidget() {
    QWidget* widget = new QWidget();

    QVBoxLayout *generalLayout = new QVBoxLayout();
    generalLayout->setMargin(0);
    generalLayout->setSpacing(10);
    generalLayout->setAlignment(Qt::AlignTop);
    
    sections_.clear();
    ParameterSection* sectionHeader;
    for (unsigned int i = 0; i < data->size(); i++) {
        sectionHeader = new ParameterSection((*data)[i]->title());
        sections_.append(sectionHeader);
        ParameterElementData *element;
        for (unsigned int j = 0; j < (*data)[i]->size(); j++) {
            element = (*(*data)[i])[j];
            sectionHeader->addParameter(element);

        }
        
        sectionHeader->finishAddingParameters();
        sectionHeader->loadFromConfig();
        generalLayout->addWidget(sectionHeader);
    }

    widget->setLayout(generalLayout);
    return widget;
}

void ParametersWidget::load() {
    for(int i=0; i< sections_.size(); ++i) {
        sections_[i]->loadFromConfig();
    }
}

void ParametersWidget::resetParameters(const QMap<QString, QString>& toBeReset) {
    if(toBeReset.isEmpty()) return;
    for(int i=0; i< sections_.size(); ++i) {
        sections_[i]->resetParameters(toBeReset);
    }
}

void ParametersWidget::searchParams(const QString& text) {
    parametersActuallyShown_.clear();
    
    QString searchText = text.trimmed().toLower();
    
    if(searchText == "") {
        parametersActuallyShown_ = parametersDisplayed_;
        changeFormWidget();
        return;
    }
    
    for(int i=0; i<parametersDisplayed_.size(); ++i) {
        if(data->elementExist(parametersDisplayed_[i])) {
            if(parametersDisplayed_[i].contains(searchText, Qt::CaseInsensitive) || 
                    data->getLabel(parametersDisplayed_[i]).contains(searchText, Qt::CaseInsensitive)) {
                parametersActuallyShown_.append(parametersDisplayed_[i]);
            }
        }
    }
    
    changeFormWidget();
    
}