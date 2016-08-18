#include <QVBoxLayout>
#include <QFrame>

#include "parameter_widget.h"

ParametersWidget::ParametersWidget(confData* conf, QWidget* parent)
: QWidget(parent), data(conf) {
    initialize(0, QStringList());
}

ParametersWidget::ParametersWidget(confData* conf, int userLevel, QWidget *parent)
: QWidget(parent), data(conf) {
    initialize(userLevel, QStringList());
}

ParametersWidget::ParametersWidget(confData* conf, QStringList parametersDisplayed, int userLevel, QWidget *parent)
: QWidget(parent), data(conf) {
    initialize(userLevel, parametersDisplayed);
}

void ParametersWidget::setSelectionUserLevel(int level){
    if(level == userLevel_) return;
    userLevel_ = level;
    changeFormWidget();
}

void ParametersWidget::changeParametersDisplayed(const QStringList& toBeDisplayed) {
    parametersDisplayed_ = toBeDisplayed;
    changeFormWidget();
}

void ParametersWidget::initialize(int userLevel, const QStringList& toBeDisplayed) {
    userLevel_ = userLevel;
    parametersDisplayed_ = toBeDisplayed;

    scrollArea_ = new QScrollArea();
    scrollArea_->setWidgetResizable(true);
    scrollArea_->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    scrollArea_->setWidget(formWidget());

    mainLayout_ = new QHBoxLayout();
    mainLayout_->setSpacing(0);
    mainLayout_->setMargin(0);
    mainLayout_->addWidget(scrollArea_);

    setLayout(mainLayout_); 
    
}

void ParametersWidget::changeFormWidget() {
    for(int i=0; i< sections_.size(); ++i) {
        sections_[i]->changeDisplayedParameters(userLevel_, parametersDisplayed_);
    }
    mainLayout_->invalidate();
    mainLayout_->update();
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
        sectionHeader = new ParameterSection(data, (*data)[i]->title());
        sections_.append(sectionHeader);
        confElement *element;
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
    
}