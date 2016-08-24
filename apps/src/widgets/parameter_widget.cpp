#include <QVBoxLayout>
#include <QFrame>
#include <QtGui/qlineedit.h>

#include "parameter_widget.h"
#include "parameter_validator.h"

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
    searchBox->setText("");
    parametersActuallyShown_ = parametersDisplayed_;
    changeFormWidget();
}

void ParametersWidget::initialize(int userLevel, const QStringList& toBeDisplayed) {
    userLevel_ = userLevel;
    parametersDisplayed_ = toBeDisplayed;
    parametersActuallyShown_ = toBeDisplayed;

    scrollArea_ = new QScrollArea();
    scrollArea_->setWidgetResizable(true);
    scrollArea_->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    scrollArea_->setWidget(formWidget());
    
    searchBox = new QLineEdit;
    searchBox->setFrame(false);
    connect(searchBox, SIGNAL(editingFinished()), this, SLOT(searchParams()));
    
    graphicalButton* searchButton = new graphicalButton(data->getIcon("search"));
    searchButton->setFixedSize(QSize(searchBox->sizeHint().height(), searchBox->sizeHint().height()));
    connect(searchButton, SIGNAL(clicked()), this, SLOT(searchParams()));
    
    QLabel* searchLabel = new QLabel("Search Parameters");
    QFont font = searchLabel->font();
    font.setBold(true);
    searchLabel->setFont(font);
    
    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    
    QHBoxLayout* searchLayout = new QHBoxLayout;
    searchLayout->addWidget(searchLabel);
    searchLayout->addWidget(spacer);
    searchLayout->addWidget(searchBox);
    searchLayout->addWidget(searchButton);

    searchWidget = new QWidget;
    searchWidget->setLayout(searchLayout);
    
    mainLayout_ = new QVBoxLayout();
    mainLayout_->setSpacing(0);
    mainLayout_->setMargin(0);
    mainLayout_->addWidget(searchWidget);
    mainLayout_->addWidget(scrollArea_);

    setLayout(mainLayout_); 
    
}

void ParametersWidget::changeFormWidget() {
    for(int i=0; i< sections_.size(); ++i) {
        sections_[i]->changeDisplayedParameters(userLevel_, parametersActuallyShown_);
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
    if(toBeReset.isEmpty()) return;
    for(int i=0; i< sections_.size(); ++i) {
        sections_[i]->resetParameters(toBeReset);
    }
}

void ParametersWidget::searchParams() {
    parametersActuallyShown_.clear();
    
    QString searchText = searchBox->text().trimmed().toLower();
    
    if(searchText == "") {
        parametersActuallyShown_ = parametersDisplayed_;
        changeFormWidget();
        return;
    }
    
    for(int i=0; i<parametersDisplayed_.size(); ++i) {
        confElement* e = data->get(parametersDisplayed_[i]);
        if(e) {
            if(e->get("valuelabel").contains(searchText, Qt::CaseInsensitive) || 
                    e->get("label").contains(searchText, Qt::CaseInsensitive)) {
                parametersActuallyShown_.append(parametersDisplayed_[i]);
            }
        }
    }
    
    changeFormWidget();
    
}