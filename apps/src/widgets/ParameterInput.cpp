#include <QListView>
#include <QApplication>
#include <QDebug>
#include <QFileDialog>
#include <QMessageBox>
#include <QDesktopServices>

#include "ParameterValidator.h"
#include "GraphicalButton.h"
#include "BrowserWidget.h"
#include "NoScrollComboBox.h"
#include "LineEditSet.h"
#include "YesNoWidget.h"

#include "ProjectData.h"
#include "ApplicationData.h"

#include "ParameterInput.h"

ParameterInput::ParameterInput(ParameterElementData *e, QWidget *parent)
: QWidget(parent), element(e) {

    setAutoFillBackground(true);
    setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Maximum);

    layout_ = new QGridLayout(this);

    layout_->setMargin(1);
    layout_->setSpacing(2);
    layout_->setAlignment(Qt::AlignLeft);

    ParameterTypeInfo typeInfo_ = element->typeInfo();

    if (typeInfo_.type == ParameterTypeInfo::Type::TEXT_EDIT) inputWidget_ = setupEditWidget();
    else if (typeInfo_.type == ParameterTypeInfo::Type::INT) inputWidget_ = setupEditWidget();
    else if (typeInfo_.type == ParameterTypeInfo::Type::FLOAT) inputWidget_ = setupEditWidget();
    else if (typeInfo_.type == ParameterTypeInfo::Type::BOOL) inputWidget_ = setupBoolWidget();
    else if (typeInfo_.type == ParameterTypeInfo::Type::FILE) inputWidget_ = setupDirWidget(false);
    else if (typeInfo_.type == ParameterTypeInfo::Type::DIRECTORY) inputWidget_ = setupDirWidget(true);
    else if (typeInfo_.type == ParameterTypeInfo::Type::DROP_DOWN) inputWidget_ = setupDropDownWidget();
    else inputWidget_ = new QWidget(this);

    lockButton_ = new GraphicalButton(ApplicationData::icon("lock"));
    lockButton_->setToolTip("Lock this parameter, no script or user would be able to change this value (until it is forced or unlocked again)");
    lockButton_->setCheckable(true);
    lockButton_->setFixedSize(QSize(18, 18));
    connect(lockButton_, SIGNAL(stateChanged(int)), this, SLOT(setReadOnlyState(int)));
    
    if(!element->lockable()) lockButton_->hide();
    
    layout_->addWidget(lockButton_, 0, 0, Qt::AlignVCenter);
    layout_->addWidget(inputWidget_, 0, 1, Qt::AlignVCenter);
    
    if(!element->helpUrl().isEmpty()) {
        GraphicalButton* linkButton_ = new GraphicalButton(ApplicationData::icon("external_link"));
        linkButton_->setToolTip("Get help from external source (opens in web-browser)");
        linkButton_->setCheckable(false);
        linkButton_->setFixedSize(18, 18);
        connect(linkButton_, &GraphicalButton::clicked, [=]{
            QDesktopServices::openUrl(QUrl(element->helpUrl().toLower()));
        });
        layout_->addWidget(linkButton_, 0, 2, Qt::AlignVCenter);
    }

    setLayout(layout_);
}

void ParameterInput::saveValue(const QString& value) {
    QStringList errors = ParameterValidator::valueErrors(element->typeInfo(), value);

    if (errors.size() != 0) {
        QString err, fullErr;
        foreach(err, errors) fullErr += err + "\n";
        QMessageBox::warning(this, tr("Error in setting value for parameter: ") + element->name(), fullErr);
        load();
        return;
    }
    
    element->getSection()->getConf()->set(element->name(), value);
}

void ParameterInput::load() {
    bool lock = element->locked();
    lockButton_->setChecked(lock);
    inputWidget_->setDisabled(lock);
    
    bool isWrong = element->isWrong();
    
    QPalette pal = inputWidget_->palette();
    if (isWrong) {
        QString resourceDir = QString(ApplicationData::configDir().canonicalPath() + "/resource/");
        QPixmap texturePix = QPixmap(resourceDir + "is-wrong-bg.png");
        QBrush texture;
        if (!texturePix.isNull()) texture = QBrush(texturePix);
        else texture = QBrush(Qt::blue, Qt::BDiagPattern);
        pal.setBrush(QPalette::Base, texture);
    } else {
        pal.setColor(QPalette::Base, Qt::white);
    }
    
    inputWidget_->setPalette(pal);
    emit shouldLoadValue(element->value().toString());
    update();
}

void ParameterInput::setReadOnlyState(int state) {
    if (state == Qt::Checked) {
        inputWidget_->setDisabled(true);
    } else if (state == Qt::Unchecked) {
        inputWidget_->setDisabled(false);
    }

    if (lockButton_ != NULL) {
        bool lock = false;
        if (lockButton_->isChecked()) lock = true;
        if(element->locked() != lock) {
            element->setLock(lock);
            element->getSection()->getConf()->save();
        }
    }
}

int ParameterInput::userLevel() {
    return element->userLevel();
}

void ParameterInput::show() {
    inputWidget_->show();
    updateGeometry();
    
    layout_->invalidate();
    layout_->update();
    update();
    QWidget::show();
    emit shown();
}

QWidget* ParameterInput::setupDirWidget(bool isDir) {
    BrowserWidget* widget = new BrowserWidget();
    if (isDir) widget->setType(BrowserWidget::BrowseType::DIRECTORY);
    widget->setDefaultPath(projectData.projectDir().canonicalPath());

    connect(this, SIGNAL(shouldLoadValue(const QString&)),
            widget, SLOT(setPath(const QString&)));
    connect(widget, SIGNAL(pathChanged(const QString&)),
            this, SLOT(saveValue(const QString&)));

    return widget;
}

QWidget* ParameterInput::setupDropDownWidget() {
    QComboBox* combo = new NoScrollComboBox(this);
    combo->setFocusPolicy(Qt::StrongFocus);
    QStringList items = element->typeInfo().properties;
    combo->addItems(items);

    connect(this, &ParameterInput::shouldLoadValue, 
                [ = ](const QString & v){
                    if(v.contains('=')) combo->setCurrentIndex(v.split('=').first().trimmed().toInt());
                    else if(combo->findText(v) >=0 ) combo->setCurrentText(v);
                    else if(QVariant(v).canConvert<int>() && QVariant(v).convert(QMetaType::Int)) {
                        combo->setCurrentIndex(v.toInt()); 
                        //qDebug() << "Loaded by index" << element->get("valueLabel") << value;
                    }
                    else {
                        qDebug() << "Error in loading the parameter" << element->name() << v;
                        qDebug() << "Possible values:";
                        qDebug() << items;
                    }
                });
    connect(combo, static_cast<void(QComboBox::*)(const QString&)> (&QComboBox::activated),
            [ = ](const QString& value) {
                if(value.contains('=')) saveValue(value.split('=').first().trimmed());
                else saveValue(value);
            });

    return combo;
}

QWidget* ParameterInput::setupEditWidget() {
    ParameterTypeInfo info = element->typeInfo();
    LineEditSet* widget;

    if (info.type == ParameterTypeInfo::Type::INT) {
        widget = new IntLineEditSet(info.count, this);
        static_cast<IntLineEditSet*> (widget)->setRange(info.deduceMinMaxPairs(info.properties));
    } else if (info.type == ParameterTypeInfo::Type::FLOAT) {
        widget = new DoubleLineEditSet(info.count, this);
        static_cast<DoubleLineEditSet*> (widget)->setRange(info.deduceMinMaxPairs(info.properties));
    } else {
        widget = new LineEditSet(info.count, this);
    }

    connect(this, SIGNAL(shouldLoadValue(const QString&)),
            widget, SLOT(setValue(const QString&)));
    connect(widget, SIGNAL(valueChanged(const QString&)),
            this, SLOT(saveValue(const QString&)));

    return widget;
}

QWidget* ParameterInput::setupBoolWidget() {
    YesNoWidget* widget = new YesNoWidget(this);

    connect(this, &ParameterInput::shouldLoadValue,
            [ = ](const QString & value){bool val = false;
        QString valStr = value.trimmed().toLower();
        if (valStr == "yes" || valStr == "y" || valStr == "1") val = true;
                widget->setValue(val);
        });

    connect(widget, &YesNoWidget::valueChanged,
            [ = ](bool value){QString val = "n";
        if (value) val = "y";
                saveValue(val); });

    return widget;
}
