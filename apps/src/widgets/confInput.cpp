
#include <iostream>
#include <QListView>
#include <QApplication>
#include <QDebug>
#include <QFileDialog>
#include <QMessageBox>

#include "confInput.h"
#include "noScrollComboBox.h"
#include "confValidator.h"

using namespace std;

confInput::confInput(confData *conf, confElement *e, QWidget *parent)
: QWidget(parent), data(conf), element(e) {

    setAutoFillBackground(true);
    setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Maximum);

    layout_ = new QGridLayout(this);

    layout_->setMargin(1);
    layout_->setSpacing(2);
    layout_->setAlignment(Qt::AlignLeft);

    confElement::TypeInfo typeInfo_ = element->typeInfo();

    if (typeInfo_.type == confElement::Type::TEXT_EDIT) inputWidget_ = setupEditWidget();
    else if (typeInfo_.type == confElement::Type::INT) inputWidget_ = setupEditWidget();
    else if (typeInfo_.type == confElement::Type::FLOAT) inputWidget_ = setupEditWidget();
    else if (typeInfo_.type == confElement::Type::BOOL) inputWidget_ = setupBoolWidget();
    else if (typeInfo_.type == confElement::Type::FILE) inputWidget_ = setupDirWidget(false);
    else if (typeInfo_.type == confElement::Type::DIRECTORY) inputWidget_ = setupDirWidget(true);
    else if (typeInfo_.type == confElement::Type::DROP_DOWN) inputWidget_ = setupDropDownWidget();
    else inputWidget_ = new QWidget(this);

    lockButton_ = new graphicalButton(data->getIcon("lock"));
    lockButton_->setCheckable(true);
    lockButton_->setFixedSize(QSize(18, 18));
    connect(lockButton_, SIGNAL(stateChanged(int)), this, SLOT(setReadOnlyState(int)));

    layout_->addWidget(lockButton_, 0, 0);
    layout_->addWidget(inputWidget_, 0, 1);

    setLayout(layout_);
}

void confInput::saveValue(const QString& value) {
    QStringList errors = confValidator::valueErrors(element->typeInfo(), value);

    if (errors.size() != 0) {
        QString err, fullErr;
        foreach(err, errors) fullErr += err + "\n";
        QMessageBox::warning(this, tr("Error in setting value for parameter"), fullErr);
        return;
    }
    
    element->set("value", value);
    if (element->isWrong()) {
        element->setIsWrong(false);
        QPalette pal = inputWidget_->palette();
        pal.setColor(QPalette::Base, Qt::white);
        inputWidget_->setPalette(pal);
    }
    data->setModified(true);
}

void confInput::load() {
    bool lock = element->locked();
    lockButton_->setChecked(lock);
    inputWidget_->setDisabled(lock);
    
    bool isWrong = element->isWrong();
    
    QPalette pal = inputWidget_->palette();
    if (isWrong) {
        QString resourceDir = QString(data->getDir("config") + "resource/");
        QPixmap texturePix = QPixmap(resourceDir + "is-wrong-bg.png");
        QBrush texture;
        if (!texturePix.isNull()) texture = QBrush(texturePix);
        else texture = QBrush(Qt::blue, Qt::BDiagPattern);
        pal.setBrush(QPalette::Base, texture);
    } else {
        pal.setColor(QPalette::Base, Qt::white);
    }
    
    inputWidget_->setPalette(pal);
    emit shouldLoadValue(element->get("value"));
    update();
}

void confInput::setReadOnlyState(int state) {
    if (state == Qt::Checked) {
        inputWidget_->setDisabled(true);
    } else if (state == Qt::Unchecked) {
        inputWidget_->setDisabled(false);
    }

    if (lockButton_ != NULL) {
        if (lockButton_->isChecked()) element->setLock(true);
        else element->setLock(false);
    }
}

int confInput::userLevel() {
    return element->userLevel();
}

void confInput::show() {
    inputWidget_->show();
    updateGeometry();
    
    layout_->invalidate();
    layout_->update();
    update();
    QWidget::show();
    emit shown();
}

QWidget* confInput::setupDirWidget(bool isDir) {
    BrowserWidget* widget = new BrowserWidget(this);
    if (isDir) widget->setType(BrowserWidget::BrowseType::DIRECTORY);
    widget->setDefaultPath(data->getDir("project"));

    connect(this, SIGNAL(shouldLoadValue(const QString&)),
            widget, SLOT(setPath(const QString&)));
    connect(widget, SIGNAL(pathChanged(const QString&)),
            this, SLOT(saveValue(const QString&)));

    return widget;
}

QWidget* confInput::setupDropDownWidget() {
    QComboBox* combo = new ComboInputWidget(this);
    combo->setFocusPolicy(Qt::StrongFocus);
    combo->addItems(element->typeInfo().properties);

    connect(this, &confInput::shouldLoadValue, 
                [ = ](const QString & value){combo->setCurrentIndex(value.toInt());});
    connect(combo, static_cast<void(QComboBox::*)(int)> (&QComboBox::currentIndexChanged),
            [ = ](int index){saveValue(QString::number(index));});

    return combo;
}

QWidget* confInput::setupEditWidget() {
    confElement::TypeInfo info = element->typeInfo();
    EditSetWidget* widget;

    if (info.type == confElement::Type::INT) {
        widget = new EditIntSetWidget(info.count, this);
        static_cast<EditIntSetWidget*> (widget)->setRange(info.deduceMinMaxPairs(info.properties));
    } else if (info.type == confElement::Type::FLOAT) {
        widget = new EditDoubleSetWidget(info.count, this);
        static_cast<EditDoubleSetWidget*> (widget)->setRange(info.deduceMinMaxPairs(info.properties));
    } else {
        widget = new EditSetWidget(info.count, this);
    }

    connect(this, SIGNAL(shouldLoadValue(const QString&)),
            widget, SLOT(setValue(const QString&)));
    connect(widget, SIGNAL(valueChanged(const QString&)),
            this, SLOT(saveValue(const QString&)));

    return widget;
}

QWidget* confInput::setupBoolWidget() {
    YesNoWidget* widget = new YesNoWidget(this);

    connect(this, &confInput::shouldLoadValue,
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
