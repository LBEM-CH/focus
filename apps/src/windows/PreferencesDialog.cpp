/* 
 * Author: Nikhil Biyani - nikhil(dot)biyani(at)gmail(dot)com
 *
 * This file is a part of 2dx.
 * 
 * 2dx is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or any 
 * later version.
 * 
 * 2dx is distributed in the hope that it will be useful, but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public 
 * License for more details <http://www.gnu.org/licenses/>.
 */

#include <QtWidgets>
#include <QFontDatabase>
#include <QPalette>
#include <QColor>
#include <QTableView>
#include <QToolBar>
#include <QAction>
#include <QActionGroup>
#include <QFormLayout>
#include <QSpinBox>

#include "UserPreferences.h"
#include "ApplicationData.h"
#include "UserPreferenceData.h"

#include "PreferencesDialog.h"


PreferencesDialog::PreferencesDialog(QWidget* parent)
: QDialog(parent) {

    pagesWidget_ = new QStackedWidget;
    pagesWidget_->addWidget(getAppsPage());
    pagesWidget_->addWidget(getApperancePage());
    pagesWidget_->setCurrentIndex(0);

    QPushButton *closeButton = new QPushButton(tr("Close"));
    connect(closeButton, &QAbstractButton::clicked, this, &QWidget::close);

    QFrame* vLine = new QFrame(this);
    vLine->setFrameStyle(QFrame::VLine | QFrame::Sunken);
    
    QHBoxLayout *buttonsLayout = new QHBoxLayout;
    buttonsLayout->addStretch(1);
    buttonsLayout->addWidget(closeButton);
    
    QVBoxLayout *verticalLayout = new QVBoxLayout;
    verticalLayout->addWidget(pagesWidget_, 1);
    verticalLayout->addSpacing(12);
    verticalLayout->addLayout(buttonsLayout, 0);
    
    QHBoxLayout *horizontalLayout = new QHBoxLayout;
    horizontalLayout->setMargin(0);
    horizontalLayout->setSpacing(0);
    horizontalLayout->addWidget(setupToolBar(), 0);
    horizontalLayout->addWidget(vLine, 0);
    horizontalLayout->addLayout(verticalLayout, 1);
    
    setLayout(horizontalLayout);

    setWindowTitle(tr("Preferences"));
    setModal(true);
    resize(500, 400);
}

QToolBar* PreferencesDialog::setupToolBar() {  
    QToolBar* contentsWidget_ = new QToolBar("Select");
    contentsWidget_->setOrientation(Qt::Vertical);
    contentsWidget_->setIconSize(QSize(36, 36));

    QAction *externalButton = new QAction(ApplicationData::icon("external"), tr("Linked Apps"), contentsWidget_);
    externalButton->setCheckable(true);
    connect(externalButton, &QAction::triggered,
            [=] () {
                pagesWidget_->setCurrentIndex(0);
            });
            
    QAction *appearanceButton = new QAction(ApplicationData::icon("appearance"), tr("Appearance"), contentsWidget_);
    appearanceButton->setCheckable(true);
    connect(appearanceButton, &QAction::triggered,
            [=] () {
                pagesWidget_->setCurrentIndex(1);
            });
            
    QActionGroup* group = new QActionGroup(this);
    group->setExclusive(true);
    group->addAction(externalButton);
    group->addAction(appearanceButton);
    
    contentsWidget_->addAction(externalButton);
    contentsWidget_->addAction(appearanceButton);
    
    externalButton->setChecked(true);
    return contentsWidget_;
}

QWidget* PreferencesDialog::getApperancePage() {
    QWidget* widget = new QWidget();

    //---------------------
    // Font
    //---------------------
    QGroupBox *fontGroup = new QGroupBox(tr("Font"));

    //Font Size
    QSpinBox* fontSizeCombo = new QSpinBox;
    fontSizeCombo->setMinimum(8);
    fontSizeCombo->setMaximum(14);
    fontSizeCombo->setValue(QApplication::font().pointSize());

    connect(fontSizeCombo, static_cast<void(QSpinBox::*)(int)>(&QSpinBox::valueChanged), 
            [=] (int i){UserPreferences().setFontSize(i);});

    //Font Weight
    QComboBox *fontWeightCombo = new QComboBox;
    fontWeightCombo->addItems(QStringList() << "0" << "25" << "50" << "75");
    fontWeightCombo->setCurrentText(QString::number(QApplication::font().weight()));

    connect(fontWeightCombo, static_cast<void(QComboBox::*)(const QString&)> (&QComboBox::currentTextChanged),
            [ = ] (const QString & value){UserPreferences().setFontWeight(value);});

    QFormLayout *fontLayout = new QFormLayout;
    fontLayout->setRowWrapPolicy(QFormLayout::WrapLongRows);
    fontLayout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    fontLayout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    fontLayout->setLabelAlignment(Qt::AlignRight);
    fontLayout->addRow("Font Size", fontSizeCombo);
    fontLayout->addRow("Font Weight", fontWeightCombo);

    fontGroup->setLayout(fontLayout);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addWidget(fontGroup);
    mainLayout->addStretch(1);
    widget->setLayout(mainLayout);
    return widget;
}

QWidget* PreferencesDialog::getAppsPage() {
    QWidget* widget = new QWidget();
    QVBoxLayout *layout = new QVBoxLayout;
    layout->addStretch(0);

    ParametersConfiguration *data = userPreferenceData.data();
    for (unsigned int i = 0; i < data->size(); i++) {
        QGroupBox* sectionBox = new QGroupBox((*data)[i]->title());
        QFormLayout* formLayout_ = new QFormLayout();
        formLayout_->setRowWrapPolicy(QFormLayout::WrapLongRows);
        formLayout_->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
        formLayout_->setFormAlignment(Qt::AlignLeft | Qt::AlignVCenter);
        formLayout_->setLabelAlignment(Qt::AlignLeft);

        for (unsigned int j = 0; j < (*data)[i]->size(); j++) {
            ParameterElementData* element = (*(*data)[i])[j];
            QLabel* label = new QLabel(element->label());
            QString toolTip = element->name() + "<br><br>" + element->legend();
            label->setToolTip(toolTip);
            LineEditSet* editWid = new LineEditSet(1, sectionBox);
            editWid->setValue(element->value().toString());
            elementToWidget.insert(element->name(), editWid);
            formLayout_->addRow(label, editWid);   
        }
        
        sectionBox->setLayout(formLayout_);
        layout->addWidget(sectionBox);
    }

    QPushButton *saveButton = new QPushButton("Save", this);
    connect(saveButton, SIGNAL(released()), this, SLOT(saveApps()));   
    QPushButton *resetButton = new QPushButton("Reset", this);
    connect(resetButton, SIGNAL(released()), this, SLOT(loadApps()));
    
    QHBoxLayout* buttonLayout = new QHBoxLayout;
    buttonLayout->addWidget(resetButton);
    buttonLayout->addWidget(saveButton);
    
    layout->addLayout(buttonLayout);
    layout->addStretch(1);
    widget->setLayout(layout);
    loadApps();
    return widget;
}

void PreferencesDialog::saveApps() {
    for(int i=0; i< elementToWidget.keys().size(); ++i) {
        QString element = elementToWidget.keys()[i];
        UserData::Instance().set(element, elementToWidget[element]->valueAt(0));
    }
    close();
}

void PreferencesDialog::loadApps() {
    for(int i=0; i< elementToWidget.keys().size(); ++i) {
        QString element = elementToWidget.keys()[i];
        elementToWidget[element]->setValue(UserData::Instance().get(element));
    }
}
