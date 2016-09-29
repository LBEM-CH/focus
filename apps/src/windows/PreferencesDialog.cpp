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
#include <QToolButton>
#include <QButtonGroup>
#include <QFormLayout>
#include <QSpinBox>

#include "UserPreferences.h"
#include "ApplicationData.h"
#include "UserPreferenceData.h"

#include "PreferencesDialog.h"
#include "ParameterWidget.h"
#include "ProjectData.h"


PreferencesDialog::PreferencesDialog(QWidget* parent)
: QDialog(parent) {

    pagesWidget_ = new QStackedWidget;
    pagesWidget_->addWidget(getGeneralPage());
    pagesWidget_->addWidget(getPathsPage());
    pagesWidget_->addWidget(getViewersPage());
    pagesWidget_->addWidget(getFontsPage());
    pagesWidget_->setCurrentIndex(0);

    QPushButton *closeButton = new QPushButton(tr("Close"));
    connect(closeButton, &QAbstractButton::clicked, this, &QWidget::close);
    
    QHBoxLayout *buttonsLayout = new QHBoxLayout;
    buttonsLayout->setMargin(10);
    buttonsLayout->addStretch(1);
    buttonsLayout->addWidget(closeButton);
    
    QFrame* hLine = new QFrame(this);
    hLine->setFrameStyle(QFrame::HLine | QFrame::Sunken);
    
    QVBoxLayout *verticalLayout = new QVBoxLayout;
    verticalLayout->setMargin(0);
    verticalLayout->setSpacing(0);
    verticalLayout->addStretch(0);
    verticalLayout->addWidget(setupToolBar(), 0);
    verticalLayout->addWidget(hLine, 0);
    verticalLayout->addWidget(pagesWidget_, 1);
    verticalLayout->addSpacing(12);
    verticalLayout->addLayout(buttonsLayout, 0);
    
    setLayout(verticalLayout);

    setWindowTitle(tr("Preferences"));
    setModal(true);
    resize(550, 450);
}

QToolBar* PreferencesDialog::setupToolBar() {
    toolBarButtonGroup_ = new QButtonGroup(this);
    toolBarButtonGroup_->setExclusive(true);
    
    contentsWidget_ = new QToolBar("Select");
    contentsWidget_->setOrientation(Qt::Horizontal);
    contentsWidget_->setIconSize(QSize(32, 32));
    
    QWidget* spacer1 = new QWidget();
    spacer1->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QWidget* spacer2 = new QWidget();
    spacer2->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    contentsWidget_->addWidget(spacer1);
    getToolButton("general", "General", 0);
    getToolButton("paths", "Paths", 1);
    getToolButton("viewer", "Viewers", 2);
    getToolButton("fonts", "Font", 3);
    contentsWidget_->addWidget(spacer2);

    return contentsWidget_;
}

void PreferencesDialog::getToolButton(const QString& ic, const QString& text, int indexOfStackedWidget) {
    QToolButton *button = new QToolButton;
    button->setIcon(ApplicationData::icon(ic));
    button->setText(text);
    button->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    button->setFixedSize(QSize(64, 64));
    button->setCheckable(true);
    if(indexOfStackedWidget == 0) button->setChecked(true);
    connect(button, &QToolButton::toggled, [ = ] (){
        pagesWidget_->setCurrentIndex(indexOfStackedWidget);
    });
    
    toolBarButtonGroup_->addButton(button);
    contentsWidget_->addWidget(button);
}

QWidget* PreferencesDialog::getGeneralPage() {
    GroupContainer* widget = new GroupContainer();
    widget->setTitle("General");
    
    QFormLayout* mainLayout = new QFormLayout;
    mainLayout->setRowWrapPolicy(QFormLayout::WrapLongRows);
    mainLayout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    mainLayout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    mainLayout->setLabelAlignment(Qt::AlignRight);
    
    QCheckBox* autoSaveBox = new QCheckBox("Automatically save configurations when they are changed");
    autoSaveBox->setChecked(UserPreferences().autoSaveConfigs());
    connect(autoSaveBox, &QCheckBox::toggled, [=](bool check){
        UserPreferences().setAutoSaveConfigs(check);
        projectData.setAutoSave(check);
    });
    mainLayout->addRow(autoSaveBox);
    
    QCheckBox* advancedBox = new QCheckBox("Show advanced by default (valid on restart)");
    advancedBox->setChecked(UserPreferences().showAdvanced());
    connect(advancedBox, &QCheckBox::toggled, [=](bool check){
        UserPreferences().setShowAdvanced(check);
    });
    mainLayout->addRow(advancedBox);
     
    QSlider* outputVerbosityControl = new QSlider;
    outputVerbosityControl->setOrientation(Qt::Horizontal);
    outputVerbosityControl->setFixedSize(100, 20);
    outputVerbosityControl->setMinimum(0);
    outputVerbosityControl->setMaximum(3);
    outputVerbosityControl->setTickPosition(QSlider::TicksBothSides);
    outputVerbosityControl->setTickInterval(1);
    outputVerbosityControl->setSingleStep(1);
    outputVerbosityControl->setValue(UserPreferences().userLevel());
    connect(outputVerbosityControl, &QSlider::valueChanged, [=] (int level) {
        UserPreferences().setUserLevel(level);
    });
    mainLayout->addRow("Default verbosity level (valid on restart)", outputVerbosityControl);
    
    widget->setContainerLayout(mainLayout);
    
    QWidget* pageWid = new QWidget;
    QVBoxLayout *layout = new QVBoxLayout;
    layout->setMargin(0);
    layout->setSpacing(0);
    layout->addWidget(widget);
    layout->addStretch(1);
    pageWid->setLayout(layout);
    
    return pageWid;
}



QWidget* PreferencesDialog::getFontsPage() {
    QWidget* widget = new QWidget();

    //---------------------
    // Font
    //---------------------
    GroupContainer *fontGroup = new GroupContainer;
    fontGroup->setTitle("Font");

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

    fontGroup->setContainerLayout(fontLayout);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->setMargin(0);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(fontGroup);
    mainLayout->addStretch(1);
    widget->setLayout(mainLayout);
    return widget;
}

QWidget* PreferencesDialog::getViewersPage() {
    ParameterSectionData* section = (*userPreferenceData.data())[1];
    QStringList parameters;
    for (unsigned int j = 0; j < section->size(); j++) parameters.append((*section)[j]->name());
    ParametersWidget* widget = new ParametersWidget(userPreferenceData.data(), parameters, 1);
    return widget;
}

QWidget* PreferencesDialog::getPathsPage() {
    ParameterSectionData* section = (*userPreferenceData.data())[0];
    QStringList parameters;
    for (unsigned int j = 0; j < section->size(); j++) parameters.append((*section)[j]->name());
    ParametersWidget* widget = new ParametersWidget(userPreferenceData.data(), parameters, 1);
    return widget;
}
