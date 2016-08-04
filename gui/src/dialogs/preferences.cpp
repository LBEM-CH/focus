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

#include "user_preferences.h"
#include "confModel.h"

#include "preferences.h"

PreferencesDialog::PreferencesDialog(confData* data, QWidget* parent)
: QDialog(parent), mainData(data) {

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
    resize(300, 400);
}

QToolBar* PreferencesDialog::setupToolBar() {  
    QToolBar* contentsWidget_ = new QToolBar("Select");
    contentsWidget_->setOrientation(Qt::Vertical);
    contentsWidget_->setIconSize(QSize(36, 36));

    QAction *externalButton = new QAction(*(mainData->getIcon("external")), tr("Linked Apps"), contentsWidget_);
    externalButton->setCheckable(true);
    connect(externalButton, &QAction::triggered,
            [=] () {
                pagesWidget_->setCurrentIndex(0);
            });
            
    QAction *appearanceButton = new QAction(*(mainData->getIcon("appearance")), tr("Appearance"), contentsWidget_);
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
    QComboBox *fontSizeCombo = new QComboBox;
    for (int i = 8; i < 16; ++i) fontSizeCombo->addItem(QString::number(i));
    fontSizeCombo->setCurrentText(QString::number(QApplication::font().pointSize()));

    connect(fontSizeCombo, static_cast<void(QComboBox::*)(const QString&)> (&QComboBox::currentTextChanged),
            [ = ] (const QString & value){UserPreferences(mainData).setFontSize(value);});

    //Font Weight
    QComboBox *fontWeightCombo = new QComboBox;
    fontWeightCombo->addItems(QStringList() << "0" << "25" << "50" << "75");
    fontWeightCombo->setCurrentText(QString::number(QApplication::font().weight()));

    connect(fontWeightCombo, static_cast<void(QComboBox::*)(const QString&)> (&QComboBox::currentTextChanged),
            [ = ] (const QString & value){UserPreferences(mainData).setFontWeight(value);});

    //Font Family
    QComboBox *fontFamilyCombo = new QComboBox;
    fontFamilyCombo->addItems(QFontDatabase().families(QFontDatabase::Latin));
    fontFamilyCombo->setCurrentText(QApplication::font().family());

    connect(fontFamilyCombo, static_cast<void(QComboBox::*)(const QString&)> (&QComboBox::currentTextChanged),
            [ = ] (const QString & value){UserPreferences(mainData).setFontFamily(value);});

    QFormLayout *fontLayout = new QFormLayout;
    fontLayout->setRowWrapPolicy(QFormLayout::WrapLongRows);
    fontLayout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    fontLayout->setFormAlignment(Qt::AlignHCenter | Qt::AlignTop);
    fontLayout->setLabelAlignment(Qt::AlignRight);
    fontLayout->addRow("Font Size", fontSizeCombo);
    fontLayout->addRow("Font Weight", fontWeightCombo);
    fontLayout->addRow("Font Family", fontFamilyCombo);

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

    QHBoxLayout *top = new QHBoxLayout;
    QHBoxLayout *middle = new QHBoxLayout;
    QHBoxLayout *bottom = new QHBoxLayout;

    layout->addLayout(top, 0);
    layout->addLayout(middle, 1);
    layout->addLayout(bottom, 0);

    QFont font;
    font.setStyleHint(QFont::Times);
    font.setBold(true);
    font.setPixelSize(14);

    QLabel *title = new QLabel("External Applications (Double Click to Edit)", this);
    title->setFont(font);
    title->setTextFormat(Qt::RichText);
    top->addWidget(title);

    confModel *dataModel = new confModel(mainData->getSubConf("appConf"), this);
    QTableView* preferencesTable = new QTableView(this);
    preferencesTable->setModel(dataModel);
    preferencesTable->horizontalHeader()->hide();
    preferencesTable->verticalHeader()->hide();
    preferencesTable->setColumnHidden(0, true);
    preferencesTable->setColumnHidden(2, true);
    preferencesTable->setRowHidden(0, true);
    preferencesTable->resizeColumnsToContents();
    preferencesTable->setShowGrid(false);
    preferencesTable->setAlternatingRowColors(true);
    middle->addWidget(preferencesTable);

    QPushButton *saveButton = new QPushButton("Save", this);
    connect(saveButton, SIGNAL(released()), this, SLOT(saveApps()));
    bottom->addWidget(saveButton);

    widget->setLayout(layout);
    return widget;
}

void PreferencesDialog::saveApps() {
    mainData->getSubConf("appConf")->save();
    close();
}
