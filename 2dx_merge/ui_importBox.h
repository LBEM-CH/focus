/********************************************************************************
** Form generated from reading ui file 'importBox.ui'
**
** Created: Thu Oct 16 09:53:51 2008
**      by: Qt User Interface Compiler version 4.4.0
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_IMPORTBOX_H
#define UI_IMPORTBOX_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QGridLayout>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QSpacerItem>
#include <QtGui/QSpinBox>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Dialog
{
public:
    QDialogButtonBox *buttonBox;
    QWidget *widget;
    QGridLayout *gridLayout;
    QLabel *label_2;
    QLineEdit *importFileNameBox;
    QLabel *label_5;
    QLineEdit *proteinAcronymBox;
    QSpacerItem *spacerItem;
    QSpacerItem *spacerItem1;
    QLabel *label;
    QSpinBox *tiltAngleBox;
    QSpacerItem *spacerItem2;
    QLabel *label_3;
    QLineEdit *frameNumberBox;
    QSpacerItem *spacerItem3;
    QLabel *label_4;
    QLineEdit *subImageBox;
    QSpacerItem *spacerItem4;

    void setupUi(QDialog *Dialog)
    {
    if (Dialog->objectName().isEmpty())
        Dialog->setObjectName(QString::fromUtf8("Dialog"));
    Dialog->resize(645, 213);
    buttonBox = new QDialogButtonBox(Dialog);
    buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
    buttonBox->setGeometry(QRect(270, 170, 341, 32));
    buttonBox->setOrientation(Qt::Horizontal);
    buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::NoButton|QDialogButtonBox::Ok);
    widget = new QWidget(Dialog);
    widget->setObjectName(QString::fromUtf8("widget"));
    widget->setGeometry(QRect(-2, 0, 651, 171));
    gridLayout = new QGridLayout(widget);
    gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
    gridLayout->setContentsMargins(5, 5, 5, 0);
    label_2 = new QLabel(widget);
    label_2->setObjectName(QString::fromUtf8("label_2"));
    label_2->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

    gridLayout->addWidget(label_2, 0, 0, 1, 1);

    importFileNameBox = new QLineEdit(widget);
    importFileNameBox->setObjectName(QString::fromUtf8("importFileNameBox"));

    gridLayout->addWidget(importFileNameBox, 0, 1, 1, 3);

    label_5 = new QLabel(widget);
    label_5->setObjectName(QString::fromUtf8("label_5"));
    label_5->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

    gridLayout->addWidget(label_5, 1, 0, 1, 1);

    proteinAcronymBox = new QLineEdit(widget);
    proteinAcronymBox->setObjectName(QString::fromUtf8("proteinAcronymBox"));
    proteinAcronymBox->setMaxLength(4);

    gridLayout->addWidget(proteinAcronymBox, 1, 1, 1, 1);

    spacerItem = new QSpacerItem(25, 22, QSizePolicy::Expanding, QSizePolicy::Minimum);

    gridLayout->addItem(spacerItem, 1, 2, 1, 1);

    spacerItem1 = new QSpacerItem(335, 22, QSizePolicy::Expanding, QSizePolicy::Minimum);

    gridLayout->addItem(spacerItem1, 1, 3, 1, 1);

    label = new QLabel(widget);
    label->setObjectName(QString::fromUtf8("label"));
    label->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

    gridLayout->addWidget(label, 2, 0, 1, 1);

    tiltAngleBox = new QSpinBox(widget);
    tiltAngleBox->setObjectName(QString::fromUtf8("tiltAngleBox"));
    tiltAngleBox->setMaximum(90);
    tiltAngleBox->setSingleStep(5);

    gridLayout->addWidget(tiltAngleBox, 2, 1, 1, 1);

    spacerItem2 = new QSpacerItem(380, 27, QSizePolicy::Expanding, QSizePolicy::Minimum);

    gridLayout->addItem(spacerItem2, 2, 2, 1, 2);

    label_3 = new QLabel(widget);
    label_3->setObjectName(QString::fromUtf8("label_3"));
    QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
    sizePolicy.setHorizontalStretch(0);
    sizePolicy.setVerticalStretch(0);
    sizePolicy.setHeightForWidth(label_3->sizePolicy().hasHeightForWidth());
    label_3->setSizePolicy(sizePolicy);
    label_3->setMinimumSize(QSize(0, 20));
    label_3->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

    gridLayout->addWidget(label_3, 3, 0, 1, 1);

    frameNumberBox = new QLineEdit(widget);
    frameNumberBox->setObjectName(QString::fromUtf8("frameNumberBox"));
    frameNumberBox->setMaxLength(6);

    gridLayout->addWidget(frameNumberBox, 3, 1, 1, 2);

    spacerItem3 = new QSpacerItem(335, 22, QSizePolicy::Expanding, QSizePolicy::Minimum);

    gridLayout->addItem(spacerItem3, 3, 3, 1, 1);

    label_4 = new QLabel(widget);
    label_4->setObjectName(QString::fromUtf8("label_4"));
    label_4->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

    gridLayout->addWidget(label_4, 4, 0, 1, 1);

    subImageBox = new QLineEdit(widget);
    subImageBox->setObjectName(QString::fromUtf8("subImageBox"));
    subImageBox->setMaxLength(2);

    gridLayout->addWidget(subImageBox, 4, 1, 1, 1);

    spacerItem4 = new QSpacerItem(380, 21, QSizePolicy::Expanding, QSizePolicy::Minimum);

    gridLayout->addItem(spacerItem4, 4, 2, 1, 2);

    QWidget::setTabOrder(importFileNameBox, proteinAcronymBox);
    QWidget::setTabOrder(proteinAcronymBox, tiltAngleBox);
    QWidget::setTabOrder(tiltAngleBox, frameNumberBox);
    QWidget::setTabOrder(frameNumberBox, subImageBox);
    QWidget::setTabOrder(subImageBox, buttonBox);

    retranslateUi(Dialog);
    QObject::connect(buttonBox, SIGNAL(accepted()), Dialog, SLOT(accept()));
    QObject::connect(buttonBox, SIGNAL(rejected()), Dialog, SLOT(reject()));

    QMetaObject::connectSlotsByName(Dialog);
    } // setupUi

    void retranslateUi(QDialog *Dialog)
    {
    Dialog->setWindowTitle(QApplication::translate("Dialog", "Import File", 0, QApplication::UnicodeUTF8));
    label_2->setText(QApplication::translate("Dialog", "Import File:", 0, QApplication::UnicodeUTF8));
    label_5->setText(QApplication::translate("Dialog", "Protein Acronym:", 0, QApplication::UnicodeUTF8));
    label->setText(QApplication::translate("Dialog", "Tilt Angle:", 0, QApplication::UnicodeUTF8));
    label_3->setText(QApplication::translate("Dialog", "Frame#:", 0, QApplication::UnicodeUTF8));
    label_4->setText(QApplication::translate("Dialog", "Sub-Image:", 0, QApplication::UnicodeUTF8));
    Q_UNUSED(Dialog);
    } // retranslateUi

};

namespace Ui {
    class Dialog: public Ui_Dialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_IMPORTBOX_H
