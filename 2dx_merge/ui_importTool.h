/********************************************************************************
** Form generated from reading ui file 'importTool.ui'
**
** Created: Thu Oct 16 09:53:51 2008
**      by: Qt User Interface Compiler version 4.4.0
**
** WARNING! All changes made in this file will be lost when recompiling ui file!
********************************************************************************/

#ifndef UI_IMPORTTOOL_H
#define UI_IMPORTTOOL_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QDialog>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QLabel>
#include <QtGui/QTableWidget>
#include <QtGui/QToolButton>

QT_BEGIN_NAMESPACE

class Ui_ImportTool
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QLabel *label_2;
    QComboBox *patternInput;
    QToolButton *addRegExpButton;
    QToolButton *removRegExpButton;
    QHBoxLayout *hboxLayout1;
    QLabel *label;
    QLabel *patternMessage;
    QHBoxLayout *hboxLayout2;
    QTableWidget *importView;
    QTableWidget *parsedView;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *ImportTool)
    {
    if (ImportTool->objectName().isEmpty())
        ImportTool->setObjectName(QString::fromUtf8("ImportTool"));
    ImportTool->resize(644, 517);
    gridLayout = new QGridLayout(ImportTool);
    gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
    gridLayout->setVerticalSpacing(5);
    gridLayout->setContentsMargins(5, 5, 5, 10);
    hboxLayout = new QHBoxLayout();
#ifndef Q_OS_MAC
    hboxLayout->setSpacing(-1);
#endif
    hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
    hboxLayout->setContentsMargins(10, -1, 10, -1);
    label_2 = new QLabel(ImportTool);
    label_2->setObjectName(QString::fromUtf8("label_2"));
    QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    sizePolicy.setHorizontalStretch(0);
    sizePolicy.setVerticalStretch(0);
    sizePolicy.setHeightForWidth(label_2->sizePolicy().hasHeightForWidth());
    label_2->setSizePolicy(sizePolicy);

    hboxLayout->addWidget(label_2);

    patternInput = new QComboBox(ImportTool);
    patternInput->setObjectName(QString::fromUtf8("patternInput"));
    QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Fixed);
    sizePolicy1.setHorizontalStretch(0);
    sizePolicy1.setVerticalStretch(0);
    sizePolicy1.setHeightForWidth(patternInput->sizePolicy().hasHeightForWidth());
    patternInput->setSizePolicy(sizePolicy1);
    patternInput->setEditable(true);

    hboxLayout->addWidget(patternInput);

    addRegExpButton = new QToolButton(ImportTool);
    addRegExpButton->setObjectName(QString::fromUtf8("addRegExpButton"));

    hboxLayout->addWidget(addRegExpButton);

    removRegExpButton = new QToolButton(ImportTool);
    removRegExpButton->setObjectName(QString::fromUtf8("removRegExpButton"));

    hboxLayout->addWidget(removRegExpButton);


    gridLayout->addLayout(hboxLayout, 0, 0, 1, 1);

    hboxLayout1 = new QHBoxLayout();
    hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
    hboxLayout1->setContentsMargins(10, -1, 10, -1);
    label = new QLabel(ImportTool);
    label->setObjectName(QString::fromUtf8("label"));
    QSizePolicy sizePolicy2(QSizePolicy::Maximum, QSizePolicy::Preferred);
    sizePolicy2.setHorizontalStretch(0);
    sizePolicy2.setVerticalStretch(0);
    sizePolicy2.setHeightForWidth(label->sizePolicy().hasHeightForWidth());
    label->setSizePolicy(sizePolicy2);

    hboxLayout1->addWidget(label);

    patternMessage = new QLabel(ImportTool);
    patternMessage->setObjectName(QString::fromUtf8("patternMessage"));

    hboxLayout1->addWidget(patternMessage);


    gridLayout->addLayout(hboxLayout1, 1, 0, 1, 1);

    hboxLayout2 = new QHBoxLayout();
    hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
    importView = new QTableWidget(ImportTool);
    importView->setObjectName(QString::fromUtf8("importView"));

    hboxLayout2->addWidget(importView);

    parsedView = new QTableWidget(ImportTool);
    if (parsedView->columnCount() < 4)
        parsedView->setColumnCount(4);
    QTableWidgetItem *__colItem = new QTableWidgetItem();
    parsedView->setHorizontalHeaderItem(0, __colItem);
    QTableWidgetItem *__colItem1 = new QTableWidgetItem();
    parsedView->setHorizontalHeaderItem(1, __colItem1);
    QTableWidgetItem *__colItem2 = new QTableWidgetItem();
    parsedView->setHorizontalHeaderItem(2, __colItem2);
    QTableWidgetItem *__colItem3 = new QTableWidgetItem();
    parsedView->setHorizontalHeaderItem(3, __colItem3);
    if (parsedView->rowCount() < 1)
        parsedView->setRowCount(1);
    QTableWidgetItem *__rowItem = new QTableWidgetItem();
    parsedView->setVerticalHeaderItem(0, __rowItem);
    parsedView->setObjectName(QString::fromUtf8("parsedView"));

    hboxLayout2->addWidget(parsedView);


    gridLayout->addLayout(hboxLayout2, 2, 0, 1, 1);

    buttonBox = new QDialogButtonBox(ImportTool);
    buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
    buttonBox->setOrientation(Qt::Horizontal);
    buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::NoButton|QDialogButtonBox::Ok);

    gridLayout->addWidget(buttonBox, 3, 0, 1, 1);


    retranslateUi(ImportTool);
    QObject::connect(buttonBox, SIGNAL(accepted()), ImportTool, SLOT(accept()));
    QObject::connect(buttonBox, SIGNAL(rejected()), ImportTool, SLOT(reject()));

    patternInput->setCurrentIndex(-1);


    QMetaObject::connectSlotsByName(ImportTool);
    } // setupUi

    void retranslateUi(QDialog *ImportTool)
    {
    ImportTool->setWindowTitle(QApplication::translate("ImportTool", "Import Images", 0, QApplication::UnicodeUTF8));
    label_2->setText(QApplication::translate("ImportTool", "Translate:", 0, QApplication::UnicodeUTF8));

#ifndef QT_NO_TOOLTIP
    patternInput->setToolTip(QString());
#endif // QT_NO_TOOLTIP

    addRegExpButton->setText(QApplication::translate("ImportTool", "+", 0, QApplication::UnicodeUTF8));
    removRegExpButton->setText(QApplication::translate("ImportTool", "-", 0, QApplication::UnicodeUTF8));
    label->setText(QApplication::translate("ImportTool", "Pattern Error:", 0, QApplication::UnicodeUTF8));
    patternMessage->setText(QString());
    parsedView->horizontalHeaderItem(0)->setText(QApplication::translate("ImportTool", "Name", 0, QApplication::UnicodeUTF8));
    parsedView->horizontalHeaderItem(1)->setText(QApplication::translate("ImportTool", "Angle", 0, QApplication::UnicodeUTF8));
    parsedView->horizontalHeaderItem(2)->setText(QApplication::translate("ImportTool", "Number", 0, QApplication::UnicodeUTF8));
    parsedView->horizontalHeaderItem(3)->setText(QApplication::translate("ImportTool", "Sub-Image", 0, QApplication::UnicodeUTF8));
    parsedView->verticalHeaderItem(0)->setText(QApplication::translate("ImportTool", "d", 0, QApplication::UnicodeUTF8));
    Q_UNUSED(ImportTool);
    } // retranslateUi

};

namespace Ui {
    class ImportTool: public Ui_ImportTool {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_IMPORTTOOL_H
