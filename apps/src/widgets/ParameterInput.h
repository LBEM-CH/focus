#ifndef CONFINPUT_H
#define CONFINPUT_H

#include <QWidget>
#include <QEvent>
#include <QWhatsThis>
#include <QWhatsThisClickedEvent>
#include <QGridLayout>
#include <QLabel>
#include <QList>
#include <QLineEdit>
#include <QButtonGroup>
#include <QRadioButton>
#include <QPushButton>
#include <QCheckBox>
#include <QComboBox>
#include <QSpacerItem>
#include <QPalette>
#include <QProcess>
#include <QIcon>
#include <QColor>
#include <float.h>

#include "GraphicalButton.h"
#include "ParameterElementData.h"

class ParameterInput : public QWidget {
    Q_OBJECT

public:
    ParameterInput(ParameterElementData *e, QWidget *parent = NULL);
    int userLevel();

public slots:
    void saveValue(const QString& value);
    void load();  
    void setReadOnlyState(int state);
    void show();

signals:
    void shown();
    void shouldLoadValue(const QString& value);

private:

    QWidget* setupDirWidget(bool isDir = false);
    QWidget* setupEditWidget();
    QWidget* setupBoolWidget();
    QWidget* setupDropDownWidget();

    ParameterElementData *element;

    QGridLayout* layout_;
    GraphicalButton* lockButton_;
    QWidget* inputWidget_;
};

#endif
