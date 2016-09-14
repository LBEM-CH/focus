#ifndef YESNO_WIDGET_HPP
#define YESNO_WIDGET_HPP

#include <QWidget>
#include <QRadioButton>
#include <QLabel>
#include <QButtonGroup>
#include <QHBoxLayout>

class YesNoWidget : public QWidget {

    Q_OBJECT

public:

    YesNoWidget(QWidget* parent = NULL)
    : QWidget(parent) {
        QButtonGroup* buttonGroup = new QButtonGroup(this);
        yes = new QRadioButton(this);
        yesLabel = new QLabel("Yes");
        no = new QRadioButton(this);
        noLabel = new QLabel("No");
        buttonGroup->addButton(yes);
        buttonGroup->addButton(no);
        buttonGroup->setExclusive(true);
        connect(buttonGroup, SIGNAL(buttonClicked(int)), this, SLOT(updateValues()));


        QHBoxLayout* layout = new QHBoxLayout(this);
        layout->setMargin(0);
        layout->addWidget(yes, 0);
        layout->addWidget(yesLabel);
        layout->addWidget(no, 0);
        layout->addWidget(noLabel);

        setLayout(layout);
    }

    bool value() {
        if (yes->isChecked()) return true;
        else return false;
    }


public slots:

    void updateValues() {
        bool val = false;
        if (yes->isChecked()) val = true;
        emit valueChanged(val);
    }

    void setValue(bool value) {
        if (value) yes->setChecked(true);
        else no->setChecked(true);
    }

signals:
    void valueChanged(bool value);

private:
    QRadioButton *yes, *no;
    QLabel *yesLabel, *noLabel;
};

#endif /* YESNO_WIDGET_HPP */

