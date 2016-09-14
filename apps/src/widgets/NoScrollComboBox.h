#ifndef COMBO_INPUT_WIDGET_HPP
#define COMBO_INPUT_WIDGET_HPP

#include <QComboBox>
#include <QWheelEvent>

class NoScrollComboBox : public QComboBox {

    Q_OBJECT

public:
    NoScrollComboBox(QWidget* parent = NULL)
    : QComboBox(parent) {
        setFrame(false);
    }

public slots:

    void wheelEvent(QWheelEvent *e) {
        if (hasFocus())
            QComboBox::wheelEvent(e);
    }


};

#endif /* COMBO_INPUT_WIDGET_HPP */