#include <QDebug>

#include "GraphicalButton.h"

void GraphicalButton::initialize() {
    if (!icon_.isNull()) {
        setFixedSize(icon_.actualSize(size()*2));
        connect(this, SIGNAL(toggled(bool)), this, SLOT(changeState(bool)));
    } else {
        qDebug() << "Invalid icon encountered in graphical button.";
    }
}

GraphicalButton::GraphicalButton(QIcon icon, QWidget *parent)
: QAbstractButton(parent) {
    icon_ = icon;
    initialize();
}

void GraphicalButton::paintEvent(QPaintEvent *event) {
    QWidget::paintEvent(event);
    QPainter imageBase(this);
    QSize iSize = size();
    if (!icon_.isNull()) {
        if (!isChecked()) {
            if (!isDown()) {
                imageBase.drawPixmap(0, 0, icon_.pixmap(iSize));
            } else {
                imageBase.drawPixmap(0, 0, icon_.pixmap(iSize, QIcon::Active));
            }
        } else {
            if (!isDown()) imageBase.drawPixmap(0, 0, icon_.pixmap(iSize, QIcon::Normal, QIcon::On));
            else imageBase.drawPixmap(0, 0, icon_.pixmap(iSize, QIcon::Active, QIcon::On));
        }
    }
}

void GraphicalButton::changeState(bool checked) {
    if (checked) emit stateChanged(Qt::Checked);
    else emit stateChanged(Qt::Unchecked);
}

void GraphicalButton::setCheckable(bool checkable) {
    QAbstractButton::setCheckable(checkable);
}

