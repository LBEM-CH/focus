#include <QtGui>
#include "noscrollcombobox.h"

NoScrollComboBox::NoScrollComboBox(QWidget *parent) : QComboBox(parent)
{
}

void NoScrollComboBox::focusInEvent(QFocusEvent *e)
{
    setFocusPolicy(Qt::WheelFocus);
}

void NoScrollComboBox::focusOutEvent(QFocusEvent *e)
{
    setFocusPolicy(Qt::StrongFocus);
}
