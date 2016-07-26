#include <QtWidgets>
#include "noScrollComboBox.h"

NoScrollComboBox::NoScrollComboBox(QWidget *parent) : QComboBox(parent)
{
}

void NoScrollComboBox::focusInEvent(QFocusEvent *e)
{
    setFocusPolicy(Qt::StrongFocus);
}

void NoScrollComboBox::focusOutEvent(QFocusEvent *e)
{
    setFocusPolicy(Qt::StrongFocus);
}
