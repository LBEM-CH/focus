#ifndef NOSCROLLCOMBOBOX_H
#define NOSCROLLCOMBOBOX_H

#include <QComboBox>

class NoScrollComboBox : public QComboBox
{
    Q_OBJECT
public:
    NoScrollComboBox(QWidget *parent = 0);

protected:
    void focusInEvent(QFocusEvent *e);
    void focusOutEvent(QFocusEvent *e);

signals:

public slots:

};



#endif // NOSCROLLCOMBOBOX_H
