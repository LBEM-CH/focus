#ifndef GRAPHICALBUTTON_H
#define GRAPHICALBUTTON_H

#include <QCheckBox>
#include <QPainter>
#include <QIcon>

class GraphicalButton : public QAbstractButton {
    Q_OBJECT

public:
    GraphicalButton(QIcon icon, QWidget *parent = NULL);

public slots:
    void changeState(bool checked);
    void setCheckable(bool checkable);

signals:
    void stateChanged(int state);

protected:
    virtual void paintEvent(QPaintEvent *event);

private:
    QIcon icon_;
    void initialize();
};

#endif
