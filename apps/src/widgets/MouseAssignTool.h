#ifndef MOUSEASSIGNTOOL_H
#define MOUSEASSIGNTOOL_H

#include <QLabel>
#include <QGridLayout>

class MouseAssignTool : public QWidget {
    Q_OBJECT

    QList<QLabel *> buttons;

public:
    MouseAssignTool(QWidget *parent = NULL);
    void assignButton(int button, const QString &text);

};

#endif
