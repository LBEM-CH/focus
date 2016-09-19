#ifndef BLOCKCONTAINER_H
#define BLOCKCONTAINER_H

#include <QFrame>
#include <QString>
#include <QPalette>
#include <QColor>
#include <QLinearGradient>
#include <QPoint>
#include <QFont>
#include <QVBoxLayout>
#include <QGridLayout>
#include <QLabel>
#include <QSpacerItem>

class BlockContainer : public QFrame {
    Q_OBJECT

public:
    BlockContainer(const QString& title, QWidget* mainWidget, QWidget* headerWidget = 0, QWidget* parent = 0);

public slots:
    void setHeaderTitle(const QString& titleLabel);

signals:
    void doubleClicked();

protected:
    void mouseDoubleClickEvent(QMouseEvent *event);

private:
    QWidget* setupHeader(const QString& title, QWidget* headerWidget);

    QLabel* headerTitle;

};

#endif /* BLOCKCONTAINER_H */

