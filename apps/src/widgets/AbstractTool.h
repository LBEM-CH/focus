#ifndef ABSTRACT_TOOL_H
#define ABSTRACT_TOOL_H

#include <QGraphicsScene>
#include <QActionGroup>
#include <QGraphicsItemGroup>
#include <QMenu>
#include <QHash>
#include <QList>
#include <QDebug>

class AbstractTool : public QObject {
    Q_OBJECT

public:
    AbstractTool(QObject *parent = NULL);
    ~AbstractTool();
    QGraphicsItemGroup *overlay();

    void registerActions(QWidget *w, QMenu *menu = NULL);

public slots:
    void addAction(QAction *action);
    void setEnabled(bool enabled = true);
    void setVisible(bool enabled = true);

    void addGraphicsItem(QGraphicsItem *item);

private:
    QActionGroup *actionGroup;
    QList<QAction*> actions;
    QGraphicsItemGroup *internalOverlay;
    QHash<QString, bool> states;

};

#endif

