#ifndef ABSTRACT_TOOL_H
#include <QGraphicsScene>
#include <QActionGroup>
#include <QGraphicsItemGroup>
#include <QMenu>
#include <QHash>
#include <QList>
#include <QDebug>

class abstractTool : public QObject
{

  Q_OBJECT

  public slots:
  void addAction(QAction *action);
  void setEnabled(bool enabled = true);
  void setVisible(bool enabled = true);

	void addGraphicsItem(QGraphicsItem *item);

  private:
  QActionGroup *actionGroup;
  QList<QAction*> actions;
  QGraphicsItemGroup *internalOverlay;
  QHash<QString,bool> states;

  public:
  abstractTool(QObject *parent = NULL);
  ~abstractTool();
  QGraphicsItemGroup *overlay();

	void registerActions(QWidget *w, QMenu *menu = NULL);

};

#endif

