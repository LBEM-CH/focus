#include <AbstractTool.h>

AbstractTool::AbstractTool(QObject *parent)
						 :QObject(parent)
{
  states["enabled"] = false;
  states["visible"] = false;
  internalOverlay = new QGraphicsItemGroup;
}

AbstractTool::~AbstractTool()
{
}

void AbstractTool::addAction(QAction *action)
{
  //actionGroup->addAction(action);
  actions<<action;
}

void AbstractTool::setEnabled(bool enabled)
{
  states["enabled"] = enabled;
  internalOverlay->setEnabled(enabled);
  setVisible(enabled); 
}

void AbstractTool::setVisible(bool enabled)
{
  states["visible"] = enabled;
  internalOverlay->setVisible(enabled);
  qDebug()<<"This event has happened: "<<enabled; 
}

void AbstractTool::addGraphicsItem(QGraphicsItem *item)
{
  internalOverlay->addToGroup(item);
}

QGraphicsItemGroup *AbstractTool::overlay()
{
  return internalOverlay;
}

void AbstractTool::registerActions(QWidget *w, QMenu *menu)
{
  foreach(QAction *action, actions)
  {
		w->addAction(action);
    if(menu != NULL) menu->addAction(action);
  }
}


