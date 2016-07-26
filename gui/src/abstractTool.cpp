#include <abstractTool.h>

abstractTool::abstractTool(QObject *parent)
						 :QObject(parent)
{
  states["enabled"] = false;
  states["visible"] = false;
  internalOverlay = new QGraphicsItemGroup;
}

abstractTool::~abstractTool()
{
}

void abstractTool::addAction(QAction *action)
{
  //actionGroup->addAction(action);
  actions<<action;
}

void abstractTool::setEnabled(bool enabled)
{
  states["enabled"] = enabled;
  internalOverlay->setEnabled(enabled);
  setVisible(enabled); 
}

void abstractTool::setVisible(bool enabled)
{
  states["visible"] = enabled;
  internalOverlay->setVisible(enabled);
  qDebug()<<"This event has happened: "<<enabled; 
}

void abstractTool::addGraphicsItem(QGraphicsItem *item)
{
  internalOverlay->addToGroup(item);
}

QGraphicsItemGroup *abstractTool::overlay()
{
  return internalOverlay;
}

void abstractTool::registerActions(QWidget *w, QMenu *menu)
{
  foreach(QAction *action, actions)
  {
		w->addAction(action);
    if(menu != NULL) menu->addAction(action);
  }
}


