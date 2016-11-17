#include <latticeTool.h>

latticeTool::latticeTool(QObject *parent)
					  :AbstractTool(parent)
{
  QAction *action = new QAction(QString("View Lattice"),this);
  action->setShortcut(tr("L"));
  action->setCheckable(true);
  connect(action,SIGNAL(toggled(bool)),this,SLOT(setEnabled(bool)));
  addAction(action);

  for(int i=-8;i<=8;i++)
		for(int j=-8;j<=8;j++)
		{
			QGraphicsEllipseItem *item = new QGraphicsEllipseItem(100*i-5,500*j-5,10,10);
      item->setZValue(1.0);

			QPen pen(item->pen());
			pen.setColor(QColor(150,250,240));
			item->setPen(pen);
			addGraphicsItem(item);
		}
  setEnabled(false);
  setVisible(false);
}


