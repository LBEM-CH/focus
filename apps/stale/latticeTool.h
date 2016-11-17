#ifndef _LATTICETOOL_H_
#define _LATTICETOOL_H_

#include <AbstractTool.h>
#include <QGraphicsEllipseItem>

class latticeTool : public AbstractTool
{
  Q_OBJECT

  private:
  QList<QGraphicsEllipseItem *> latticeNodes;

  public:
  latticeTool(QObject *parent = NULL);
  

};

#endif

