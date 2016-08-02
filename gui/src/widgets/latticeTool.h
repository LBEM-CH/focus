#ifndef _LATTICETOOL_H_
#define _LATTICETOOL_H_

#include <abstractTool.h>
#include <QGraphicsEllipseItem>

class latticeTool : public abstractTool
{
  Q_OBJECT

  private:
  QList<QGraphicsEllipseItem *> latticeNodes;

  public:
  latticeTool(QObject *parent = NULL);
  

};

#endif

