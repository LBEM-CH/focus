#ifndef LARGEMRC_H
#define LARGEMRC_H

#include <QObject>
#include <QPixmapCache>
#include <QMutex>
#include <QDebug>
#include <mrcHeader.h>
#include <QSet>
#include <math.h>

class largeMRC : public QObject
{
  Q_OBJECT

  private:

  char *data;
  mrcHeader *header;
  QString fileName;

  quint32 nx, ny, nz;
  quint64 cellSize;
  quint32 mode;
  float max, min;

  bool isComplex;

  quint32 w;

  QMutex mutex;

  QSet<int> loaded;
  
  bool fileValid;

  void initialize();
  bool load(quint32 yIndex);

  float amp(quint32 x, quint32 y);

  float fastMagnitude(float a, float b);

  public:
  largeMRC(const QString &fileName, QObject *parent);
  ~largeMRC();
  QPixmap pixmap(int x, int y);

	int pixmapWidth(int x, int y);
	int pixmapHeight(int x, int y);

  quint32 width();
  quint32 height();
  quint32 slice();

  bool isFFT();
};

#endif
