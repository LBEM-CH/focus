#ifndef THREADEDLOAD_H
#define THREADEDLOAD_H

#include <QThread>
#include <QImage>
#include <QColor>
#include <math.h>

#ifndef PI
#define PI 3.14159265
#endif 

class loadThread : public QThread
{

  private: 

  char *rawData;
  uchar *imageData;
  quint32 nx, ny;
  float imageMin, imageMax;
  int mode;
  quint32 leftI, rightI;
  bool showPhase;
  QImage::Format format;

	float fastMagnitude(float a, float b);

  public:
  enum loadType {fft, real};
  loadThread(char *rawData, uchar *imageData, quint32 nx, quint32 ny, float min, float max, int mode, quint32 leftI, quint32 rightI, QImage::Format format, loadThread::loadType type, bool showPhase = false, QObject *parent = NULL);

  protected:
  void run();

  private:
    loadType type;

};

#endif
