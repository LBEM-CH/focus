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
  uint32_t nx, ny;
  float imageMin, imageMax;
  int mode;
  uint32_t leftI, rightI;
  bool showPhase;
  QImage::Format format;

	float fastMagnitude(float a, float b);

  public:
  enum loadType {fft, real};
  loadThread(char *rawData, uchar *imageData, uint32_t nx, uint32_t ny, float min, float max, int mode, uint32_t leftI, uint32_t rightI, QImage::Format format, loadThread::loadType type, bool showPhase = false, QObject *parent = NULL);

  protected:
  void run();

  private:
    loadType type;

};

#endif
