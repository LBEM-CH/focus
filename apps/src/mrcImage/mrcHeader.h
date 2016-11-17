#ifndef MRCHEADER_H
#define MRCHEADER_H

#include <QFile>

class mrcHeader
{
  private:
  QString fileName;
  int header[256];
  qint64 headerOffset;

  bool swapByteOrder;
  bool fileValid;

  QList<QString> variables;
	
  void initialize();
  bool loadHeader();
  void byteSwap(void *data, int size=4);
	
  public:
  mrcHeader(const QString &mrcFile, qint64 offset=0);
  bool saveHeader(qint64 offset);

  int* rawData();
  int* get(QString var);
  void set(QString var, void *value);
  qint64 dataOffset();
  quint32 mode();
  quint32 nx();
  quint32 ny();
  quint32 nz();
  quint32 mx();
  quint32 my();
  quint32 mz();
  float cellA();
  float cellB();
  float cellC();

  void setNX(quint32 var);
  void setNY(quint32 var);	
  void setNZ(quint32 var);	
  void setMode(quint32 mode);	
  float max();	
  float min();
  float mean();
  float rms();
  void setMax(float var);
  void setMin(float var);
  bool isValid();
  bool isFFT();
  bool swapEndian();
};

#endif
