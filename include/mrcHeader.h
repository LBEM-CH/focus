/*
 *  mrcHeader.h
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/26/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

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
  uint32_t mode();
  uint32_t nx();
  uint32_t ny();
  uint32_t nz();
  uint32_t mx();
  uint32_t my();
  uint32_t mz();
  float cellA();
  float cellB();
  float cellC();

  void setNX(uint32_t var);
  void setNY(uint32_t var);	
  void setNZ(uint32_t var);	
  void setMode(uint32_t mode);	
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
