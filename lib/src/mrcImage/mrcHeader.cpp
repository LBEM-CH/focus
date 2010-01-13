/*
 *  mrcHeader.cpp
 *  2dx_image
 *
 *  Created by Bryant Gipson on 5/26/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "mrcHeader.h"
#include <iostream>
using namespace std;

void mrcHeader::byteSwap(void *data, int size)
{
  if(size == 1) return;
  for(int i=0;i<size/2;i++)
  {
    char temp = ((char*)data)[i];
    ((char*)data)[i] = ((char*)data)[size-i-1];
    ((char*)data)[size-i-1] = temp;
  }
}

bool mrcHeader::loadHeader()
{
  QFile imageFile(fileName);
  if(!imageFile.open(QIODevice::ReadOnly)) return false;
  imageFile.seek(headerOffset);
  imageFile.read((char*)header,1024);
  imageFile.close();

  int bitMask = header[16] | header[17] | header[18];
  if(bitMask == 3 || bitMask == 0) swapByteOrder = false;
  else swapByteOrder = true;

  if(swapByteOrder)
  for(int i=0;i<256;i++)
    byteSwap(&((int*)(header))[i]);

  return true;
}

bool mrcHeader::saveHeader(qint64 offset)
{
  QFile imageFile(fileName);
  if(!imageFile.open(QIODevice::Append)) return false;
  imageFile.seek(offset);

  imageFile.write((char*)header,1024);
  imageFile.close();
  return true;
}

void mrcHeader::initialize()
{
  variables <<"nx"<<"ny"<<"nz"<<"mode"<<"nxstart"<<"nystart"<<"nzstart"<<"mx"<<"my"<<"mz"<<"cella"<<"cellb"<<"cellc";
  variables <<"alpha"<<"beta"<<"gamma"<<"mapc"<<"mapr"<<"maps"<<"amin"<<"amax"<<"amean"<<"ispg"<<"nsymbt";
  for(int i=25;i<=49;i++)
    variables << "extra" + QString::number(i-25);
  variables <<"originX"<<"originY"<<"originZ"<<"map"<<"machst"<<"rms";
}

int* mrcHeader::rawData()
{
  return header;
}

int* mrcHeader::get(QString var)
{
  return &header[variables.indexOf(var.trimmed().toLower())];
}

void mrcHeader::set(QString var, void *value)
{
  header[variables.indexOf(var.trimmed().toLower())] = *((int*)value);
}

qint64 mrcHeader::dataOffset()
{
  return headerOffset+1024;
}

uint32_t mrcHeader::mode()
{
  uint32_t value = *(uint32_t*)(get("mode"));
  return value;
}

uint32_t mrcHeader::nx()
{
  uint32_t value = *(uint32_t*)(get("nx"));
  return value;
}

uint32_t mrcHeader::ny()
{
  uint32_t value = *(uint32_t*)(get("ny"));
  return value;
}

uint32_t mrcHeader::nz()
{
  uint32_t value = *(uint32_t*)(get("nz"));
  if(value == 0) return 1;
  return value;
}

uint32_t mrcHeader::mx()
{
  uint32_t value = *(uint32_t*)(get("mx"));
  return value;
}

uint32_t mrcHeader::my()
{
  uint32_t value = *(uint32_t*)(get("my"));
  return value;
}

uint32_t mrcHeader::mz()
{
  uint32_t value = *(uint32_t*)(get("mz"));
  return value;
}

float mrcHeader::cellA()
{
  float value = *(float*)(get("cella"));
  return value;
}

float mrcHeader::cellB()
{
  float value = *(float*)(get("cellb"));
  return value;
}

float mrcHeader::cellC()
{
  float value = *(float*)(get("cellc"));
  return value;
}

void mrcHeader::setNX(uint32_t var)
{
  header[variables.indexOf("nx")] = var;
}

void mrcHeader::setNY(uint32_t var)
{
  header[variables.indexOf("ny")] = var;
}

void mrcHeader::setNZ(uint32_t var)
{
  header[variables.indexOf("nz")] = var;
}

void mrcHeader::setMode(uint32_t var)
{
  header[variables.indexOf("mode")] = var;
}

float mrcHeader::max()
{
  float value = *(float*)(get("amax"));
  return value;
}

float mrcHeader::min()
{
  float value = *(float*)(get("amin"));
  return value;
}

float mrcHeader::mean()
{
  return *(float*)(get("amean"));
}

float mrcHeader::rms()
{
  return *(float*)(get("rms"));
}

void mrcHeader::setMax(float var)
{
  header[variables.indexOf("amax")] = *((int*)&var);
}

void mrcHeader::setMin(float var)
{
  header[variables.indexOf("amin")] = *((int*)&var);
}

bool mrcHeader::isFFT()
{
  if(mode()==3||mode()==4) return true;
  return false;
}

mrcHeader::mrcHeader(const QString &mrcFile, qint64 offset)
{
  fileName = mrcFile;
  headerOffset = offset;
  initialize();
  fileValid=loadHeader();
  if(!fileValid) cerr<<"MRC load failure."<<endl;
}

bool mrcHeader::swapEndian()
{
  return swapByteOrder;
}

bool mrcHeader::isValid()
{
  return fileValid;
}

