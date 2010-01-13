/*
 *  mrcImage.cpp
 *  mrcLoad
 *
 *  Created by Bryant Gipson on 3/23/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "mrcImage.h"
using namespace std;

void mrcImage::initialize()
{
  fileHeader = NULL;
  data = NULL;
}

mrcImage::mrcImage()
{
  initialize();
  empty = 1;
}

mrcImage::mrcImage(char *fileName, int slice)
{
  //cout<<"Loading "<<fileName<<"."<<endl;
  initialize();
  empty = 1;
  if(loadImage(fileName,slice)) empty = 0;
  //cout<<fileName<<" loaded."<<endl;
}

mrcImage::mrcImage(mrcImage::mrcHeader *header, char *data, char *fileName)
{
  initialize();
  empty = 1;
  setHeader(header);
  if(saveImage(data,fileName)) empty = 0;
}
 
mrcImage::~mrcImage()
{
  if(fileHeader!=NULL) delete fileHeader;
  if(fileHeader!=NULL) delete data;
}

void mrcImage::swapData(void *data, int size)
{
  if(size == 1) return;
  for(int i=0;i<size/2;i++)
  {
    char temp = ((char*)data)[i];
    ((char*)data)[i] = ((char*)data)[size-i-1];
    ((char*)data)[size-i-1] = temp;
  }
}

void mrcImage::calculateSizes()
{
  if(fileHeader->mode == 0) cellSize = 1;                        // Char
  else if (fileHeader->mode == 1) cellSize = 2;                  // Short
  else if (fileHeader->mode == 2) cellSize = 4;                  // Float
  else if (fileHeader->mode == 3) cellSize = 2;                  // 1/2 Complex (2,2)
  else if (fileHeader->mode == 4) cellSize = 4;                  // 1/2 Complex (4,4);
 
  if(fileHeader->nz!=0) 
    imageSize = fileHeader->nx*fileHeader->ny*fileHeader->nz;
  else
    imageSize = fileHeader->nx*fileHeader->ny;
  if(fileHeader->mode == 3 || fileHeader->mode == 4) imageSize*=2;  
  dataSize = imageSize*cellSize;
}

void mrcImage::loadHeader(ifstream &mrcFile)
{
  fileHeader = new mrcHeader;
  mrcFile.seekg(0,ios::beg);
  mrcFile.read((char*)fileHeader,1024);

  int bitMask = fileHeader->mapc | fileHeader->mapr | fileHeader->maps;
  if(bitMask == 3 || bitMask == 0) byteSwap = false;
  else {cout<<"Image byte order conflicts with platform.\nAttempting load anyhow."<<endl; byteSwap = true;}
  
  if(byteSwap)
    for(int i=0;i<256;i++)
      swapData(&((int*)(fileHeader))[i]);

  calculateSizes();
}

void mrcImage::loadData(ifstream &mrcFile, int slice)
{
  data = new char[dataSize]; 
  mrcFile.seekg(1024,ios::beg);
  mrcFile.read(data,dataSize);

  if(byteSwap)
    for(int i=0;i<imageSize;i++)
      swapData(&data[i*cellSize],cellSize);      
}

int mrcImage::loadImage(char *fileName, int slice)
{
  ifstream mrcFile;
  mrcFile.open(fileName);
  if(!mrcFile.is_open()) return 0;
  loadHeader(mrcFile);
  loadData(mrcFile, slice);
  mrcFile.close();
  return 1;
}

void mrcImage::saveHeader(ofstream &mrcFile)
{
  mrcFile.seekp(0,ios::beg);
  mrcFile.write((char*)fileHeader,1024);
} 

void mrcImage::saveData(char *data, ofstream &mrcFile)
{
  mrcFile.seekp(1024,ios::beg);
  mrcFile.write(data,dataSize);
}

int mrcImage::saveImage(char *data, char *fileName)
{
  ofstream mrcFile;
  mrcFile.open(fileName);
  if(!mrcFile.is_open()) return 0;
  saveHeader(mrcFile);
  saveData(data,mrcFile);
  mrcFile.close();
  return 1;
}

void mrcImage::setHeader(mrcImage::mrcHeader *header)
{
  fileHeader = header;
  calculateSizes();
}

mrcImage::mrcHeader *mrcImage::headerFromData(unsigned int width, unsigned int height, unsigned int mode, char *data)
{
  unsigned int cellSize, dataSize, imageSize;

  mrcImage::mrcHeader *fileHeader = new mrcImage::mrcHeader;

  for(int i=0;i<256;i++)
    ((int*)fileHeader)[i]=0;

  fileHeader->amin = FLT_MAX;

  fileHeader->mode = mode;
  fileHeader->nx = width;
  fileHeader->ny = height;
  fileHeader->nz = 1;
  if(mode == 3 || mode == 4) 
  {
    fileHeader->a = (fileHeader->nx-1)*2;
    fileHeader->mx = (fileHeader->nx-1)*2;
  }
  else 
  { 
    fileHeader->a = fileHeader->nx;
    fileHeader->mx= fileHeader->nx;
  }
  fileHeader->b = fileHeader->ny;
  fileHeader->c = fileHeader->nz;

  if(fileHeader->mode == 0) cellSize = 1;                        // Char
  else if (fileHeader->mode == 1) cellSize = 2;                  // Short
  else if (fileHeader->mode == 2) cellSize = 4;                  // Float
  else if (fileHeader->mode == 3) cellSize = 2;                  // 1/2 Complex (2,2)
  else if (fileHeader->mode == 4) cellSize = 4;                  // 1/2 Complex (4,4);

  imageSize = fileHeader->nx*fileHeader->ny;
  dataSize = imageSize*cellSize;

  fileHeader->mapc = 1; fileHeader->mapr = 2; fileHeader->maps = 3;

  float amp, ampr, ampi;
  
  for(int i=0;i<imageSize;i++)
  {
    if(mode == 3 || mode == 4) 
    {
      if(mode == 3) {ampr = ((short*)data)[2*i]; ampi = ((short*)data)[2*i + 1];}
      if(mode == 4) {ampr = ((float*)data)[2*i]; ampi = ((float*)data)[2*i + 1];}

      amp = sqrt(ampr*ampr + ampi*ampi);
    }
    else
    {
      if(mode = 0) amp = ((unsigned char*)data)[i];
      if(mode = 1) amp = ((short*)data)[i];
      if(mode = 2) amp = ((float*)data)[i];
    }
    if(amp>fileHeader->amax) fileHeader->amax = amp;
    if(amp<fileHeader->amin) fileHeader->amin = amp;
    fileHeader->amean += amp/(float)imageSize;
    fileHeader->rms+= amp*amp/(float)imageSize;
  }
  fileHeader->rms-= fileHeader->amean*fileHeader->amean;
  fileHeader->rms = sqrt(fileHeader->rms);
  fileHeader->my = fileHeader->ny;
  fileHeader->mz = fileHeader->ny;
  fileHeader->b = fileHeader->ny;
  fileHeader->c = fileHeader->ny;

  fileHeader->alpha = 90.0;
  fileHeader->beta = 90.0;
  fileHeader->gamma = 90.0;

  return fileHeader;
}

char *mrcImage::complexFromReal(unsigned int width, unsigned int height, unsigned int mode, char *data)
{
  if(mode == 3 || mode == 4) return NULL;

  unsigned int imageSize = (width/2+1)*height;
 
  char *complexData = new char[imageSize*2*sizeof(float)];
  for(int i=0;i<width/2+1;i++)  
    for(int j=0;j<height;j++)
    {
      if(j<height-1)
      {
        if(mode == 0) ((float*)complexData)[(j*(width/2+1)+i)*2] = (float)data[(j)*(width)+i+width/2];
        if(mode == 1) ((float*)complexData)[(j*(width/2+1)+i)*2] = (float)((short*)data)[(j)*(width)+i+width/2];
        if(mode == 2) ((float*)complexData)[(j*(width/2+1)+i)*2] = ((float*)data)[(j)*(width)+i+width/2];
        ((float*)complexData)[(j*(width/2+1)+i)*2 + 1] = 0.0;
      }
      else
        ((float*)complexData)[(j*(width/2+1)+i)*2]=0.0;

//      ((float*)complexData)[((j*(width/2+1)+i)*2+1)]=0.0;
    } 
  return complexData;
}

char *mrcImage::formatComplex(unsigned int width, unsigned int height, fftwf_complex *data)
{
  unsigned int imageSize = (width/2+1)*height;
 
  char *complexData = new char[imageSize*2*sizeof(float)];
  for(int i=0;i<width/2+1;i++)  
    for(int j=0;j<height;j++)
    {
      if(j<height-1)
      {
        ((float*)complexData)[(j*(width/2+1)+i)*2] = (float)data[(j)*(width)+i+width/2][0];
        ((float*)complexData)[(j*(width/2+1)+i)*2+1] = (float)data[(j)*(width)+i+width/2][1];
      }
      else
        ((float*)complexData)[(j*(width/2+1)+i)*2]=0.0;

//      ((float*)complexData)[((j*(width/2+1)+i)*2+1)]=0.0;
    } 
  return complexData;
}

bool mrcImage::isEmpty()
{
  return empty;
}

char *mrcImage::rawData()
{
  return data;
}

mrcImage::mrcHeader *mrcImage::header()
{
  return fileHeader;
}

unsigned int mrcImage::width()
{
  return fileHeader->nx;
}

unsigned int mrcImage::height()
{
  return fileHeader->ny;
}

unsigned int mrcImage::slices()
{
  return fileHeader->nz;
}

unsigned int mrcImage::mode()
{
  return fileHeader->mode;
}

float mrcImage::min()
{
  return fileHeader->amin;
}

float mrcImage::max()
{
  return fileHeader->amax;
}

float mrcImage::mean()
{
  return fileHeader->amean;
}

float mrcImage::intensity(int x, int y)
{
  int mode = fileHeader->mode;
  int w = width(), h = height();
  if(x<0 || y<0 || x>w-1 || y>h-1) return 0.0;
  if(mode == 0) return (float)(((unsigned char*)data)[x+y*w]);
  if(mode == 1) return (float)(((short*)data)[x+y*w]);
  if(mode == 2) return (float)(((float*)data)[x+y*w]);
  if(mode == 3) return sqrt((float)(((short*)data)[2*(x+y*w)])*(float)(((short*)data)[2*(x+y*w)])+(float)(((short*)data)[2*(x+y*w)+1])*(float)(((short*)data)[2*(x+y*w)+1]));
  if(mode == 4) return sqrt((float)(((float*)data)[2*(x+y*w)])*(float)(((float*)data)[2*(x+y*w)])+(float)(((float*)data)[2*(x+y*w)+1])*(float)(((float*)data)[2*(x+y*w)+1]));
}

