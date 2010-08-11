#ifndef MRCIMAGE_H
#define MRCIMAGE_H

#include <iostream>
#include <complex>
#include <fstream>
#include <float.h>
#include <fftw3.h>

class mrcImage;

class mrcImage
{
  public:
  struct mrcHeader
  {
    unsigned int nx,ny,nz;              //0,4,8 Image Size
    unsigned int mode;                  //12 Image Mode: 0 - uchar, 1 - short, 2 - float, 3 - complex short (2,2), 4 - complex float (4,4)
    int nxStart,nyStart,nzStart;        //16, 20, 24 Unit cell Offset
    int mx,my,mz;                       //28, 32, 36 Unit cell size in voxels;
    float a,b,c;                        //40, 44, 48 Cell dimensions in angstrom
    float alpha,beta,gamma;             //52, 56, 60 Cell angles in degrees
    int mapc, mapr, maps;               //64, 68, 72 Column axis, Row axis, Section axis
    float amin, amax, amean;            //76, 80, 84 Minimum, Maximum, Average density values
    int ispg;                           //88 Space group number
    int nsymbt;                         //92 Bytes used for symmetry operations table
    float extra[25];                    //96-192 User defined information
    float xOrigin,yOrigin,zOrigin;      //196,200,204 Phase origin in pixels
    char Signature[4];                  //208
    int MachineStamp;                   //216
    float rms;                          //220 RMS deviation of map from mean density
    int nlabl;                          //224 Number of labels present
    char labels[10][80];                //228 10 80-Character lables
  };

  private:
  mrcHeader *fileHeader;
  char *data;
  unsigned int cellSize;
  unsigned int dataSize;
  unsigned int imageSize;
  bool empty;
  bool byteSwap;
  
  void initialize();
  void calculateSizes();
  void swapData(void *data, int size=4);
  void loadHeader(std::ifstream &mrcFile);
  void loadData(std::ifstream &mrcFile, int slice);
  void saveHeader(std::ofstream &mrcFile);
  void saveData(char *data, std::ofstream &mrcFile);
  
  public:
  mrcImage();
  mrcImage(char *fileName, int slice=0);
  mrcImage(mrcImage::mrcHeader *header, char *data, char *fileName);
  ~mrcImage();
  bool isEmpty();
  int loadImage(char *fileName, int slice=0);
  int saveImage(char *data, char *fileName);
 
  void setHeader(mrcImage::mrcHeader *header);
  static mrcImage::mrcHeader *headerFromData(unsigned int width, unsigned int height, unsigned int mode, char *data);
  static char *complexFromReal(unsigned int width, unsigned int height, unsigned int mode, char *data);
  static char *formatComplex(unsigned int width, unsigned int height, fftwf_complex *data);
 
  char *rawData();
  mrcImage::mrcHeader *header();
  unsigned int width();
  unsigned int height();
  unsigned int slices();
  unsigned int mode();
  float min();
  float max();
  float mean();

  float intensity(int x, int y);
};

#endif
