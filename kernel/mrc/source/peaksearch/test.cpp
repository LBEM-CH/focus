#include <iostream>
#include "mrcImage.h"
using namespace std;

int main()
{
  float *data = new float[500*500];
  ifstream f;
  f.open("./average.dat");
  for(int i=0;i<500*500;i++)
  {
    f>>data[i];  
  }  
  f.close();
  char *complexData = mrcImage::complexFromReal(500,500,2,(char*)data);
  cout<<"Data converted"<<endl;
  mrcImage::mrcHeader *header = mrcImage::headerFromData(251,500,4,complexData);
  cout<<"Header Generated"<<endl;
  mrcImage(header,complexData,"average.mrc");
  cout<<"mrc file written"<<endl;
  return 0; 
}
