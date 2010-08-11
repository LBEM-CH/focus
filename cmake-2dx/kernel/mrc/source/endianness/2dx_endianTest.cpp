#include <iostream>
#include <fstream>
#include <mrcImage.h>
#include "endianOperations.h"
using namespace std;

#define DEBUG 0

int main(int argc, char **argv)
{
  if(argc<2) {cout<<"Tests the byte ordering of an mrc image and the current system."<<endl<<"Usage: 2dx_endianTest.exe {filename}"<<endl; return 0;}
  int a=1;
  if(DEBUG)
  {
    cout<<"Byte ordering in memory:"<<endl;
    cout<<"Big Endian: 0 0 0 1"<<endl;
    cout<<"Little Endian: 1 0 0 0"<<endl;
    for(int i=0;i<sizeof(int);i++)
    {
     cout<<(int)((unsigned char*)&a)[i]<<" ";
    }
  }
  if((int)((unsigned char*)&a)[0]) cout<<":System is Little Endian"<<endl;
  else cout<<":System is Big Endian"<<endl;

  if(argc>1) if(oppositeEndian(argv[1])) cout<<"::Image has opposite Endian byte ordering."<<endl<<"n"<<endl; else cout<<":Image Endian byte ordering compatible with system."<<endl<<"y"<<endl;

  return 0;
}
