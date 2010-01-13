#include <iostream>
#include <mrcImage.h>
#include <endianOperations.h>
using namespace std;

int main(int argc, char **argv)
{
  if(argc<2) {cout<<"Swaps the byte ordering of a file if the file is of an endianness incompatible with the current hardware."<<endl<<"Usage: 2dx_byteSwap.exe {filename}"<<endl; return 0;}
  if(oppositeEndian(argv[1]))
  {
    if(byteSwap(argv[1]))
      cerr<<"Byte swap of "<<argv[1]<<" successful."<<endl;
    else
      cerr<<"Byte swap of "<<argv[1]<<" failed."<<endl;
  }
  else
    cout<<"Endianness of image is already compatible with current image.\n No byte swapping done."<<endl;
  return 0;
}
