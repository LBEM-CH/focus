#include "endianOperations.h"
using namespace std;

void byteSwap(void *data, int size)
{
  if(size == 1) return;
  for(int i=0;i<size/2;i++)
  {
    char temp = ((char*)data)[i];
    ((char*)data)[i] = ((char*)data)[size-i-1];
    ((char*)data)[size-i-1] = temp;
  }
}

bool oppositeEndian(char *fileName)
{
  ifstream mrcFile;
  mrcFile.open(fileName, ios::binary);
  if(!mrcFile.is_open()) {cout<<"Cannot read "<<fileName<<endl; return (0);}
  mrcImage::mrcHeader *fileHeader = new mrcImage::mrcHeader;
//  mrcFile.seekg(0,ios::beg);
//  cout<<mrcFile.tellg()<<endl;
  mrcFile.read((char*)fileHeader,1024);
  int bitMask = fileHeader->mapc | fileHeader->mapr | fileHeader->maps;
  mrcFile.close();
  delete fileHeader;

  if(bitMask == 3 || bitMask == 0) return(false);
  else return true;
}

bool byteSwap(char *fileName)
{
  unsigned int dataSize = 0, cellSize;
  char *rawData;

  ifstream mrcFile;
  mrcFile.open(fileName, ios::binary);
  if(!mrcFile.is_open()) {cout<<"Cannot read "<<fileName<<endl; return (0);}

  mrcImage::mrcHeader *header = new mrcImage::mrcHeader; 
  mrcFile.read((char*)header,1024);
 
  for(int i=0;i<56;i++)
    byteSwap(&((int*)header)[i],4);

  int bitMask = header->mapc | header->mapr | header->maps;
/*	if(bitMask == 0)
	{
		header->mapc=1;
		header->mapr=2;
		header->maps=3;
    bitMask=3;
	}
*/
	if(bitMask != 3) {cerr<<"Byte swap has produced an invalid header: "<<header->mapc<<" "<<header->mapr<<" "<<header->maps<<" Should be 1 2 3"<<endl; return (0);}

	unsigned int mode = header->mode;
	if(mode == 0) cellSize = 1;
	else if(mode == 1) cellSize = 2;
	else if(mode == 2) cellSize = 4;
	else if(mode == 3) cellSize = 4;
	else if(mode == 4) cellSize = 8;

	dataSize = header->nx*header->ny*cellSize;
	rawData = new char[dataSize];
	mrcFile.read((char*)rawData,dataSize);
	mrcFile.close();

	ofstream result;
	result.open(fileName, ios::binary);

	result.write((char*)header,1024);

	if(mode == 3 || mode ==4) cellSize/=2;

	if(mode != 0)
		for(int i=0;i<dataSize/cellSize;i++)
			byteSwap(&rawData[i*cellSize],cellSize);

	result.write((char*)rawData,dataSize);

	delete header;
	delete rawData;
	result.close();
	return true;
}

