#include <iostream>
using namespace std;
int main()
{
  int a=1;
  cout<<"Byte ordering in memory:"<<endl;
  cout<<"Big Endian: 0 0 0 1"<<endl;
  cout<<"Little Endian: 1 0 0 0"<<endl;
  for(int i=0;i<sizeof(int);i++)
  {
   cout<<(int)((unsigned char*)&a)[i]<<" ";
  }
  cout<<endl;
  return 0;
}
