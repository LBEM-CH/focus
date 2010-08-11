#include <QApplication>
#include <QImage>
#include <QFileInfo>
#include <mrcImage.h>
#include <iostream>
#include <istream>
using namespace std;

int main(int argc, char **argv)
{
  if(argc<3) {cout<<"Converts an mrcImage to a png. Add min/max values to rescale to a new range.\n Usage: 2dx_mrc2png.exe {infile} {outfile} [{min}] [{max}]"<<endl; return 0;}
  if(!QFileInfo(argv[1]).exists()) {cout<<argv[1]<<" does not exist."<<endl; return 0;}
  QApplication app(argc, argv);
  mrcImage image(argv[1]);
  if(argc>4) 
  {
    int min, max;
    //std::istringstream sa(argv[2]), sb(argv[3]); 
    //sa>>min; sb>>max;
    min = atoi(argv[3]); max = atoi(argv[4]);
    
    cout<<"Image has range of: "<<image.getHeader()->min()<<" "<<image.getHeader()->max()<<endl;
    cout<<"Rescaling from "<<min<<" to "<<max<<endl;
    image.rescale(min,max);
  }
  QImage *result = image.getImage();
  QString outfile = argv[2];
  if(!outfile.endsWith(".png")) outfile+=".png";
  result->save(outfile);
  return 0;
}
