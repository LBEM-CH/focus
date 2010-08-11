#include "common.h"
//#include "test.h"
int main(int argc, char **argv)
{   int dir,m,i;
   
  printf("\n2dx_peaksearch: Finds all peaks in frequency domain of an image. \n");

printf("argc=%d  argv=%s %d %d %f %d \n",argc,argv[1],atoi(argv[2]), atoi(argv[3]), atof(argv[4]), atoi(argv[5]));
fflush(stdout);


  if(argc!=2 && argc!=3 && argc!=4 && argc!=5 && argc!=6)
  {
    printf("\t usage: 2dx_peaksearch.exe <imagename>.mrc {<fft_peaks=40> <shifted_fft_peaks=140> <inner_exclusion_radius=0.01>}\n");
    printf("\t example: 2dx_peaksearch.exe AII00327305.mrc\n"); 
    printf("\t example: 2dx_peaksearch.exe AII00327304.mrc 60 300 0.01\n"); 
    exit(1);
  }



  if(argc==2)
    fft2d_small(argv[1]);
  else if(argc==3)
    fft2d_small(argv[1],atoi(argv[2]));
  else if(argc==4)
    fft2d_small(argv[1],atoi(argv[2]),atoi(argv[3])); 
  else if(argc==5)
    fft2d_small(argv[1],atoi(argv[2]),atoi(argv[3]),atof(argv[4])); 
  else  if(argc==6)
    fft2d_small(argv[1],atoi(argv[2]),atoi(argv[3]),atof(argv[4]),atoi(argv[5])); 
     
    
 //   dir=1; m=2;
 //   i=fft_2d(dir,m);

  return 0;
}
