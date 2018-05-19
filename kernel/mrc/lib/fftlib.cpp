#include <complex>
#include <math.h>
#include <fftw3.h>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <string.h>
#ifdef USE_THREADS_2DX
#include <pthread.h>
#include <omp.h>
#endif
using namespace std;

extern "C" {
  int tdxfft_(void *array, const int &nx, const int &ny, const int &idir);
}

bool importWisdom()
{
  char str[80];
  strcpy(str, getenv("HOME"));
  strcat(str,"/.2dx/fftlib.wis");
  FILE *f = fopen(str,"r");
  if(f==NULL) return false;
  fftwf_import_wisdom_from_file(f);
  fclose(f);
  return true;
}

bool exportWisdom()
{
  char str[80];
  strcpy(str, getenv("HOME"));
  strcat(str,"/.2dx/fftlib.wis");
  FILE *f = fopen(str,"w");
  if(f==NULL) return false;
  fftwf_export_wisdom_to_file(f);
  fclose(f);
  return true;
}

int tdxfft_(void *array, const int &nx, const int &ny, const int &idir)
{
#ifdef USE_THREADS_2DX
  fftwf_init_threads();
  // int ithreads = omp_get_max_threads();
  int ithreads = 24;
  printf(" Using %d threads in fftlib.cpp\n",ithreads);
  fftwf_plan_with_nthreads(ithreads);
#else
  printf(" Using no threads in fftlib.cpp\n");
#endif
  fftwf_set_timelimit(30);
  fftwf_plan p;
  if(idir==0) 
  {
    importWisdom();
    p = fftwf_plan_dft_r2c_2d(nx, ny, (float*)array, (fftwf_complex*)array, FFTW_ESTIMATE);
    fftwf_execute(p);
    exportWisdom();
    fftwf_destroy_plan(p);
    float onevol=1.0/sqrtf(nx*ny);
    for(int i=0;i<ny*(nx/2+1);i++) 
    { 
      ((fftwf_complex*)array)[i][0]*=onevol; 
      ((fftwf_complex*)array)[i][1]*=-onevol;
    }
  }
  else
  {
    float onevol=1.0/sqrtf(nx*ny);
    for(int i=0;i<ny*(nx/2+1);i++) 
    {
      ((fftwf_complex*)array)[i][0]*=onevol; 
      if(idir==-1) ((fftwf_complex*)array)[i][1]*=onevol;
      else if(idir==1) ((fftwf_complex*)array)[i][1]*=-onevol;
    }

    importWisdom();
    p = fftwf_plan_dft_c2r_2d(nx, ny, (fftwf_complex*)array, (float*)array, FFTW_ESTIMATE);
    fftwf_execute(p);
    exportWisdom();
    fftwf_destroy_plan(p);
  }

  
#ifdef USE_THREADS_2DX
  fftwf_cleanup_threads();
#endif
  return true;
}
