#include <complex>
#include <math.h>
#include <fftw3.h>
#include <iostream>
using namespace std;

extern "C"{
void tdxconv_(fftwf_complex *x, const int &xw, const int &xh, fftwf_complex *k, const int &kw, const int &kh, fftwf_complex *y); 
}

void tdxconv_(fftwf_complex *x, const int &xw, const int &xh, fftwf_complex *k, const int &kw, const int &kh, fftwf_complex *y) 
{
  int w=xw+kw-1, h=xh+kh-1;
  //if(xw>kw) w = xw; else w = kw;
  //if(xh>kh) h = xh; else h = kh;

  fftwf_complex *X=new(fftwf_complex[w*h]);
  fftwf_complex *H=new(fftwf_complex[w*h]);
  fftwf_plan p;

  cout<<"\n\nImage - 2dx_conv"<<xw<<" "<<xh<<endl;
  int l,m;
  for(int j=0;j<h;j++)
  {
    for(int i=0;i<w;i++)
    {
      if(i<xw && j<xh)
      {
        l = i; m = j;
        X[i+j*w][0]=x[l+m*xw][0];
        X[i+j*w][1]=x[l+m*xw][1];
        cout<<X[i+j*w][0]<<" + i("<<X[i+j*w][1]<<")\t";
      }
      else
      {
        X[i+j*w][0]=0.0;
        X[i+j*w][1]=0.0;
      }    
    }
    cout<<endl;
  }
 
  cout<<endl<<"\n\nKernel - 2dx_conv"<<kw<<" "<<kh<<endl;
  for(int j=0;j<h;j++)
  {
    for(int i=0;i<w;i++)
    {
      if(i<kw && j<kh)
      {
        H[i+j*w][0]=k[i+j*kw][0];
        H[i+j*w][1]=k[i+j*kw][1];
        cout<<H[i+j*w][0]<<" + i("<<H[i+j*w][1]<<")\t";
      }
      else
      {
        H[i+j*w][0]=0.0;
        H[i+j*w][1]=0.0;
      }
      y[i+j*w][0]=0.0;
      y[i+j*w][1]=0.0;
    }
    cout<<endl;
  }

  cout<<endl;
 
  p = fftwf_plan_dft_2d(w,h,X,X,FFTW_FORWARD,FFTW_ESTIMATE);
  fftwf_execute(p);
  fftwf_destroy_plan(p);
 
  p = fftwf_plan_dft_2d(w,h,H,H,FFTW_FORWARD,FFTW_ESTIMATE);
  fftwf_execute(p);
  fftwf_destroy_plan(p); 
 
  fftwf_complex a;
 
  for(int j=0;j<h;j++)
    for(int i=0;i<w;i++)
    {
      a[0]=X[i+j*w][0]*H[i+j*w][0]-X[i+j*w][1]*H[i+j*w][1];
      a[1]=X[i+j*w][0]*H[i+j*w][1]+X[i+j*w][1]*H[i+j*w][0];

      y[i+j*w][0]=a[0]/(w*h);
      y[i+j*w][1]=a[1]/(w*h);
    }

  p = fftwf_plan_dft_2d(w,h,y,y,FFTW_BACKWARD,FFTW_ESTIMATE);
  fftwf_execute(p);
  fftwf_destroy_plan(p);

  cout<<"\n\nOutput - 2dx_conv: "<<endl;

  for(int j=0;j<h;j++)
  {
    for(int i=0;i<w;i++)
    {
       float a = sqrt(powf(y[i+j*w][0],2.0)+ powf(y[i+j*w][1],2.0)); 
       if(a<0.001) a = 0;
        cout.precision(3);
        cout<<a<<"\t";
    } 
    cout<<endl;
  }

  delete X;
  delete H;
}
