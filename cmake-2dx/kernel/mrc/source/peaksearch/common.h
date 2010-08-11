#ifndef COMMON_H
#define COMMON_H

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fftw3.h>
#include <string.h>
#include "mrcImage.h"
using namespace std;


int shift(int sx, int sy, float *amp, int ML, int *peak_x, int  *peak_y, float *peak_value );
int fft2d_small(char *filename, int Npeaks=40, int Npeaks_final=140, double inner_exclusion_radius=0.01, int mask_radius=1024);
int  mask_image(int sx,int sy,float *amp, float slit_width, float radius_in, float radius_out);
//int fft2d_small(char*, int, int, double,int);
void low_high_pass(int sx,int sy,float *amp, float q_l, float q_h);
int  peak_search(int sx,int sy,float *amp, int ML, int *peak_x, int *peak_y, float *peak_value);
int  peak_search_final(int sx,int sy,float *amp, int ML, int *peak_x, int *peak_y, float *peak_value);
bool importWisdom();
bool exportWisdom();

#endif

