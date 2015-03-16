/********************************************************************

   MAPHIST
   MATCH DENSITY HISTOGRAM TO REFERENCE MAP
   (C) 2011  GUNNAR F SCHROEDER 

********************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <fftw3.h>

int    map_nx, map_ny, map_nz;
int    map_xmin, map_xmax, map_ymin;
int    map_ymax, map_zmin, map_zmax;
long int    map_dim, map_dim2, map_dim3;
int    map_extent[3];
double map_dist[3];
double map_dist_1[3];
double map_min[3];
float *map_vals;
float *ref_vals;
int *id;



int read_float (float *currfloat, FILE *fin, int swap);
int read_int (int *currlong, FILE *fin, int swap);
unsigned int count_floats (FILE **fin);

int compare_floats (const float *a, const float *b)
{
  if ( *a > *b )  return 1 ;
  else return -1;
}

int compare_floats_2 (const float *a, const float *b)
{
  if ( a[0] > b[0] )  return 1 ;
  else return -1;
}

int main(int argc, char *argv[])
{


int cubic; 
float alpha;
float beta; 
float gamma; 
double gridx; 
double gridy;
double gridz; 
float ftmp;

FILE *fin;
FILE *file;
FILE *fp;

int nx, ny, nz, mx, my, mz;
int mode, mxstart, mystart, mzstart;
float xlen, ylen, zlen;
int i, swap=0, header_ok = 1;
char sjunk[5];
int j,k;
unsigned int nfloat; 
long int count;
int mapc, mapr, maps;
float amin, amax, amean, junk, xorigin, yorigin, zorigin;
double amean_tmp;

char mapinfile[200];
char maprefinfile[200];
char mapoutfile[200];

if (sizeof(int) != sizeof(float) ) {
    fprintf(stderr,"ERROR: sizeof(int) != sizeof(float) \n");
    exit(1);
}

if (argc<1) {
  fprintf(stderr,"USAGE: maphist infile.mrc infile-ref.mrc outfile.mrc \n");
  exit(1);
}

strcpy(mapinfile,argv[1]);

/*************************************************/
/*****   READ INPUT MAP HISTOGRAM MATCHING   *****/
/*************************************************/
fflush(stdout);
fin = fopen(mapinfile, "rb");

if( fin == NULL ) {
         fprintf(stderr,"ERROR: Cannot open file %s !\n",mapinfile);
         exit(0);		
} 
	nfloat = count_floats(&fin);
	header_ok *= read_int(&nx,fin,swap);
	header_ok *= read_int(&ny,fin,swap);
	header_ok *= read_int(&nz,fin,swap);
	header_ok *= read_int(&mode,fin,swap);
	header_ok *= read_int(&mxstart,fin,swap);
	header_ok *= read_int(&mystart,fin,swap);
	header_ok *= read_int(&mzstart,fin,swap);
	header_ok *= read_int(&mx,fin,swap);
	header_ok *= read_int(&my,fin,swap);
	header_ok *= read_int(&mz,fin,swap);
	header_ok *= read_float(&xlen,fin,swap);
	header_ok *= read_float(&ylen,fin,swap);
	header_ok *= read_float(&zlen,fin,swap);
	header_ok *= read_float(&alpha,fin,swap);
	header_ok *= read_float(&beta,fin,swap);
	header_ok *= read_float(&gamma,fin,swap);
	header_ok *= read_int(&mapc,fin,swap);
	header_ok *= read_int(&mapr,fin,swap);
	header_ok *= read_int(&maps,fin,swap);
	header_ok *= read_float(&amin,fin,swap);
	header_ok *= read_float(&amax,fin,swap);
	header_ok *= read_float(&amean,fin,swap);
	for (i=0; i<27; ++i) header_ok *= read_float(&junk,fin,swap);
	header_ok *= read_float(&xorigin,fin,swap);
	header_ok *= read_float(&yorigin,fin,swap);
	header_ok *= read_float(&zorigin,fin,swap);
	for (i=0; i<204; ++i) header_ok *= read_float(&junk,fin,swap);
	if (header_ok == 0) {
                exit(1);
	} 

        map_dim3= nx * ny * nz;
        printf("%d,%d,%d", nx, ny, nz);


	fclose (fin);

}


int read_float (float *currfloat, FILE *fin, int swap) {
        unsigned char *cptr, tmp;

        if (fread(currfloat,4,1,fin)!=1) return 0;
        if (swap == 1) {
                cptr = (unsigned char *)currfloat;
                tmp = cptr[0];
                cptr[0]=cptr[3];
                cptr[3]=tmp;
                tmp = cptr[1];
                cptr[1]=cptr[2];
                cptr[2]=tmp;
        }
        return 1;
}


int read_int (int *currlong, FILE *fin, int swap) {
        unsigned char *cptr, tmp;

        if (fread(currlong,4,1,fin)!=1) return 0;
        if (swap == 1) {
                cptr = (unsigned char *)currlong;
                tmp = cptr[0];
                cptr[0]=cptr[3];
                cptr[3]=tmp;
                tmp = cptr[1];
                cptr[1]=cptr[2];
                cptr[2]=tmp;
        }
        return 1;
}


unsigned int count_floats (FILE **fin) {
        unsigned int finl = 0;

        for( ; ; ) {
                if( fgetc(*fin) == EOF ) break;
                ++finl;
        }
        rewind(*fin);
        return finl / 4;
}