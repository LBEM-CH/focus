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
double *map_vals_with_id;
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

int compare_double (const double *a, const double *b)
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

if (argc<2) {
  fprintf(stderr,"USAGE: maphist infile.mrc infile-ref.mrc outfile.mrc \n");
  exit(1);
}

strcpy(mapinfile,argv[1]);
strcpy(maprefinfile,argv[2]);
strcpy(mapoutfile,argv[3]);

printf("MAPHIST \n");

/*************************************************/
/*****   READ INPUT MAP HISTOGRAM MATCHING   *****/
/*************************************************/
printf("Reading main map  ...\n");
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
        printf("Main map: %i %i %i\n", nx, ny, nz);

        if ( map_dim3 == nfloat-256 ) {
          if ( (map_vals_with_id = (double *) calloc( 2*map_dim3 , sizeof( double ) )) == NULL ) {
             fprintf(stderr,"ERROR: could not allocate memory \n");
             exit(1);
          }
        } else {
           fprintf(stderr,"ERROR: number of elements found different from header specification.\n");
        }

	for (count=0; count<map_dim3; count++) { 
             if (read_float(&ftmp,fin,swap)==0) {
                fprintf(stderr,"error reading map values\n");
                exit(1);    
             }
             map_vals_with_id[2*count]=   (double) ftmp; 
             map_vals_with_id[2*count+1]= (double) count;
	} 

	fclose (fin);



/*************************************************/
/***   READ REFERENCE MAP  ***********************/
/*************************************************/
printf("Reading reference map ...\n");
fflush(stdout);
fin = fopen(maprefinfile, "rb");

if( fin == NULL ) {
         fprintf(stderr,"ERROR: Cannot open file %s !\n",maprefinfile);
         exit(0);		
} 

        //fphi= &farr;
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

        printf("Ref map: %i %i %i\n", nx, ny, nz);
        if (map_dim3 != nx * ny * nz) { 
           fprintf(stderr,"ERROR: number of elements different in main map and reference map.\n");
           exit(1);
        }

        if ( map_dim3 <= nfloat-256 ) {
          if ( (ref_vals = (float *) calloc( map_dim3 , sizeof( float ) )) == NULL ) {
             fprintf(stderr,"ERROR: could not allocate memory \n");
             exit(1);
          }
        } else {
           fprintf(stderr,"ERROR: number of elements found different from header specification.\n");
        }

	for (count=0; count<map_dim3; count++) { 
             if (read_float(&ftmp,fin,swap)==0) {
                fprintf(stderr,"error reading map values\n");
                exit(1);    
             }
             ref_vals[count]=(float) ftmp; 
	} 

	fclose (fin);




/*********************/
/** SORT REF VALUES **/
/*********************/

printf("Sorting main map values ...\n");
fflush(stdout);
qsort(map_vals_with_id,map_dim3,2*sizeof(double),compare_double);

if ( (id = (int *) calloc( map_dim3 , sizeof( int ) )) == NULL ) {
             fprintf(stderr,"ERROR: could not allocate memory \n");
             exit(1);
}

for (i=0;i<map_dim3;i++) 
{
    id[i]=(int) map_vals_with_id[2*i+1];
}

free(map_vals_with_id);


printf("Sorting reference map values ...\n");
fflush(stdout);
qsort(ref_vals,map_dim3,sizeof(float),compare_floats);



/**********************************/
/**  READ MAIN MAP            *****/
/**********************************/
printf("Reading main map values (again) ...\n");
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

        if ( map_dim3 == nfloat-256 ) {
          if ( (map_vals = (float *) calloc( map_dim3 , sizeof( float ) )) == NULL ) {
             fprintf(stderr,"ERROR: could not allocate memory \n");
             exit(1);
          }
        } else {
           fprintf(stderr,"ERROR: number of elements found different from header specification.\n");
        }
        
	for (count=0; count<map_dim3; count++) { 
             if (read_float(&ftmp,fin,swap)==0) {
                fprintf(stderr,"error reading map values\n");
                exit(1);    
             }
             map_vals[count]=   (float) ftmp; 
	} 

	fclose (fin);



for (i=0;i<map_dim3;i++) {
    //printf("%i %i \n", i, id[i]);
    map_vals[id[i]]= (float) ref_vals[i];
}

printf("Done.. Writing output\n");
/*********************/
/**  WRITE NEW MAP  **/
/*********************/

        mapc= 1;  mapr= 2;  maps= 3;
/*
        amean_tmp=0.0; amax=-999999.9; amin=999999.9;
        for (i=0;i<map_dim3;i++) {
          ftmp=map_vals[2*i];
          if (ftmp<amin) amin=ftmp;
          if (ftmp>amax) amax=ftmp;
          amean_tmp+=ftmp;
        }
        amean=(float) (amean_tmp/ (double) map_dim3);
 */        

        amax = 1.0;
        amin = -1.0;
        amean = 0.0f;

	fp = fopen(mapoutfile, "wb");

	fwrite(&nx,4,1,fp);
	fwrite(&ny,4,1,fp);
	fwrite(&nz,4,1,fp);
	fwrite(&mode ,4,1,fp);
	fwrite(&mxstart ,4,1,fp);
	fwrite(&mystart ,4,1,fp);
	fwrite(&mzstart ,4,1,fp);
	fwrite(&mx ,4,1,fp);
	fwrite(&my ,4,1,fp);
	fwrite(&mz ,4,1,fp);
	fwrite(&xlen ,4,1,fp);
	fwrite(&ylen ,4,1,fp);
	fwrite(&zlen ,4,1,fp);
	fwrite(&alpha ,4,1,fp);
	fwrite(&beta ,4,1,fp);
	fwrite(&gamma ,4,1,fp);
	fwrite(&mapc ,4,1,fp);
	fwrite(&mapr ,4,1,fp);
	fwrite(&maps ,4,1,fp);
	fwrite(&amin ,4,1,fp);
	fwrite(&amax ,4,1,fp);
	fwrite(&amean ,4,1,fp);
        junk=1.0f;
	fwrite(&junk,4,1,fp);

        junk=0.0f;
	for (i=0; i<26; ++i)  fwrite(&junk,4,1,fp);
	 fwrite(&xorigin,4,1,fp);
	 fwrite(&yorigin,4,1,fp);
	 fwrite(&zorigin,4,1,fp);
        strcpy(sjunk,"MAP ");
	fwrite(&sjunk,4,1,fp);
	for (i=0; i<203; ++i) fwrite(&junk,4,1,fp);

      for (i=0;i<map_dim3;i++) {
        ftmp= (float) map_vals[i];
        fwrite(&ftmp,4,1,fp);
      }
   fclose(fp);

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




/*
	printf("map2map>       NC = %8d  (# columns)\n",nx); 
	printf("map2map>       NR = %8d  (# rows)\n",ny); 
	printf("map2map>       NS = %8d  (# sections)\n",nz); 
	printf("map2map>     MODE = %8d  (data type: 2 = stored as floats)\n",mode); 
	printf("map2map>  NCSTART = %8d  (index of first column, starting at 0)\n",mxstart); 
	printf("map2map>  NRSTART = %8d  (index of first row, starting at 0)\n",mystart);
	printf("map2map>  NSSTART = %8d  (index of first section, starting at 0)\n",mzstart);
	printf("map2map>       NX = %8d  (# of X intervals in unit cell)\n",mx); 
	printf("map2map>       NY = %8d  (# of Y intervals in unit cell)\n",my);
	printf("map2map>       NZ = %8d  (# of Z intervals in unit cell)\n",mz);
	printf("map2map> X length = %8.3f  (unit cell dimension)\n",xlen);
	printf("map2map> Y length = %8.3f  (unit cell dimension)\n",ylen);
	printf("map2map> Z length = %8.3f  (unit cell dimension)\n",zlen);
	printf("map2map>    Alpha = %8.3f  (unit cell angle)\n",alpha);
	printf("map2map>     Beta = %8.3f  (unit cell angle)\n",beta);
	printf("map2map>    Gamma = %8.3f  (unit cell angle)\n",gamma);
	printf("map2map>     MAPC = %8d  (columns axis: 1=X)\n",mapc); 
	printf("map2map>     MAPR = %8d  (rows axis: 2=Y)\n",mapr);
	printf("map2map>     MAPS = %8d  (sections axis: 3=Z)\n",maps);
	printf("map2map>     AMIN = %8.3f  (minimum density value)\n",amin);
	printf("map2map>     AMAX = %8.3f  (maximum density value)\n",amax);
	printf("map2map>    AMEAN = %8.3f  (mean density value)\n",amean);
	printf("map2map>  XORIGIN = %8.3f  (ignored: X origin)\n",xorigin);
	printf("map2map>  YORIGIN = %8.3f  (ignored: Y origin)\n",yorigin);
	printf("map2map>  ZORIGIN = %8.3f  (ignored: Z origin)\n",zorigin);
*/
