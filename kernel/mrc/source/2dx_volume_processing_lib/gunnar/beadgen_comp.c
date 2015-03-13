#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
//#include <fftw3.h>

#define MAX_STRING_LENGTH 1024

  int          map_nx, map_xmin, map_xmax,
               map_ny, map_ymin, map_ymax,
               map_nz, map_zmin, map_zmax;
  double       map_dnx, map_dny, map_dnz,
               map_angle1, map_angle2, map_angle3;
  long int     map_dim, map_dim2, map_dim3;

  double       map_min[3];
  int          map_extent[3];
  double       map_dist[3];
  double       map_dist_1[3];
  double      *map1=NULL;
  double      *map2=NULL;
  char         map1fname[MAX_STRING_LENGTH];
  char         map2fname[MAX_STRING_LENGTH];
  char         mapoutfname[MAX_STRING_LENGTH];
  char         outfname[MAX_STRING_LENGTH];
  int          ind,ind1,ind2,ind3;
  double       sig;
  double       x,y,z;
  FILE         *fp;
  int 	       seed;

  //fftw_plan    planf;



unsigned int count_floats (FILE **fin) {
        unsigned int finl = 0;

        for( ; ; ) {
                if( fgetc(*fin) == EOF ) break;
                ++finl;
        }
        rewind(*fin);
        return finl / 4;
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

double *read_mrc( char *vol_file )
{
        int orom;
        int cubic;
        int ordermode;
        double widthx;
        double widthy;
        double widthz;
        float alpha;
        float beta;
        float gamma;
        double gridx;
        double gridy;
        double gridz;
        unsigned extx;
        unsigned exty;
        unsigned extz;
        float *farr;
        float **fphi;
        float ftmp;
        double fftmp;
        
        double *map;

        unsigned int nvox;
        FILE *fin;
        /*long*/ int nx, ny, nz, mx, my, mz;
        /*long*/ int mode, mxstart, mystart, mzstart, testa, testb, testg;
        float xlen, ylen, zlen;
        int i, swap=0, header_ok = 1;
        int j,k;
        unsigned int nfloat, count;
        /*long*/ int mapc, mapr, maps;
        float amin, amax, amean, junk, xorigin, yorigin, zorigin;

        double m,v;
        fin = fopen(vol_file, "rb");

        if( fin == NULL ) {
          fprintf(stderr,"ERROR: Cannot open file %s !\n",vol_file);
          exit(0);
        }

        fphi= &farr;
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

        map_nx=(int) mx;
        map_ny=(int) my;
        map_nz=(int) mz;

        map_dnx=(double) xlen;
        map_dny=(double) ylen;
        map_dnz=(double) zlen;

        map_dist[0]= (double) map_dnx / (double) map_nx;
        map_dist[1]= (double) map_dny / (double) map_ny;
        map_dist[2]= (double) map_dnz / (double) map_nz;

        map_extent[0]=(int) nx;
        map_extent[1]=(int) ny;
        map_extent[2]=(int) nz;

        /*** changed on 02/07/11 ***/
        if (xorigin==0.0 && yorigin==0.0 && zorigin==0.0) {
            xorigin=mxstart*map_dist[0];
            yorigin=mystart*map_dist[1];
            zorigin=mzstart*map_dist[2];
        }

        map_xmin= 0;
        map_ymin= 0;
        map_zmin= 0;


        map_min[0]=(double) xorigin;
        map_min[1]=(double) yorigin;
        map_min[2]=(double) zorigin;


        map_xmax= map_extent[0] + map_xmin -1;
        map_ymax= map_extent[1] + map_ymin -1;
        map_zmax= map_extent[2] + map_zmin -1;


        map_dist_1[0] = 1.0 / map_dist[0];
        map_dist_1[1] = 1.0 / map_dist[1];
        map_dist_1[2] = 1.0 / map_dist[2];


        map_dim  = map_extent[0];
        map_dim2 = map_extent[0] * map_extent[1];
        map_dim3 = map_extent[0] * map_extent[1] * map_extent[2];

        map_angle1= alpha;
        map_angle2= beta;
        map_angle3= gamma;


        if ( map_dim3 == nfloat-256 ) {

          if ( (map = (double *) calloc( map_dim3 , sizeof( double ) )) == NULL ) {
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
             map1[count]=(double) ftmp;
        }
        fclose (fin);
        extx = nx; exty = ny; extz = nz; nvox = extx * exty * extz;

       testa = floor(100* alpha+0.5); 
       testb = floor(100* beta+0.5); 
       testg = floor(100* gamma+0.5);
       if (testa != 9000 || testb != 9000 || testg != 9000) orom = 0;

       widthx = xlen / (double) mx;
       widthy = ylen / (double) my;
       widthz = zlen / (double) mz;

       if (orom == 0 || floor((widthx-widthy)*1000+0.5)!=0 
              || floor((widthy-widthz)*1000+0.5)!=0 
              || floor((widthx-widthz)*1000+0.5)!=0) cubic = 0;

       gridx = mxstart * widthx;gridy = mystart * widthy;gridz = mzstart * widthz;

        ordermode = 7;
        if (mapc==1 && mapr==2 && maps==3) ordermode = 1;
        if (mapc==1 && mapr==3 && maps==2) ordermode = 2;
        if (mapc==2 && mapr==1 && maps==3) ordermode = 3;
        if (mapc==2 && mapr==3 && maps==1) ordermode = 4;
        if (mapc==3 && mapr==1 && maps==2) ordermode = 5;
        if (mapc==3 && mapr==2 && maps==1) ordermode = 6;
        if (ordermode == 7) {
          exit(1);
        }


  printf("\nDensity Map Info :\n");
  printf("   Grid ........... : %7i   %7i   %7i\n",map_nx,map_ny,map_nz);
  printf("   Index Origin ... : %7i   %7i   %7i\n",map_xmin,map_ymin,map_zmin);
  printf("   Extent ......... : %7i   %7i   %7i\n",map_extent[0],map_extent[1],map_extent[2]);
  printf("   Unit Cell ...... : %7.2f   %7.2f   %7.2f\n",map_dnx,map_dny,map_dnz);
  printf("   Grid Spacing ... : %7.3f   %7.3f   %7.3f\n",
                  map_dist[0], map_dist[1], map_dist[2]);
  printf("   MRC Origin : %7.2f   %7.2f   %7.2f\n", map_min[0],
                   map_min[1],
                   map_min[2]);

  return map;

}

void read_mrc1( char *vol_file )
{
        int orom;
        int cubic;
        int ordermode;
        double widthx;
        double widthy;
        double widthz;
        float alpha;
        float beta;
        float gamma;
        double gridx;
        double gridy;
        double gridz;
        unsigned extx;
        unsigned exty;
        unsigned extz;
        float *farr;
        float **fphi;
        float ftmp;
        double fftmp;

        unsigned int nvox;
        FILE *fin;
        /*long*/ int nx, ny, nz, mx, my, mz;
        /*long*/ int mode, mxstart, mystart, mzstart, testa, testb, testg;
        float xlen, ylen, zlen;
        int i, swap=0, header_ok = 1;
        int j,k;
        unsigned int nfloat, count;
        /*long*/ int mapc, mapr, maps;
        float amin, amax, amean, junk, xorigin, yorigin, zorigin;

        double m,v;
        fin = fopen(vol_file, "rb");

        if( fin == NULL ) {
          fprintf(stderr,"ERROR: Cannot open file %s !\n",vol_file);
          exit(0);
        }

        fphi= &farr;
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

        map_nx=(int) mx;
        map_ny=(int) my;
        map_nz=(int) mz;

        map_dnx=(double) xlen;
        map_dny=(double) ylen;
        map_dnz=(double) zlen;

        map_dist[0]= (double) map_dnx / (double) map_nx;
        map_dist[1]= (double) map_dny / (double) map_ny;
        map_dist[2]= (double) map_dnz / (double) map_nz;

        map_extent[0]=(int) nx;
        map_extent[1]=(int) ny;
        map_extent[2]=(int) nz;

        /*** changed on 02/07/11 ***/
        if (xorigin==0.0 && yorigin==0.0 && zorigin==0.0) {
            xorigin=mxstart*map_dist[0];
            yorigin=mystart*map_dist[1];
            zorigin=mzstart*map_dist[2];
        }

        map_xmin= 0;
        map_ymin= 0;
        map_zmin= 0;


        map_min[0]=(double) xorigin;
        map_min[1]=(double) yorigin;
        map_min[2]=(double) zorigin;


        map_xmax= map_extent[0] + map_xmin -1;
        map_ymax= map_extent[1] + map_ymin -1;
        map_zmax= map_extent[2] + map_zmin -1;


        map_dist_1[0] = 1.0 / map_dist[0];
        map_dist_1[1] = 1.0 / map_dist[1];
        map_dist_1[2] = 1.0 / map_dist[2];


        map_dim  = map_extent[0];
        map_dim2 = map_extent[0] * map_extent[1];
        map_dim3 = map_extent[0] * map_extent[1] * map_extent[2];

        map_angle1= alpha;
        map_angle2= beta;
        map_angle3= gamma;



        if ( map_dim3 == nfloat-256 ) {

          if ( (map1 = (double *) calloc( map_dim3 , sizeof( double ) )) == NULL ) {
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
             map1[count]=(double) ftmp;
        }
        fclose (fin);
        extx = nx; exty = ny; extz = nz; nvox = extx * exty * extz;

       testa = floor(100* alpha+0.5); 
       testb = floor(100* beta+0.5); 
       testg = floor(100* gamma+0.5);
       if (testa != 9000 || testb != 9000 || testg != 9000) orom = 0;

       widthx = xlen / (double) mx;
       widthy = ylen / (double) my;
       widthz = zlen / (double) mz;

       if (orom == 0 || floor((widthx-widthy)*1000+0.5)!=0 
              || floor((widthy-widthz)*1000+0.5)!=0 
              || floor((widthx-widthz)*1000+0.5)!=0) cubic = 0;

       gridx = mxstart * widthx;gridy = mystart * widthy;gridz = mzstart * widthz;

        ordermode = 7;
        if (mapc==1 && mapr==2 && maps==3) ordermode = 1;
        if (mapc==1 && mapr==3 && maps==2) ordermode = 2;
        if (mapc==2 && mapr==1 && maps==3) ordermode = 3;
        if (mapc==2 && mapr==3 && maps==1) ordermode = 4;
        if (mapc==3 && mapr==1 && maps==2) ordermode = 5;
        if (mapc==3 && mapr==2 && maps==1) ordermode = 6;
        if (ordermode == 7) {
          exit(1);
        }


  printf("\nDensity Map Info :\n");
  printf("   Grid ........... : %7i   %7i   %7i\n",map_nx,map_ny,map_nz);
  printf("   Index Origin ... : %7i   %7i   %7i\n",map_xmin,map_ymin,map_zmin);
  printf("   Extent ......... : %7i   %7i   %7i\n",map_extent[0],map_extent[1],map_extent[2]);
  printf("   Unit Cell ...... : %7.2f   %7.2f   %7.2f\n",map_dnx,map_dny,map_dnz);
  printf("   Grid Spacing ... : %7.3f   %7.3f   %7.3f\n",
                  map_dist[0], map_dist[1], map_dist[2]);
  printf("   MRC Origin : %7.2f   %7.2f   %7.2f\n", map_min[0],
                   map_min[1],
                   map_min[2]);




}

void read_mrc2( char *vol_file )
{
        int orom;
        int cubic;
        int ordermode;
        double widthx;
        double widthy;
        double widthz;
        float alpha;
        float beta;
        float gamma;
        double gridx;
        double gridy;
        double gridz;
        unsigned extx;
        unsigned exty;
        unsigned extz;
        float *farr;
        float **fphi;
        float ftmp;
        double fftmp;

        unsigned int nvox;
        FILE *fin;
        /*long*/ int nx, ny, nz, mx, my, mz;
        /*long*/ int mode, mxstart, mystart, mzstart, testa, testb, testg;
        float xlen, ylen, zlen;
        int i, swap=0, header_ok = 1;
        int j,k;
        unsigned int nfloat, count;
        /*long*/ int mapc, mapr, maps;
        float amin, amax, amean, junk, xorigin, yorigin, zorigin;

        double m,v;
        fin = fopen(vol_file, "rb");

        if( fin == NULL ) {
          fprintf(stderr,"ERROR: Cannot open file %s !\n",vol_file);
          exit(0);
        }

        fphi= &farr;
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

        map_nx=(int) mx;
        map_ny=(int) my;
        map_nz=(int) mz;

        map_dnx=(double) xlen;
        map_dny=(double) ylen;
        map_dnz=(double) zlen;

        map_dist[0]= (double) map_dnx / (double) map_nx;
        map_dist[1]= (double) map_dny / (double) map_ny;
        map_dist[2]= (double) map_dnz / (double) map_nz;

        map_extent[0]=(int) nx;
        map_extent[1]=(int) ny;
        map_extent[2]=(int) nz;

        /*** changed on 02/07/11 ***/
        if (xorigin==0.0 && yorigin==0.0 && zorigin==0.0) {
            xorigin=mxstart*map_dist[0];
            yorigin=mystart*map_dist[1];
            zorigin=mzstart*map_dist[2];
        }

        map_xmin= 0;
        map_ymin= 0;
        map_zmin= 0;


        map_min[0]=(double) xorigin;
        map_min[1]=(double) yorigin;
        map_min[2]=(double) zorigin;


        map_xmax= map_extent[0] + map_xmin -1;
        map_ymax= map_extent[1] + map_ymin -1;
        map_zmax= map_extent[2] + map_zmin -1;


        map_dist_1[0] = 1.0 / map_dist[0];
        map_dist_1[1] = 1.0 / map_dist[1];
        map_dist_1[2] = 1.0 / map_dist[2];


        map_dim  = map_extent[0];
        map_dim2 = map_extent[0] * map_extent[1];
        map_dim3 = map_extent[0] * map_extent[1] * map_extent[2];

        map_angle1= alpha;
        map_angle2= beta;
        map_angle3= gamma;



        if ( map_dim3 == nfloat-256 ) {

          if ( (map2 = (double *) calloc( map_dim3 , sizeof( double ) )) == NULL ) {
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
             map2[count]=(double) ftmp;
        }
        fclose (fin);
        extx = nx; exty = ny; extz = nz; nvox = extx * exty * extz;

       testa = floor(100* alpha+0.5); 
       testb = floor(100* beta+0.5); 
       testg = floor(100* gamma+0.5);
       if (testa != 9000 || testb != 9000 || testg != 9000) orom = 0;

       widthx = xlen / (double) mx;
       widthy = ylen / (double) my;
       widthz = zlen / (double) mz;

       if (orom == 0 || floor((widthx-widthy)*1000+0.5)!=0 
              || floor((widthy-widthz)*1000+0.5)!=0 
              || floor((widthx-widthz)*1000+0.5)!=0) cubic = 0;

       gridx = mxstart * widthx;gridy = mystart * widthy;gridz = mzstart * widthz;

        ordermode = 7;
        if (mapc==1 && mapr==2 && maps==3) ordermode = 1;
        if (mapc==1 && mapr==3 && maps==2) ordermode = 2;
        if (mapc==2 && mapr==1 && maps==3) ordermode = 3;
        if (mapc==2 && mapr==3 && maps==1) ordermode = 4;
        if (mapc==3 && mapr==1 && maps==2) ordermode = 5;
        if (mapc==3 && mapr==2 && maps==1) ordermode = 6;
        if (ordermode == 7) {
          exit(1);
        }

/*
  printf("\nDensity Map Info :\n");
  printf("   Grid ........... : %7i   %7i   %7i\n",map_nx,map_ny,map_nz);
  printf("   Index Origin ... : %7i   %7i   %7i\n",map_xmin,map_ymin,map_zmin);
  printf("   Extent ......... : %7i   %7i   %7i\n",map_extent[0],map_extent[1],map_extent[2]);
  printf("   Unit Cell ...... : %7.2f   %7.2f   %7.2f\n",map_dnx,map_dny,map_dnz);
  printf("   Grid Spacing ... : %7.3f   %7.3f   %7.3f\n",
                  map_dist[0], map_dist[1], map_dist[2]);
  printf("   MRC Origin : %7.2f   %7.2f   %7.2f\n", map_min[0],
                   map_min[1],
                   map_min[2]);
*/



}

void write_mrc (char *vol_file, double *map)
{
    FILE  *fout;
    int mxstart, mystart, mzstart;
    unsigned int count, pnvox;
    int nx, ny, nz, mx, my, mz;
    int mode;
    float xlen, ylen, zlen, alpha, beta, gamma;
    int mapc, mapr, maps;
    float amin, amax, amean, junk, fispg;
    char junkchar;
    int ljunk;
    char sjunk[5];
    float ftmp;
    int i, k, j, ind, wmaperr;
    float xorigin, yorigin, zorigin;

        mode= 2;

        mx= (int) map_nx;
        my= (int) map_ny;
        mz= (int) map_nz;
        xlen= (float) map_dnx;
        ylen= (float) map_dny;
        zlen= (float) map_dnz;

        nx= (int) map_extent[0];
        ny= (int) map_extent[1];
        nz= (int) map_extent[2];

        xorigin= (float) (map_min[0]);
        yorigin= (float) (map_min[1]);
        zorigin= (float) (map_min[2]);

        mxstart= (int) map_xmin;
        mystart= (int) map_ymin;
        mzstart= (int) map_zmin;
        alpha= (float) map_angle1;
        beta=  (float) map_angle2;
        gamma= (float) map_angle3;

        mapc= 1;  mapr= 2;  maps= 3;
        amax = 1.0;
        amin = -1.0;
        amean = 0.0f;

        fout = fopen(vol_file, "wb");
        fwrite(&nx,4,1,fout);
        fwrite(&ny,4,1,fout);
        fwrite(&nz,4,1,fout);
        fwrite(&mode ,4,1,fout);
        fwrite(&mxstart ,4,1,fout);
        fwrite(&mystart ,4,1,fout);
        fwrite(&mzstart ,4,1,fout);
        fwrite(&mx ,4,1,fout);
        fwrite(&my ,4,1,fout);
        fwrite(&mz ,4,1,fout);
        fwrite(&xlen ,4,1,fout);
        fwrite(&ylen ,4,1,fout);
        fwrite(&zlen ,4,1,fout);
        fwrite(&alpha ,4,1,fout);
        fwrite(&beta ,4,1,fout);
        fwrite(&gamma ,4,1,fout);
        fwrite(&mapc ,4,1,fout);
        fwrite(&mapr ,4,1,fout);
        fwrite(&maps ,4,1,fout);
        fwrite(&amin ,4,1,fout);
        fwrite(&amax ,4,1,fout);
        fwrite(&amean ,4,1,fout);
        junk=1.0f;
        fwrite(&junk,4,1,fout);
        junk=0.0f;
        for (i=0; i<26; ++i)  fwrite(&junk,4,1,fout);
         fwrite(&xorigin,4,1,fout);
         fwrite(&yorigin,4,1,fout);
         fwrite(&zorigin,4,1,fout);
        strcpy(sjunk,"MAP ");
        fwrite(&sjunk,4,1,fout);
        for (i=0; i<203; ++i) fwrite(&junk,4,1,fout);

        map_dim  = map_extent[0];
        map_dim2 = map_extent[0] * map_extent[1];
        map_dim3 = map_extent[0] * map_extent[1] * map_extent[2];
        for (k=0;k<map_extent[2];k++) {
           for (j=0;j<map_extent[1];j++) {
             for (i=0;i<map_extent[0];i++) {
               ind= k*map_dim2 + j*map_dim + i;
               ftmp= (float) map[ind];
               fwrite(&ftmp,4,1,fout);
             }
           }
        }

        fclose(fout);

}





int main (int argc, char *argv[])
{
  int i;
  double sumdif;
  double sum1, sum2;
  double f1,f2;
  double cc;
  double mean1, mean2;
  double max,min;
  int nbeads;
  double threshold;
  double rnd_number;

// PROCESS COMMAND LINE
if (argc != 7) {
  fprintf(stderr,"USAGE:  beadgen <map.mrc> <out.pdb>  <Number_of_Beads>  <Density_Threshold>  <noise> <rand_seed>\n");
  exit(1);
}

strcpy(map2fname,argv[1]);
strcpy(outfname,argv[2]);
sscanf(argv[3],"%i",&nbeads);
sscanf(argv[4],"%lf",&threshold);
sscanf(argv[5],"%lf",&sig);
sscanf(argv[6],"%i",&seed);
  srand(seed);

//strcpy(map2fname,argv[2]);
//fprintf(stderr," Map 1 .................... : %s\n",map1fname);
//fprintf(stderr," Map 2 .................... : %s\n",map2fname);


// READ MAP 1
//read_mrc1 (map1fname);

// READ MAP 2
read_mrc2 (map2fname);


min=999999.9;
max=-999999.9;
mean1=0.0;
//mean2=0.0;
//sumdif=0.0;
sum1=0.0;
//sum2=0.0;
for(i=0;i<map_dim3;i++) {
   mean1+=map1[i];
   if (map1[i] > max) max=map1[i];
   if (map1[i] < min) min=map1[i];
}

//for(i=0;i<map_dim3;i++) mean2+=map2[i];
mean1/=(double) map_dim3;

fp=fopen(outfname,"w");

for(i=0;i<nbeads;i++) {
   do {
      ind1= rand() % map_extent[0];
      ind2= rand() % map_extent[1];
      ind3= rand() % map_extent[2];
      ind= (ind3 * map_dim2 + ind2 * map_dim + ind1);   
   } while ( map1[ind] < threshold );
  
  x=(map_dist[0] * ind1) + map_min[0] ;
  y=(map_dist[1] * ind2) + map_min[1] ;
  z=(map_dist[2] * ind3) + map_min[2] ;
  //fprintf(stderr," rand %f\n",rand()/(float)RAND_MAX ); 

  rnd_number= rand()/(float)RAND_MAX; 
  //#### CHANGE ATOM TYPES from just Carbon randomly to 
  //#### 62.2 % Carbon, 17.2 % Nitrogen, 20.1 % Oxygen, 0.5 % Sulfur
  if ( rnd_number < 0.622 ) {
  fprintf(fp,"ATOM  %5i  CA  ALA A%4i    %8.3f%8.3f%8.3f  1.00  0.00\n",i%99999,i%9999,sig*2.0*(rand()/(float)RAND_MAX -0.5)+x,sig*2.0*(rand()/(float)RAND_MAX -0.5)+y, sig*2.0*(rand()/(float)RAND_MAX -0.5)+z);
  }
  if ( (rnd_number >= 0.622)  && (rnd_number < 0.794) ) {
  fprintf(fp,"ATOM  %5i  N   ALA A%4i    %8.3f%8.3f%8.3f  1.00  0.00\n",i%99999,i%9999,sig*2.0*(rand()/(float)RAND_MAX -0.5)+x,sig*2.0*(rand()/(float)RAND_MAX -0.5)+y, sig*2.0*(rand()/(float)RAND_MAX -0.5)+z);
  } 
  if ( (rnd_number >= 0.794)  && (rnd_number <= 1.0) ) {
  fprintf(fp,"ATOM  %5i  O   ALA A%4i    %8.3f%8.3f%8.3f  1.00  0.00\n",i%99999,i%9999,sig*2.0*(rand()/(float)RAND_MAX -0.5)+x,sig*2.0*(rand()/(float)RAND_MAX -0.5)+y, sig*2.0*(rand()/(float)RAND_MAX -0.5)+z);
  } 

}
  
fclose(fp);

}


