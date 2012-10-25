/*
 *  mrcfiles.h
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#ifndef MRCFILES_H
#define MRCFILES_H

// #include <stdio.h>
// #include <stdlib.h>

#include "hvemtypes.h"
#include "imodconfig.h"

#ifndef FALSE
#define FALSE       0           /*false for boolean*/
#endif
#ifndef TRUE
#define TRUE        1           /*true for boolean*/
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif


#define b3dInt16        short 
#define b3dUInt16       unsigned short
#define b3dInt32        int
#define b3dUInt32       unsigned int
#define b3dFloat        float
#define b3dByte         char
#define b3dUByte        unsigned char




#define MRC_IDTYPE_MONO   0
#define MRC_IDTYPE_TILT   1
#define MRC_IDTYPE_TILTS  2
#define MRC_IDTYPE_LINA   3
#define MRC_IDTYPE_LINS   4 

#define MRC_SCALE_LINEAR  1
#define MRC_SCALE_POWER   2
#define MRC_SCALE_LOG     3
#define MRC_SCALE_BKG     4

/* DOC_CODE MRC data modes */
/* The modes defined for MRC files in IMOD */
#define MRC_MODE_BYTE          0
#define MRC_MODE_SHORT         1
#define MRC_MODE_FLOAT         2
#define MRC_MODE_COMPLEX_SHORT 3
#define MRC_MODE_COMPLEX_FLOAT 4
#define MRC_MODE_USHORT        6
#define MRC_MODE_RGB           16
/* END_CODE */

#define MRC_WINDOW_DATASIZE 1
#define MRC_WINDOW_FULL     2
#define MRC_WINDOW_NTSC     3

#define MRC_RAMP_LIN 1
#define MRC_RAMP_EXP 2
#define MRC_RAMP_LOG 3


#define MRC_LABEL_SIZE         80
#define MRC_NEXTRA             16
#define MRC_NLABELS            10
#define MRC_HEADER_SIZE        1024   /* Length of Header is 1024 bytes. */
#define MRC_MAXCSIZE           3


typedef struct  /*complex floating number*/
{
  float a;
  float b;

} Complexfloat;

typedef struct  /*complex short number*/
{
  short a;
  short b;

} ComplexShort;

/* DOC_CODE MrcHeader structure */
/* The header structure for MRC files */
typedef struct MRCheader
{
  int   nx;         /*  # of Columns                  */
  int   ny;         /*  # of Rows                     */
  int   nz;         /*  # of Sections.                */
  int   mode;       /*  given by #define MRC_MODE...  */

  int   nxstart;    /*  Starting point of sub image.  */
  int   nystart;
  int   nzstart;

  int   mx;         /* Number of rows to read.        */
  int   my;
  int   mz;

  float   xlen;       /* length of x element in um.     */
  float   ylen;       /* get scale = xlen/nx ...        */
  float   zlen;

  float   alpha;      /* cell angles, ignore */
  float   beta;
  float   gamma;

  int   mapc;       /* map coloumn 1=x,2=y,3=z.       */
  int   mapr;       /* map row     1=x,2=y,3=z.       */
  int   maps;       /* map section 1=x,2=y,3=z.       */

  float   amin;
  float   amax;
  float   amean;
  
  /* 1/12/12: Removed nsymbt and made ispg be 4 bytes to match standard */
  int   ispg;       /* space group number in the standard */

  /* 64 bytes */

  int   next;     /* This is nsymbt in the MRC standard */
  short   creatid;  /* Used to be creator id, hvem = 1000, now 0 */

  char    blank[30];
  
  short   nint;
  short   nreal;
  short   sub;
  short   zfac;

  float   min2;
  float   max2;
  float   min3;
  float   max3;
  int   imodStamp;
  int   imodFlags;

  /*  UINT   extra[MRC_NEXTRA];*/

  /* HVEM extra data */
  /* DNM 3/16/01: divide idtype into two shorts */
  short   idtype;
  short   lens;
  short   nd1;     /* Devide by 100 to get float value. */
  short   nd2;
  short   vd1;
  short   vd2;
  float   tiltangles[6];  /* 0,1,2 = original:  3,4,5 = current */

#ifdef OLD_STYLE_HEADER
  /* before 2.6.20 */
  /* DNM 3/16/01: redefine the last three floats as wavelength numbers */
  short   nwave;   /* # of wavelengths and values */
  short   wave1;
  short   wave2;
  short   wave3;
  short   wave4;
  short   wave5;
  float   zorg;           /* origin */
  
  float   xorg;
  float   yorg;
#else
  /* MRC 2000 standard */
  float   xorg;
  float   yorg;
  float   zorg;
  char    cmap[4];
  char    stamp[4];
  float   rms;
#endif

  int nlabl;
  char  labels[MRC_NLABELS][MRC_LABEL_SIZE + 1];

  /* Internal data not stored in file header */
  unsigned char *symops;
  FILE   *fp;
  int    pos;
  struct LoadInfo *li;
  int    headerSize;
  int    sectionSkip;
  int    swapped;
  int    bytesSigned;
  int    yInverted;

  char *pathname;
  char *filedesc;
  char *userData;
} MrcHeader;
/* END_CODE */


/* to get from index to model coords -> scale -> subtract org -> rotate */
/* rotate by current tilt angles ? */

/* DOC_CODE IloadInfo structure */
/*
 * Used to control loading a subsection of a 3-D data file. 
 */
typedef struct LoadInfo
{

  /* Sub area to load. */
  int xmin;      
  int xmax;
  int ymin;
  int ymax;
  int zmin;
  int zmax;
  
  int ramp;      /* Contrast ramp type. */
  int scale;
  int black;     /* Change contrast values. */
  int white;
  
  int axis;      /* 1=x, 2=y, 3=z , 0 = default */
  
  float slope;         /* Scale input by value * slope + offset */
  float offset;
  float smin,smax;     /* Scale range smin-smax in input to 0-255 */
  int contig;          /* Load idata in contigous memory if possible */
  
  int outmin, outmax;  /* clamp values to outmin and outmax after scaling. */
  int mirrorFFT;       /* Return mirrored FFT when scaling to bytes */
  
  int   plist;         /* Size of piece list.         */
  float opx, opy, opz; /* origin of pieces.           */
  float px, py, pz;    /* size of final pieced image. */
  int   pdz;           /* number of zsections that have data. */
  int *pcoords;        /* The piece list.             */

} IloadInfo;
/* END_CODE */


struct TiltInfo
{

  float *tilt;
  float  axis_x;
  float  axis_y;
  float  axis_z;
  float  scale_x;
  float  scale_y;
  float  scale_z;
  float  alpha;
  float  beta;
  float  gamma;

};


#ifdef __cplusplus
extern "C" {
#endif


/******************************** Header functions **************************/
int mrc_head_read (FILE *fin,  MrcHeader *hdata);
int mrc_head_write(FILE *fout, MrcHeader *hdata);
void mrcInitOutputHeader(MrcHeader *hdata);
int mrcCopyExtraHeader(MrcHeader *hin, MrcHeader *hout);
int mrc_head_label(MrcHeader *hdata, const char *label);
int mrc_head_new  (MrcHeader *hdata, int x, int y, int z, int mode);
int mrc_byte_mmm  (MrcHeader *hdata, unsigned char **idata);
int mrc_head_label_cp(MrcHeader *hin, MrcHeader *hout);
int mrc_test_size(MrcHeader *hdata);

void mrc_get_scale(MrcHeader *h, float *xs, float *ys, float *zs);
void mrc_set_scale(MrcHeader *h, double x, double y, double z);
void mrc_coord_cp(MrcHeader *hout, MrcHeader *hin);
          

/************************* Write image data functions ************************/
int mrc_write_byte (FILE *fout, MrcHeader *hdata, unsigned char **data);
int mrc_write_idata(FILE *fout, MrcHeader *hdata, void *data[]);
int mrc_data_new   (FILE *fout, MrcHeader *hdata);
int mrc_write_slice(void *buf, FILE *fout, MrcHeader *hdata, 
                    int slice, char axis);
int parallelWriteSlice(void *buf, FILE *fout, MrcHeader *hdata, int slice);

/************************ Read image data functions **************************/
float mrc_read_point (FILE *fin, MrcHeader *hdata, int x, int y, int z);
void *mrc_mread_slice(FILE *fin, MrcHeader *hdata,
                      int slice, char axis);
int mrc_read_slice(void *buf, FILE *fin, MrcHeader *hdata, 
                   int slice, char axis);
int mrcReadfloatSlice(float *buf, MrcHeader *hdata, int slice);

  unsigned char **mrcGetDataMemory(struct LoadInfo *li, size_t xysize,
                                   int zsize, int pixsize);
  void mrcFreeDataMemory(unsigned char **idata, int contig, int zsize);
  float mrcGetComplexScale();
  void mrcComplexSminSmax(float inMin, float inMax, float *outMin, 
                           float *outMax);
  void mrcMirrorSource(int nx, int ny, int imageX, int imageY, int *fileX,
                       int *fileY);
void mrcContrastScaling(MrcHeader *hdata, float smin, float smax, int black,
                        int white, int ramptype, float *slope, float *offset);

unsigned char **read_mrc_byte(FILE *fin, MrcHeader *hdata, 
                              struct LoadInfo *li);
unsigned char **mrc_read_byte(FILE *fin, MrcHeader *hdata, 
                              struct LoadInfo *li,
                              void (*func)(const char *));

int mrcReadSectionbyte(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, int z);
int mrcReadZbyte(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, int z);
int mrcReadYbyte(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, int y);
int mrcReadSectionUShort(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, 
                         int z);
int mrcReadZUShort(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, int z);
int mrcReadYUShort(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, int y);
int mrcReadZ(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, int cz);
int mrcReadY(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, int cy);
int mrcReadSection(MrcHeader *hdata, struct LoadInfo *li, unsigned char *buf, int z);
int mrcReadSectionfloat(MrcHeader *hdata, IloadInfo *li, float *buf, int z);
int mrcReadYfloat(MrcHeader *hdata, IloadInfo *li, float *buf, int z);
int mrcReadZfloat(MrcHeader *hdata, IloadInfo *li, float *buf, int z);

/* misc stdio functions */
int  loadtilts(struct TiltInfo *ti, MrcHeader *hdata);
int  getloadinfo(MrcHeader *hdata,  struct LoadInfo *li); 
int  mrc_init_li(struct LoadInfo *li, MrcHeader *hd);
int  mrc_plist_li(struct LoadInfo *li, MrcHeader *hdata, const char *fname);
int  mrc_plist_load(struct LoadInfo *li, MrcHeader *hdata, FILE *fin);
int  mrc_plist_proc(struct LoadInfo *li, int nx, int ny, int nz);
int  mrc_plist_create(struct LoadInfo *li, int nx, int ny, int nz, int nfx, 
                      int nfy, int ovx, int ovy);
int  iiPlistLoadF(FILE *fin, struct LoadInfo *li, int nx, int ny, int nz);
int  iiPlistLoad(const char *filename, struct LoadInfo *li, int nx, int ny, int nz);
int iiPlistFromMetadata(const char *filename, int addMdoc, IloadInfo *li, int nx, 
                        int ny, int nz);
void mrc_liso(MrcHeader *hdata, struct LoadInfo *li);
int mrc_fix_li(struct LoadInfo *li, int nx, int ny, int nz);
int get_loadinfo(MrcHeader *hdata, struct LoadInfo *li);
unsigned char *get_byte_map(float slope, float offset, int outmin, int outmax, 
                            int bytesSigned);
unsigned char *get_short_map(float slope, float offset, int outmin, int outmax,
                             int ramptype, int swapbytes, int signedint);


/************************ Internal functions *********************************/
int getfilename(char *name, char *prompt);
void mrc_default_status(const char *string);
int mrc_getdcsize(int mode, int *dsize, int *csize);
void mrc_swap_shorts(short *data, int amt);
void mrc_swap_longs(int *data, int amt);
void mrc_swap_floats(float *data, int amt);
void mrc_swap_header(MrcHeader *hdata);
void mrc_set_cmap_stamp(MrcHeader *hdata);

#ifdef __cplusplus
}
#endif
#endif
