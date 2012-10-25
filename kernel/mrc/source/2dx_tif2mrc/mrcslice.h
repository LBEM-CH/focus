/*
 *  mrcslice.h : Definitions and declarations for mrcslice.c
 *
 *  Original author: James Kremer
 *  Revised by David Mastronarde
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#ifndef MRCSLICE_H
#define MRCSLICE_H
#include "mrcfiles.h"

/* DOC_CODE MRCslice definitions */
/* The following modes are supported by the MRC file format. */
#define SLICE_MODE_BYTE          0   /* type unsigned char                   */
#define SLICE_MODE_SHORT         1   /* type short                           */
#define SLICE_MODE_FLOAT         2   /* type float                           */
#define SLICE_MODE_COMPLEX_SHORT 3   /* 2 channels of short                  */
#define SLICE_MODE_COMPLEX_FLOAT 4   /* 2 channels of floats                 */
#define SLICE_MODE_USHORT        6   /* unsigned short                       */
#define SLICE_MODE_RGB           16  /* 3 channels of bytes                  */

/* Other modes */
#define SLICE_MODE_MAX           99  /* float data with max channels.        */
#define SLICE_MODE_UNDEFINED     -1  /* float data with max channels.        */
#define SLICE_MODE_SBYTE         -2  /* values for sliceMode to return when  */
#define SLICE_MODE_UBYTE         -3  /* user enters 'sbyte' or 'ubyte'       */

/* The maximum values for channels and bytes/channel */
#define SLICE_MAX_CSIZE          3   /* max channels.                        */
#define SLICE_MAX_DSIZE          4   /* max data size of pixel in bytes.     */
/* END_CODE */

/* DOC_CODE Ival type */
/* An Ival is just an small array of floats to hold any type of data */
typedef float Ival[SLICE_MAX_CSIZE];
/* END_CODE */

/* DOC_CODE Islice structure */
/* Union to support all data types */
union MRCdata
{
  unsigned char *b;
  b3dInt16      *s;
  b3dUInt16     *us;
  b3dFloat      *f;
};

/* The Islice structure */
typedef struct MRCslice
{
  union MRCdata data;              /* pointer to data: data.b, data.s, etc */
  b3dInt32 xsize;                  /* size of data.           */
  b3dInt32 ysize;
  b3dInt32 mode;                   /* type of storage         */
  b3dInt32 csize;                  /* number of data channels */
  b3dInt32 dsize;                  /* data size for pixel     */
  float  min, max, mean;
  b3dInt32 index;                  /* index of current value  */
  float  cval[SLICE_MAX_CSIZE];  /* value of current index  */
}Islice;
/* END_CODE */

/* DOC_CODE Istack structure */
/* An Istack is an array of pointers to Islice structures, plus a size */
typedef struct MRCvolume
{
  struct MRCslice **vol;
  b3dInt32           zsize;
}Istack;
/* END_CODE */

/* 2/3/07: removed VOLdata union and BL3DFSvolume (4D) volume structure */

#ifdef __cplusplus
extern "C" {
#endif

  /***************************************************************************/
  /* library functions                                                       */
  Islice *sliceCreate (int xsize, int ysize, int mode);
  int     sliceInit   (Islice *s, int xsize, int ysize, int mode, void *data);
  void    sliceFree   (Islice *s);
  void    sliceClear  (Islice *sl, Ival val);
  int     sliceModeIfReal(int mrcMode);
  int     sliceGetXSize(Islice *slice);
  int     sliceGetYSize(Islice *slice);
  int     sliceGetVal (Islice *s, int x, int y, Ival val);
  float   sliceGetPixelMagnitude(Islice *s, int x, int y);
  float   sliceGetValMagnitude(Ival val, int mode);
  int     slicePutVal (Islice *s, int x, int y, Ival val);
  int     sliceNewMode(Islice *s, int mode);
  int     sliceFloat(Islice *slice);
  int     sliceComplexFloat(Islice *slice);
  int     sliceMMM(Islice *slice);
  Islice *mrc_slice_getvol(Istack *v, int sno, char axis);
  int     mrc_slice_putvol(Istack *v, Islice *s, int sno, char axis);
  int     sliceMode(char *mst);
  int     corr_conj(float *g, float *h, int size);

  int     sliceAddConst(Islice *slice, Ival c);
  int     sliceMultConst(Islice *slice, Ival c);
  int     mrc_slice_valscale(Islice *s, double scale);
  int     mrc_slice_lie(Islice *sin, double in, double alpha);

  Islice *sliceBox(Islice *sl, int llx, int lly, int urx, int ury);
  int     sliceBoxIn(Islice *sl, int llx, int lly, int urx, int ury);
  int     sliceResizeIn(Islice *sl, int x, int y);
  Islice *mrc_slice_resize(Islice *slin, int nx, int ny);
  int     sliceMirror(Islice *s, char axis);
  int     sliceWrapFFTLines(Islice *s);
  int     sliceReduceMirroredFFT(Islice *s);

  int     sliceWriteMRCfile(char *filename, Islice *slice);
  Islice *sliceReadMRC(struct MRCheader *hin, int sno, char axis);
  Islice *sliceReadSubm(struct MRCheader *hin, int sno, char axis,
                        int s1, int s2, int c1, int c2);
  Islice *sliceReadFloat(MrcHeader *hin, int slice);

  Islice *sliceGradient(Islice *sin);
  int     mrc_bandpass_filter(Islice *sin, double low, double high);
  Islice *slice_mat_filter(Islice *sin, float *mat, int dim);
  void   mrc_slice_mat_getimat(Islice *sin, int x, int y, int dim, float *mat);
  float  mrc_slice_mat_mult(float *m1, float *m2, int dim);
  Islice *mrc_slice_translate(Islice *sin, double dx, double dy,
                              int xsize, int ysize);
  Islice *mrc_slice_zoom(Islice *sin, double xz, double yz, 
                         int xsize, int ysize, double cx, double cy);
  int     mrc_slice_zooms(Islice *sin, Islice *sout, double xz, double yz,
                          double cx,    double cy);
  Islice *mrc_slice_rotate(Islice *slin, double angle, int xsize, int ysize,
                           double cx, double cy);
  int      mrc_slice_rotates(Islice *slin, Islice *sout,
                        double angle, double cx, double cy);
  void    sliceQuadInterpolate(Islice *sl, double x, double y, Ival val);


  /***************************************************************************/
  /* unused/undocumented functions */

  Islice *mrc_slice_real(Islice *sin);
  int mrc_vol_wrap(Istack *v);
  int mrc_slice_wrap(Islice *s);
  int mrc_slice_lie_img(Islice *sin, Islice *mask, double alpha);     


#ifdef __cplusplus
}
#endif


/* macro functions */
/* no error checking. */
#define mrc_slice_create sliceCreate
#define mrc_slice_calcmmm sliceMMM
#define sliceGetComplexFloatVal(s,x,y,val) memcpy((val), ((s)->data.f + ( 2 * ((x) + ((y) * (s)->xsize)))), 8)
#define slicePutComplexFloatVal(s,x,y,val) memcpy(((s)->data.f + ( 2 * ((x) + ((y) * (s)->xsize)))), (val), 8)
#define slicePutFloatVal(s,x,y,val) ((s)->data.f[ (x) + ((y) + ((s)->xsize))]=(val))



#endif /* islice.h */
