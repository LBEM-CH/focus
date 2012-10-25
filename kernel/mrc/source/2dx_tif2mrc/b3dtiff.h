/*  IMOD VERSION 2.02
 *
 *  tiff.h
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

#ifndef __TIFF_H__
#define __TIFF_H__

#include "imodconfig.h"
#include "mrcfiles.h"
#include "iimage.h"

#define TIFFVERSION 42
#define TIFFENTRIES  7

/* The most significant byte is first. */
#define BIGENDIAN    0x4d4d

/* bytes are swapped, you can tell I use mostly big endian machines. :) */
#define LTLENDIAN    0x4949

/* Format for this machine */
#ifdef __vms
#define M_BYTEORDER  LTLENDIAN
#else
#ifdef B3D_LITTLE_ENDIAN
#define M_BYTEORDER  LTLENDIAN
#else
#define M_BYTEORDER  BIGENDIAN
#endif
#endif

/* field definitions */

#define F_DATATYPE      255
#define F_IMAGWIDTH     256
#define F_IMAGLENGTH    257
#define F_BITSPERPIX    258
#define F_PHOTOINTER    262
#define F_DATAOFFSET    273
#define F_STRIPBYTES    279

#define F_IMINFOOFFSET  1000

#define WIDTHINDEX     1
#define LENGTHINDEX    2
#define OFFSETINDEX    5
#define IMINFOINDEX    7

#define BYTETYPE  1
#define ASCIITYPE 2
#define SHORTTYPE 3
#define LONGTYPE  4
#define RATTYPE   5

#define BYTESZ    1
#define ASCIIS    1
#define SHORTSZ   2
#define LONGSZ    4
#define RATSZ     8

#define ENTRYSZ   12

#define MAG  0
#define MESH 1
#define DIFF 2
#define SA   3

typedef struct Tiff_header {
     short byteorder;
     short version;
     int  firstIFDoffset;
} Tf_header;

typedef struct Tiff_entry {
     short  tagfield;
     short  ftype;
     int   length;
     int   value;
} Tf_entry;

typedef struct Image_info {
     short func;          /* MAG = 0, MESH = 1, DIFF = 2, SA = 3     */
     short mag;           /* if (MAG) divide by 1000 else just value */
     short tilt;
     int  date;          /* seconds since Jan 1, 1970 00:00:00 GMT */
     char  comment[128];
     char  extra[128];
} Im_info;

typedef struct Tiff_info {
     Tf_header     header;
     short         numentries;
     Tf_entry      directory[TIFFENTRIES];
     int          nextIFD;
     Im_info       imageinfo;
     ImodImageFile *iifile;
     FILE          *fp;
     unsigned char *data;
     int          nstrip;     /* number of strips */
     int          *stripoff;
     int          *stripsize;
     int          width, length;
     int          rows_per_strip;
     int          strip_pos;
     int          strip_byte_counts;
     int          BitsPerSample;
     int           PhotometricInterpretation;
     int           mode;
} Tf_info;



unsigned int tiffFirstIFD(FILE *fp);
int tiffIFDNumber(FILE *fp);
unsigned int tiffIFD(FILE *fp, int section);
 
int isit_tiff(FILE *fp);
unsigned char *tiff_read_mrc(FILE *fp, struct MRCheader *hdata);
unsigned char *tiff_read_section(FILE *fp, Tf_info *tiff, int section);    

unsigned char *tiff_read_file(FILE *fp, Tf_info *tiff);
int tiff_open_file(char *filename, char *mode, Tf_info *tiff, int anyTifPixel);
     void tiff_close_file(Tf_info *tiff);

int read_tiffheader(FILE *fp, Tf_header *header);
int read_tiffentries(FILE *fp, Tf_info *tiff);
int read_barf_tiff(FILE *tif_fp, unsigned char *pixels);

int tiff_write_image(FILE *fout, int xsize, int ysize, int mode,
                     unsigned char *pixels, unsigned int *ifdOffset, 
                     unsigned int *dataOffset, float dmin, float dmax);
void tiff_write_entry(short tag, short type,
                      int length, unsigned int offset, FILE *fout);

#endif /* !__TIFF_H__ */

