/* iimage.h - definitions and declarations for IMOD image files
 *  $Id$
 */

#ifndef IIMAGE_H
#define IIMAGE_H

#include "mrcfiles.h"
#include "mrcslice.h"
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

  /* DOC_CODE ImodImageFile definitions */
  /* Values for the file member of ImodImageFile, describing kind of file */
#define IIFILE_UNKNOWN 0
#define IIFILE_TIFF    1
#define IIFILE_MRC     2
#define IIFILE_QIMAGE  3
#define IIFILE_RAW     4

  /* Values for the format member of ImodImageFile, describing kind data */
#define IIFORMAT_LUMINANCE 0
#define IIFORMAT_RGB       1
#define IIFORMAT_RGBA      2
#define IIFORMAT_COMPLEX   3
#define IIFORMAT_COLORMAP  4

  /* Values for the type member of ImodImageFile, describing numeric type */
#define IITYPE_UBYTE   0
#define IITYPE_BYTE  1
#define IITYPE_SHORT  2
#define IITYPE_USHORT 3
#define IITYPE_INT    4
#define IITYPE_UINT   5
#define IITYPE_FLOAT  6

  /* Values for the state member of ImodImageFile, describing file state */
#define IISTATE_NOTINIT 0
#define IISTATE_PARK    1
#define IISTATE_READY   2
#define IISTATE_BUSY    4

  /* Error codes, used by check routines */
#define IIERR_BAD_CALL  -1
#define IIERR_NOT_FORMAT 1
#define IIERR_IO_ERROR   2
#define IIERR_MEMORY_ERR 3
#define IIERR_NO_SUPPORT 4

  /* Flags for userData */
#define IIFLAG_BYTES_SWAPPED   1
#define IIFLAG_TVIPS_DATA      2
/* END_CODE */

  /* DOC_CODE Raw mode codes */
  /* Yet another set of mode values, which define the order for radio buttons
     in a raw type selector dialog */
#define RAW_MODE_SBYTE         0
#define RAW_MODE_BYTE          1
#define RAW_MODE_SHORT         2
#define RAW_MODE_USHORT        3
#define RAW_MODE_FLOAT         4
#define RAW_MODE_COMPLEX_FLOAT 5
#define RAW_MODE_RGB           6
/* END_CODE */

#define IIAXIS_X 1
#define IIAXIS_Y 2
#define IIAXIS_Z 3


  /* DOC_CODE ImodImageFile structure */
  typedef struct  ImodImageFileStruct ImodImageFile;
  typedef int (*iiSectionFunc)(ImodImageFile *inFile, char *buf, int inSection);

  struct  ImodImageFileStruct
  {
    char *filename;
    char *fmode;
    FILE *fp;
    char *description;
    int   state;

    /* Data set by new and open functions. */
    int   nx, ny, nz;
    int   file;       /* Type of file, i.e. MRC, TIF... */
    int   format;     /* Kind of data represented: i.e. gray, color, complex */
    int   type;       /* Type if numerical elements, i.e. byte, etc. */
    int   mode;       /* MRC mode value */

    /* optional data to be set if input file supports it. */
    float amin, amax, amean;
    float xscale, yscale, zscale;
    float xtrans, ytrans, ztrans;
    float xrot,   yrot,   zrot;
    int   time, wave;

    /* load info: change these for loading sub sections. */
    int   llx, lly, llz, urx, ury, urz;
    float slope, offset, smin, smax;
    int   axis;
    int   mirrorFFT;   /* Return mirrored FFT when scaling to bytes */

    /* extra storage used by individual file format functions. */
    int   headerSize;
    int   sectionSkip;
    char *header;
    char *userData;
    unsigned int userFlags;  /* Flags for the userData */
    int userCount;           /* Number of bytes of userData */
    unsigned char *colormap;
    int  planesPerImage;     /* # of planes per TIFF image */
    int  contigSamples;      /* # of contiguous samples per pixel in plane */
    int  multipleSizes;      /* Flag that TIFF file has multiple sizes */
    int  rgbSamples;         /* Number of samples for RGB TIFF file */
    int  anyTiffPixSize;     /* Set non-0 to have TIFF pixel size put into [xyz]scale */

    /* Callback functions used by different file formats. */
    iiSectionFunc readSection;
    iiSectionFunc readSectionByte;
    iiSectionFunc readSectionUShort;
    iiSectionFunc writeSection;
    void (*cleanUp)(ImodImageFile *inFile);
    void (*close)(ImodImageFile *inFile);
    int (*reopen)(ImodImageFile *inFile);

  };
/* END_CODE */

  /* DOC_CODE RawImageInfo structure */
  /* A structure for passing bare-bones information about an MRC-like (raw)
     file to a routine that makes an MRC header */
  typedef struct raw_image_info {
    int type;           /* Data type, one of the RAW_MODE_* values */
    int nx, ny, nz;     /* Size of file in X, Y, Z */
    int swapBytes;      /* Whether bytes are swapped */
    int headerSize;     /* Offset to data */
    float amin, amax;   /* Data min and max, set to 0 if unknown */
    int scanMinMax;     /* Flag that scan is needed, used internally */
    int allMatch;       /* Flag that all files match, used internally */
    int sectionSkip;    /* Padding after each section - there may be no padding
                           after last section */
    int yInverted;      /* Lines are inverted in Y */
    float pixel;        /* Pixel size in Angstroms, set to 0. if unknown */
    float zPixel;       /* Pixel size in Z if different, set to 0. otherwise */
  } RawImageInfo;
/* END_CODE */

  typedef int (*IIFileCheckFunction)(ImodImageFile *);
  typedef int (*IIRawCheckFunction)(FILE *, char *, RawImageInfo *);

  void iiAddCheckFunction(IIFileCheckFunction func);
  void iiInsertCheckFunction(IIFileCheckFunction func, int index);
  void iiDeleteCheckList();
  void iiAddRawCheckFunction(IIRawCheckFunction func, const char *name);
  void iiDeleteRawCheckList();
  ImodImageFile *iiNew(void);
  ImodImageFile *iiOpen(const char *filename, const char *mode);
  int  iiReopen(ImodImageFile *inFile);
  void iiClose(ImodImageFile *inFile);
  void iiDelete(ImodImageFile *inFile);
  int  iiSetMM(ImodImageFile *inFile, float inMin, float inMax, float scaleMax);

  int iiReadSection(ImodImageFile *inFile, char *buf, int inSection);
  int iiReadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
  int iiReadSectionUShort(ImodImageFile *inFile, char *buf, int inSection);
  int iiLoadPCoord(ImodImageFile *inFile, int useMdoc, struct LoadInfo *li,
                   int nx, int ny, int nz);

  /* Create and write support. */
  int iiInit(ImodImageFile *i, int xsize, int ysize, int zsize, 
             int file, int format, int type);
  int iiWriteSection(ImodImageFile *inFile, char *buf, int inSection);

  /* Declarations for specific file types needed by other modules */
  int iiTIFFCheck(ImodImageFile *inFile);
  int iiMRCCheck(ImodImageFile *inFile);
  int iiMRCreadSection(ImodImageFile *inFile, char *buf, int inSection);
  int iiMRCreadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
  int iiMRCreadSectionUShort(ImodImageFile *inFile, char *buf, int inSection);
  int iiMRCLoadPCoord(ImodImageFile *inFile, struct LoadInfo *li, int nx,
                      int ny, int nz);
  int tiffReadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
  int tiffReadSectionUShort(ImodImageFile *inFile, char *buf, int inSection);
  int tiffReadSection(ImodImageFile *inFile, char *buf, int inSection);
  void tiffClose(ImodImageFile *inFile);
  int tiffGetField(ImodImageFile *inFile, int tag, void *value);
  int tiffGetArray(ImodImageFile *inFile, int tag, unsigned short *count, void *value);
  void tiffSuppressWarnings(void);
  void tiffSuppressErrors(void);
  void tiffFilterWarnings(void);
  int tiffOpenNew(ImodImageFile *inFile);
  int tiffWriteSection(ImodImageFile *inFile, void *buf, int compression, 
                       int inverted, int resolution, int quality);
  int tiffWriteSetup(ImodImageFile *inFile, int compression, 
                     int inverted, int resolution, int quality, 
                     int *outRows, int *outNum);
  int tiffWriteStrip(ImodImageFile *inFile, int strip, void *buf);
  void tiffWriteFinish(ImodImageFile *inFile);
  int tiffVersion(int *minor);
  int tiffReopen(ImodImageFile *inFile);
  void tiffDelete(ImodImageFile *inFile);
  void tiffSetMapping(int value);
  int iiLikeMRCCheck(ImodImageFile *inFile);
  void iiLikeMRCDelete(ImodImageFile *inFile);
  int iiSetupRawHeaders(ImodImageFile *inFile, RawImageInfo *info);
  int analyzeDM3(FILE *fp, char *filename, int dmformat, RawImageInfo *info, int *dmtype);

#ifdef __cplusplus
}
#endif

#endif
