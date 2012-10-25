/*   b3dutil.h   - utility functions for getting version and copyright, 
 *                      trimming program name
 *
 *   Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 */                                                                           

#ifndef B3DUTIL_H
#define B3DUTIL_H

#include <stdio.h>

/* Include this since this include file was split off from here */
#include "cfsemshare.h"

#define MAX_IMOD_ERROR_STRING  512

#define B3DMIN(a,b) ((a) < (b) ? (a) : (b))
#define B3DMAX(a,b) ((a) > (b) ? (a) : (b))
#define B3DCLAMP(a,b,c) a = B3DMAX((b), B3DMIN((c), (a)))
#define B3DNINT(a) (int)floor((a) + 0.5)
#define B3DFREE(a) if (a) {free(a); a = NULL;}
#define B3DMALLOC(a,b) (a *)malloc((b) * sizeof(a))
#define B3DREALLOC(a,b,c) a = (b *)realloc(a, (c) * sizeof(b))

#define IMOD_MRC_STAMP 1146047817
#define WRITE_SBYTES_DEFAULT 0
#define WRITE_SBYTES_ENV_VAR "WRITE_MODE0_SIGNED"
#define READ_SBYTES_ENV_VAR "READ_MODE0_SIGNED"
#define MRC_FLAGS_SBYTES  1

/* Determinant of 3x3 matrix */
#define determ3(a1,a2,a3,b1,b2,b3,c1,c2,c3) ((a1)*(b2)*(c3) - (a1)*(b3)*(c2) +\
  (a2)*(b3)*(c1) - (a2)*(b1)*(c3) + (a3)*(b1)*(c2) - (a3)*(b2)*(c1))

#ifdef __cplusplus
extern "C" {
#endif

  int imodVersion(const char *pname);
  void imodCopyright(void);
  void imodUsageHeader(const char *pname);
  char *IMOD_DIR_or_default(int *assumed);
  char *imodProgName(const char *fullname);
  int imodBackupFile(const char *filename);
  int imodGetpid();
  void pidToStderr();
  char *f2cString(const char *str, int strSize);
  int c2fString(const char *cStr, char *fStr, int fSize);
  void b3dSetStoreError(int ival);
  void b3dError(FILE *stream, char *format, ...);
  char *b3dGetError(void);

  int b3dFseek(FILE *fp, int offset, int flag);
  size_t b3dFread(void *buf, size_t size, size_t count, FILE *fp);
  size_t b3dFwrite(void *buf, size_t size, size_t count, FILE *fp);
  void b3dRewind(FILE *fp);
  int mrc_big_seek(FILE *fp, int base, int size1, int size2, int flag);
  int mrcHugeSeek(FILE *fp, int base, int x, int y, int z, int nx, int ny, 
                  int dsize, int flag);
  int fgetline(FILE *fp, char s[],int limit);

  void b3dHeaderItemBytes(int *nflags, int *nbytes);
  void setOrClearFlags(b3dUInt32 *flags, b3dUInt32 mask, int state);
  int numberInList(int num, int *list, int nlist, int noListValue);
  unsigned char **makeLinePointers(void *array, int xsize, int ysize, int dsize);

  int b3dIMin(int narg, ...);
  int b3dIMax(int narg, ...);
  double wallTime(void);
  int b3dMilliSleep(int msecs);
  int numOMPthreads(int optimalThreads);
  int b3dOMPthreadNum();
  void overrideWriteBytes(int value);
  int writeBytesSigned();
  int readBytesSigned(int stamp, int flags, int mode, float dmin, float dmax);
  void b3dShiftBytes(unsigned char *usbuf, char *sbuf, int nx, int ny, int direction,
                     int bytesSigned);


#ifdef __cplusplus
}
#endif


#endif
