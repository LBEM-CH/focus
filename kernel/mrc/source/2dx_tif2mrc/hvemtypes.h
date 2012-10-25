#ifndef HVEMTYPES_H
#define HVEMTYPES_H
/*
  $Id$

  $Log$
  Revision 3.3  2003/02/27 20:20:16  mast
  Change conditional definitions of X, Y, Z to defines of b3dX,Y,Z
  
  Revision 3.2  2003/02/21 22:15:25  mast
  Include new type definitions from imodconfig, eliminate old typedefs that
  conflict with Qt, and eliminate X11 definitions
  
  Revision 3.1  2002/11/30 07:24:00  mast
  add ability to exclude X11 definitions for Qt use

*/


#include <sys/types.h>

/* Read the definitions of the b3d... data types of defined bit size */
#include "imodconfig.h"

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#define b3dX 0
#define b3dY 1
#define b3dZ 2

#include <limits.h>

#ifndef FLT_MAX 
#define FLT_MAX         1.E+37f
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif
#ifndef SEEK_END
#define SEEK_END 2
#endif

#endif /* hvemtypes.h */
