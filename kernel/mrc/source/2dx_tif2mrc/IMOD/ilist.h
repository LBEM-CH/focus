/* 
 *  ilist.h -- Header for general list functions
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.3  2004/11/20 04:02:03  mast
Added quantum, dup, last, truncate functions

Revision 3.2  2003/10/01 05:15:28  mast
Give the structure a name to avoid including in plugins

Revision 3.1  2002/12/01 15:39:50  mast
Declare extern C if c++

*/

#ifndef ILIST_H
#define ILIST_H

#ifdef __cplusplus
extern "C" {
#endif

#define LIST_QUANTUM 1

  typedef struct ilist_struct
  {
    void *data;        /* Pointer to data */
    int   dsize;       /* Size of data element in bytes */
    int   current;     /* Current item */
    int   size;        /* Number of items on list */
    int   store;       /* Number of items space allocated for */
    int   quantum;     /* Increment when allocating more space */
  }Ilist;


  Ilist *ilistNew(int dsize, int asize);
  void   ilistDelete (Ilist *list);
  void   ilistQuantum(Ilist *list, int size);
  Ilist *ilistDup(Ilist *list);

  void  *ilistFirst(Ilist *list);
  void  *ilistNext(Ilist *list);
  void  *ilistLast(Ilist *list);
  void  *ilistItem(Ilist *list, int element);
  int    ilistSize(Ilist *list);
  void   ilistTruncate(Ilist *list, int size);

  int    ilistAppend(Ilist *list, void *data);
  void   ilistRemove(Ilist *list, int element);
  int    ilistSwap  (Ilist *list, int e1, int e2);
  int   ilistInsert(Ilist *list, void *data, int element);

  int  ilistPush(Ilist *list, void *data);
  void *ilistPop(Ilist *list);
  int  ilistFloat(Ilist *list, int element);
  void ilistShift(Ilist *list, int start, int amount);

#ifdef __cplusplus
}
#endif

#endif

