/*   pip_fwrap.c  -   Fortran wrapper for Pip function calls
 *
 *   Copyright (C) 2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 *   Log at end
 */                                                                           


#include <stdlib.h>
#include <string.h>
#include "imodconfig.h"
#include "b3dutil.h"
#include "parse_params.h"

#ifdef F77FUNCAP
#define pipinitialize PIPINITIALIZE
#define pipexitonerrorfw PIPEXITONERRORFW
#define pipaddoption PIPADDOPTION
#define pipnextarg PIPNEXTARG
#define pipnumberofargs PIPNUMBEROFARGS
#define pipgetnonoptionarg PIPGETNONOPTIONARG
#define pipgetstring PIPGETSTRING
#define pipgetinteger PIPGETINTEGER
#define pipgettwointegers PIPGETTWOINTEGERS
#define pipgettwofloats PIPGETTWOFLOATS
#define pipgetthreeintegers PIPGETTHREEINTEGERS
#define pipgetthreefloats PIPGETTHREEFLOATS
#define pipgetfloat PIPGETFLOAT
#define pipgetboolean PIPGETBOOLEAN
#define pipgetintegerarray PIPGETINTEGERARRAY
#define pipgetfloatarray PIPGETFLOATARRAY
#define pipprinthelp PIPPRINTHELP
#define pipprintentries PIPPRINTENTRIES
#define pipgeterror PIPGETERROR
#define pipseterror PIPSETERROR
#define pipnumberofentries PIPNUMBEROFENTRIES
#define pipdone PIPDONE
#define pipparseinput PIPPARSEINPUT
#define pipallowcommadefaults PIPALLOWCOMMADEFAULTS
#define pipreadoptionfile PIPREADOPTIONFILE
#define pipsetmanpageoutput PIPSETMANPAGEOUTPUT
#define pipenableentryoutput PIPENABLEENTRYOUTPUT 
#define pipsetspecialflags PIPSETSPECIALFLAGS
#define pipreadstdinifset PIPREADSTDINIFSET
#define pipsetlinkedoption PIPSETLINKEDOPTION
#define piplinkedindex PIPLINKEDINDEX
#define pipexit PIPEXIT
#else
#define pipinitialize pipinitialize_
#define pipexitonerrorfw pipexitonerrorfw_
#define pipaddoption pipaddoption_
#define pipnextarg pipnextarg_
#define pipnumberofargs pipnumberofargs_
#define pipgetnonoptionarg pipgetnonoptionarg_
#define pipgetstring pipgetstring_
#define pipgetinteger pipgetinteger_
#define pipgettwointegers pipgettwointegers_
#define pipgettwofloats pipgettwofloats_
#define pipgetthreeintegers pipgetthreeintegers_
#define pipgetthreefloats pipgetthreefloats_
#define pipgetfloat pipgetfloat_
#define pipgetboolean pipgetboolean_
#define pipgetintegerarray pipgetintegerarray_
#define pipgetfloatarray pipgetfloatarray_
#define pipprinthelp pipprinthelp_
#define pipprintentries pipprintentries_
#define pipgeterror pipgeterror_
#define pipseterror pipseterror_
#define pipnumberofentries pipnumberofentries_
#define pipdone pipdone_
#define pipparseinput pipparseinput_
#define pipallowcommadefaults pipallowcommadefaults_
#define pipreadoptionfile pipreadoptionfile_
#define pipsetmanpageoutput pipsetmanpageoutput_
#define pipenableentryoutput pipenableentryoutput_
#define pipsetspecialflags pipsetspecialflags_
#define pipreadstdinifset pipreadstdinifset_
#define pipsetlinkedoption pipsetlinkedoption_
#define piplinkedindex piplinkedindex_
#define pipexit pipexit_
#endif

static char *pipf2cstr(char *str, int strSize);


int pipinitialize(int *numOptions)
{
  return PipInitialize(*numOptions);
}

/* The real pipexitonerror is a fortran routine that sets the fortran exit prefix too */
int pipexitonerrorfw(int *useStdErr, char *prefix, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(prefix, stringSize)))
    return -1;
  err = PipExitOnError(*useStdErr, cStr);
  free(cStr);
  return err;
}

/* This is here for the Fortran exitError to exit via, due to exit status being lost
   when exiting tilt on Windows */
void pipexit(int *val)
{
  exit(*val);
}

void pipallowcommadefaults(int *val)
{
  PipAllowCommaDefaults(*val);
}

void pipsetmanpageoutput(int *val)
{
  PipSetManpageOutput(*val);
}

void pipenableentryoutput(int *val)
{
  PipEnableEntryOutput(*val);
}

void pipsetspecialflags(int *inCase, int *inDone, int *inStd, int *inLines,
                        int *inAbbrevs)
{
  PipSetSpecialFlags(*inCase, *inDone, *inStd, *inLines, *inAbbrevs);
}

int pipreadstdinifset()
{
  return PipReadStdinIfSet();
}

int pipreadoptionfile(char *progName, int *helpLevel, int *localDir, 
                      int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(progName, stringSize)))
    return -1;
  err = PipReadOptionFile(cStr, *helpLevel, *localDir);
  free(cStr);
  return err;
}

int pipaddoption(char *optionString, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(optionString, stringSize)))
    return -1;
  err = PipAddOption(cStr);
  free(cStr);
  return err;
}

int pipnextarg(char *argString, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(argString, stringSize)))
    return -1;
  err = PipNextArg(cStr);
  free(cStr);
  return err;
}

void pipnumberofargs(int *numOptArgs, int *numNonOptArgs)
{
  PipNumberOfArgs(numOptArgs, numNonOptArgs);
}

int pipgetnonoptionarg(int *argNo, char *arg, int stringSize)
{
  char *argPtr = NULL;
  int err;
  err = PipGetNonOptionArg(*argNo - 1, &argPtr);
  if (!err && c2fString(argPtr, arg, stringSize)) {
    PipSetError("Non-option argument too long for character variable");
    err = -1;
  }

  /* Are we supposed to free it? */
  if (argPtr)
    free(argPtr);
  return err;
}

int pipgetstring(char *option, char *string, int optionSize, int stringSize)
{
  char *strPtr = NULL;
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  
  err = PipGetString(cStr, &strPtr);
  if (!err && c2fString(strPtr, string, stringSize)) {
    PipSetError("In PipGetString, string is too long for character variable");
    err = -1;
  }

  /* Are we supposed to free it? */
  if (strPtr)
    free(strPtr);
  free(cStr);
  return err;
}

int pipgetinteger(char *option, int *val, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetInteger(cStr, val);
  free(cStr);
  return err;
}

int pipgetfloat(char *option, float *val, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetFloat(cStr, val);
  free(cStr);
  return err;
}

int pipgettwointegers(char *option, int *val1, int *val2, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetTwoIntegers(cStr, val1, val2);
  free(cStr);
  return err;
}

int pipgettwofloats(char *option, float *val1, float *val2, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetTwoFloats(cStr, val1, val2);
  free(cStr);
  return err;
}

int pipgetthreeintegers(char *option, int *val1, int *val2, int *val3,
                        int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetThreeIntegers(cStr, val1, val2, val3);
  free(cStr);
  return err;
}

int pipgetthreefloats(char *option, float *val1, float *val2, float *val3,
                      int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetThreeFloats(cStr, val1, val2, val3);
  free(cStr);
  return err;
}

int pipgetboolean(char *option, int *val, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetBoolean(cStr, val);
  free(cStr);
  return err;
}

int pipgetintegerarray(char *option, int *array, int *numToGet, int *arraySize,
                       int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetIntegerArray(cStr, array, numToGet, *arraySize);
  free(cStr);
  return err;
}

int pipgetfloatarray(char *option, float *array, int *numToGet, int *arraySize,
                     int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipGetFloatArray(cStr, array, numToGet, *arraySize);
  free(cStr);
  return err;
}

int pipprinthelp(char *string, int *useStdErr, int *inputFiles, 
                 int *outputFiles, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(string, stringSize))) {
    PipSetError("Memory error in pipgethelp_");
    return -1;
  }
  err = PipPrintHelp(cStr, *useStdErr, *inputFiles, *outputFiles);
  free(cStr);
  return err;
}

void pipprintentries()
{
  PipPrintEntries();
}

int pipgeterror(char *errString, int stringSize)
{
  char *strPtr = NULL;
  int err;

  /* Set up string as null, and with a null at end in case it is too long */
  err = PipGetError(&strPtr);
  if (!strPtr)
    return -1;

  err = c2fString(strPtr, errString, stringSize);

  /* Are we supposed to free it? */
  free(strPtr);
  return err;
}

int pipseterror(char *errString, int stringSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(errString, stringSize))) {
    PipSetError("Memory error in pipseterror_");
    return -1;
  }
  err = PipSetError(cStr);
  free(cStr);
  return err;
}

int pipsetlinkedoption(char *option, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize))) {
    PipSetError("Memory error in pipsetlinkedoption_");
    return -1;
  }
  err = PipSetLinkedOption(cStr);
  free(cStr);
  return err;
}

int piplinkedindex(char *option, int *index, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipLinkedIndex(cStr, index);
  (*index)++;
  free(cStr);
  return err;
}


int pipnumberofentries(char *option, int *numEntries, int optionSize)
{
  char *cStr;
  int err;
  if (!(cStr = pipf2cstr(option, optionSize)))
    return -1;
  err = PipNumberOfEntries(cStr, numEntries);
  free(cStr);
  return err;
}

void pipdone(void)
{
  PipDone();
}

/* Create a C string with a copy of a Fortran string */
static char *pipf2cstr(char *str, int strSize)
{
  char *newStr = f2cString(str, strSize);
  if (!newStr)
    PipSetError("Memory error converting string from Fortran to C");
  return newStr;
}

/*

$Log$
Revision 1.6  2011/06/16 15:11:13  mast
linked options

Revision 1.5  2011/05/27 04:31:24  mast
renamed pipexitonerror since there is now a higher level wrapper

Revision 1.4  2011/02/10 04:38:14  mast
Unused variable cleanup

Revision 1.3  2009/12/04 21:32:32  mast
Fixed define

Revision 1.2  2009/12/04 20:27:42  mast
Added new functions for printing entries

Revision 1.1  2007/09/20 02:43:08  mast
Moved to new library

Revision 3.7  2007/06/22 05:01:38  mast
Chnages for special flags

Revision 3.6  2005/02/11 01:42:34  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.5  2003/10/24 03:02:14  mast
move routines to new b3dutil file

Revision 3.4  2003/10/08 17:20:55  mast
New functions for autodoc files

Revision 3.3  2003/08/08 16:22:26  mast
Add functiond for getting two numbers

Revision 3.2  2003/06/10 23:21:54  mast
Avoid freeing strings that were never allocated

Revision 3.1  2003/06/05 00:24:02  mast
Addition to IMOD

*/
