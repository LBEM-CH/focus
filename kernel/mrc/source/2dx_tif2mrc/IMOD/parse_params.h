/*   parse_params.h  -  declarations for C functions in PIP package
 *
 *   Copyright (C) 2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 */                                                                           

#ifndef PARSE_PARAMS_H
#define PARSE_PARAMS_H
// #include <stdio.h>
#define PIP_INTEGER  1
#define PIP_FLOAT    2
#ifdef __cplusplus
extern "C" {
#endif

int PipInitialize(int numOpts);
int PipExitOnError(int useStdErr, const char *prefix);
void exitError(const char *format, ...);
void setExitPrefix(const char *prefix);
int PipAddOption(const char *optionString);
int PipNextArg(const char *argString);
void PipNumberOfArgs(int *numOptArgs, int *numNonOptArgs);
int PipGetNonOptionArg(int argNo, char **arg);
int PipGetString(const char *option, char **string);
int PipGetInteger(const char *option, int *val);
int PipGetFloat(const char *option, float *val);
int PipGetBoolean(const char *option, int *val);
int PipGetTwoIntegers(const char *option, int *val1, int *val2);
int PipGetTwoFloats(const char *option, float *val1, float *val2);
int PipGetThreeIntegers(const char *option, int *val1, int *val2, int *val3);
int PipGetThreeFloats(const char *option, float *val1, float *val2, float *val3);
int PipGetIntegerArray(const char *option, int *array, int *numToGet, int arraySize);
int PipGetFloatArray(const char *option, float *array, int *numToGet, int arraySize);
int PipPrintHelp(const char *progName, int useStdErr, int inputFiles,
                 int outputFiles);
int PipGetError(char **errString);
int PipSetError(const char *errString);
int PipNumberOfEntries(const char *option, int *numEntries);
void PipDone(void);
int PipParseInput(int argc, char *argv[], const char *options[], int numOptions,
                  int *numOptArgs, int *numNonOptArgs);
int PipParseEntries(int argc, char *argv[], int *numOptArgs,
                    int *numNonOptArgs);
int PipReadOptionFile(const char *progName, int helpLevel, int localDir);
int PipMemoryError(void *ptr, const char *routine);
void PipAllowCommaDefaults(int val);
void PipSetManpageOutput(int val);
void PipEnableEntryOutput(int val);
void PipPrintEntries();
int PipSetLinkedOption(const char *option);
int PipLinkedIndex(const char *option, int *index);
void PipSetSpecialFlags(int inCase, int inDone, int inStd, int inLines, 
                        int inAbbrevs);
int PipReadStdinIfSet(void);
int PipStartsWith(const char *fullStr, const char *subStr);
int PipGetInOutFile(const char *option, int nonOptArgNo, char **filename);
void PipReadOrParseOptions(int argc, char *argv[], const char *options[], 
                           int numOpts, const char *progName, int minArgs, 
                           int numInFiles, int numOutFiles, int *numOptArgs,
                           int *numNonOptArgs, void (headerFunc)(const char *));
int PipReadNextLine(FILE *pFile, char *lineStr, int strSize, char comment, 
                    int keepComments, int inLineComments, int *firstNonWhite);
int PipGetLineOfValues(const char *option, const char *strPtr, void *array, int valType, 
                       int *numToGet, int arraySize);
#ifdef __cplusplus
}
#endif
#endif
