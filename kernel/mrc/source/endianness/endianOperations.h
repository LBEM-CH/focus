#ifndef ENDIANOPERATIONS_H
#define ENDIANOPERATIONS_H

#include <iostream>
#include <mrcImage.h>

void byteSwap(void *data, int size);
bool oppositeEndian(char *fileName);
bool byteSwap(char *fileName);

#endif 
