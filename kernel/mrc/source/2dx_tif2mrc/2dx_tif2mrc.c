/*
 *  tif2mrc -- Convert TIFF image files to MRC image files.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2010 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mrcc.h"
#include "b3dtiff.h"
#include "b3dutil.h"
#include "parse_params.h"
#include "cfsemshare.h"
#include "hvemtypes.h"
#include "iimage.h"
#include "imodconfig.h"
#include "mrcfiles.h"
#include "mrcslice.h"

static float minmaxmean(unsigned char *tifdata, int mode, int unsign, 
                        int divide, int xsize, int ysize, int *min, int *max);
static void convertrgb(unsigned char *tifdata, int xsize, int ysize, int ntsc);
static void expandIndexToRGB(unsigned char **datap, ImodImageFile *iifile, 
                              int section);
static void convertLongToFloat(unsigned char *tifdata, ImodImageFile *iifile);
static void manageMode(Tf_info *tiff, int keepUshort, int forceSigned, 
                       int makegray, int *pixSize, int *mode);
static int manageTVIPSdata(ImodImageFile *iifile, char *label, float *tiltAngle);

int main( int argc, char *argv[])
{

  FILE *bgfp  = NULL;
  FILE *tiffp = NULL;
  FILE *mrcfp = NULL;

  Tf_info tiff;
  struct MRCheader hdata;

  int mode = 0, pixSize = 0;
  unsigned char *bgdata;
  unsigned char *tifdata;
  int min, max;
  int iarg, k, tmpdata;
  size_t x, xdo;
  int bg = FALSE;
  int makegray = FALSE;
  int fillEntered = FALSE;
  int unsign = 0;
  int divide = 0;
  int keepUshort = 0;
  int forceSigned = 0;
  int readFirst = 0;
  int useNTSC = 0;
  int anyTifPixel = 0;
  int pixelEntered = 0;
  int xsize, ysize;
  int mrcxsize = 0, mrcysize = 0, mrcsizeset;
  float userFill, fillVal, mean, tmean, tiltAngle, pixelSize = 1., yPixelSize = 1.;
  float chunkCriterion = 100.;
  short *sptr;
  short *bgshort;
  int bgBits, bgxsize, bgysize, xoffset, yoffset, y, ydo;
  int doChunks, chunk, numChunks, linesPerChunk, nlines, linesDone;
  unsigned char *fillPtr;
  unsigned char byteFill[3];
  short shortFill;
  unsigned short ushortFill;
  char label[MRC_LABEL_SIZE];
  char *tiltFile = NULL;
  FILE *tiltfp;
  char *openmode = "rb";
  char *bgfile;
  char *progname = "2dx_tif2mrc";
  char prefix[100];
  sprintf(prefix, "\nERROR: %s - ", progname);
  setExitPrefix(prefix);

  xsize = 0;
  ysize = 0;
  mean = 0;
  min = 100000;
  max = -100000;
  label[0] = 0x00;

  if (argc < 3){
    printf("2dx_tif2mrc");
    printf("Usage: %s [options] <tiff files...> <mrcfile>\n" , progname);
    printf("Options:\n");
    printf("\t-g      Convert 24-bit RGB to 8-bit grayscale\n");
    printf("\t-G      Convert 24-bit RGB to 8-bit grayscale with NTSC"
            " scaling\n");
    printf("\t-u      Convert unsigned 16-bit values by "
            "subtracting 32768\n");
    printf("\t-d      Convert unsigned 16-bit values by "
            "dividing by 2\n");
    printf("\t-k      Keep unsigned 16-bit values; store in unsigned "
            "integer mode\n");
    printf("\t-s      Store as signed integers (mode 1) even if data "
            "are unsigned\n");
    printf("\t-p  #   Set pixel spacing in MRC header to given #\n");
    printf("\t-P      Set pixel spacing in MRC header from resolution in TIFF file\n");
    printf("\t-T file Output tilt angles from TVIPS input files to given file\n");
    printf("\t-f      Read only first image of multi-page file\n");
    printf("\t-o x,y  Set output file size in X and Y\n");
    printf("\t-F  #   Set value to fill areas with no image data to given #\n");
    printf("\t-b file Background subtract image in given file\n");
    printf("\t-t #    Set criterion in megabytes for reading files in "
           "chunks\n");
    printf("\t-m      Turn off file-to-memory mapping in libtiff\n");

    exit(3);
  }

  for (iarg = 1; iarg < argc - 1 ; iarg++){
    if (argv[iarg][0] == '-'){
      switch (argv[iarg][1]){
         
      case 'h': /* help */
        break;

      case 'g': /* convert rgb to gray scale */
        makegray = TRUE;
        break;
 
      case 'G': /* convert rgb to gray scale */
        makegray = TRUE;
        useNTSC = 1;
        break;
 
      case 'u': /* treat ints as unsigned */
        unsign = 1;
        break;
 
      case 'd': /* treat ints as unsigned and divide by 2*/
        divide = 1;
        break;
 
      case 'k': /* save as unsigned */
        keepUshort = 1;
        break;
 
      case 's': /* save unsigned as signed */
        forceSigned = 1;
        break;
 
      case 'p': /* Insert pixel size in header */
        pixelSize = atof(argv[++iarg]);
        if (pixelSize <= 0.)
          pixelSize = 1.;
        else
          pixelEntered = 1;
        yPixelSize = pixelSize;
        break;

      case 'P': /* Use resolution from TIFF file regardless of value */
        anyTifPixel = 1;
        break;

      case 'f': /* read only first image */
        readFirst = 1;
        break;
 
      case 'F': /* Define fill value */
        userFill = atof(argv[++iarg]);
        fillEntered = TRUE;
        break;

      case 'b':
        bgfile = strdup(argv[++iarg]);
        /*  bgfp = fopen(argv[++iarg], "rb"); */
        bg = TRUE;
        break;

      case 'o': /* Set output size */
        sscanf(argv[++iarg], "%d%*c%d", &mrcxsize, &mrcysize);
        break;

      case 'm':
        tiffSetMapping(0);
        break;

      case 't':
        chunkCriterion = atof(argv[++iarg]);
        break;

      case 'T':
        tiltFile = strdup(argv[++iarg]);

      default:
        break;
      }
      
    }
    else
      break;
       
  }
  if ( (argc - 1) < (iarg + 1))
    exitError("Argument error: no output file specified");

  if (divide + unsign + keepUshort + forceSigned > 1)
    exitError("You must select only one of -u, -d, -k, or -s.");
  if (divide)
    unsign = 1;
  if (unsign)
    forceSigned = 1;
  tiffFilterWarnings();
  chunkCriterion *= 1024 * 1024;
  if (pixelEntered && anyTifPixel)
    exitError("You cannot enter both -p and -P");
  if (tiltFile) {
    imodBackupFile(tiltFile);
    tiltfp = fopen(tiltFile, "w");
    if (!tiltfp)
      exitError("Opening tilt angle file %s", tiltFile);
  }

  if (iarg == (argc - 2) && !readFirst){

    /* check for multi-paged tiff file. */
    /* Open the TIFF file. */
    int tiffPages;

    if (tiff_open_file(argv[iarg], openmode, &tiff, anyTifPixel))
      exitError("Couldn't open %s.", argv[iarg]);
       
    tiffp = tiff.fp;
    if (tiff.iifile) {
      tiffPages = tiff.iifile->nz;
    } else {
      tiffPages = tiffIFDNumber(tiffp);
    }
    if (tiffPages > 1) {

      int section = 0;
        
      printf("Reading multi-paged TIFF file.\n");

      if (bg)
        exitError("Background subtraction not supported for "
                "multi-paged images.");

      if (mrcxsize) 
        printf("Warning: output file size option ignored for "
                "multi-paged file\n");

      if (!tiff.iifile) {
        read_tiffheader(tiffp, &(tiff.header));
        rewind(tiffp);
        fread(&(tiff.header.byteorder), 2, 1, tiffp);
         
        tiff.header.firstIFDoffset = tiffFirstIFD(tiffp);
        rewind(tiffp);
        read_tiffentries(tiffp, &tiff);
      }

      if (!getenv("IMOD_NO_IMAGE_BACKUP") && imodBackupFile(argv[argc - 1]))
        exitError("Couldn't create backup file");
      mrcfp = fopen(argv[argc - 1], "wb");
      if (!mrcfp){
        perror("tif2mrc");
        exitError("Opening %s\n", argv[argc - 1]);
      }

      xsize = tiff.directory[WIDTHINDEX].value;
      ysize = tiff.directory[LENGTHINDEX].value;
      mrc_head_new(&hdata, xsize, ysize, tiffPages, mode);
      mrc_head_write(mrcfp, &hdata);
      manageMode(&tiff, keepUshort, forceSigned, makegray, &pixSize, &mode);

      printf("Converting %d images size %d x %d\n", 
             tiffPages, xsize, ysize);

      for (section = 0; section < tiffPages; section++) {
          
        tifdata = (unsigned char *)tiff_read_section
          (tiffp, &tiff, section);

        if (!tifdata)
          exitError("Failed to get image data for section %d", section);

        if (tiff.PhotometricInterpretation == 3)
          expandIndexToRGB(&tifdata, tiff.iifile, section);

        /* convert RGB to gray scale */
        if (tiff.PhotometricInterpretation / 2 == 1 && makegray)
          convertrgb(tifdata, xsize, ysize, useNTSC);

        /* Convert long ints to floats */
        convertLongToFloat(tifdata, tiff.iifile);

         
        mean += minmaxmean(tifdata, mode, unsign, divide, xsize, 
                           ysize, &min, &max);

        mrc_big_seek( mrcfp, 1024, section * xsize, ysize * pixSize, SEEK_SET);
     
        if (!mode && hdata.bytesSigned)
          b3dShiftBytes(tifdata, (char *)tifdata, xsize, ysize, 1, 1);
        b3dFwrite(tifdata, pixSize * xsize, ysize, mrcfp);
          
        free(tifdata);
      }
      /* write more info to mrc header. 1/17/04 eliminate unneeded rewind */
      if (tiff.iifile && !pixelEntered) {
        pixelSize = tiff.iifile->xscale;
        yPixelSize = tiff.iifile->yscale;
      }
      hdata.nx = xsize;
      hdata.ny = ysize;
      hdata.mx = hdata.nx;
      hdata.my = hdata.ny;
      hdata.mz = hdata.nz;
      hdata.xlen = hdata.nx * pixelSize;
      hdata.ylen = hdata.ny * yPixelSize;
      hdata.zlen = hdata.nz * pixelSize;
      if (mode == MRC_MODE_RGB) {
        hdata.amax = 255;
        hdata.amean = 128.0;
        hdata.amin = 0;
      } else {
        hdata.amax = max;
        hdata.amean = mean / hdata.nz;
        hdata.amin = min;
        printf("Min = %d, Max = %d, Mean = %g\n", min, max, hdata.amean);
      }
      hdata.mode = mode;
      mrc_head_label(&hdata, "tif2mrc: Converted to mrc format.");
      mrc_head_write(mrcfp, &hdata);

      /* cleanup */
      fclose(mrcfp);
      exit(0);

    }
    tiff_close_file(&tiff);
  }
  
  /* read in bg file */
  if (bg){
    if (tiff_open_file(bgfile, openmode, &tiff, anyTifPixel))
      exitError("Couldn't open %s.", bgfile);
    bgfp = tiff.fp;
    bgdata = (unsigned char *)tiff_read_file(bgfp, &tiff);
    if (!bgdata)
      exitError("Reading %s.", bgfile);
    bgBits = tiff.BitsPerSample;
    if ((bgBits != 8 && bgBits !=16) || tiff.PhotometricInterpretation >= 2)
      exitError("Background file must be 8 or 16-bit grayscale");

    bgxsize = tiff.directory[WIDTHINDEX].value;
    bgysize = tiff.directory[LENGTHINDEX].value;

    if (bgBits == 8) {
      max = 0;
      min = 255;
        
      for (y = 0; y < bgysize; y++){
        for (x = 0; x < bgxsize; x++){
          tmpdata = bgdata[x + (y * bgxsize)];
          if (tmpdata > max)
            max = tmpdata;
          if (tmpdata < min)
            min = tmpdata;
        }
      }
       
      for (y = 0; y < bgysize; y++){
        for (x = 0; x < bgxsize; x++){
          bgdata[x + (y * bgxsize)] = max - 
            bgdata[x + (y * bgxsize)];
         
        }
      }
    }

    tiff_close_file(&tiff);
  }


  /* Write out mrcheader */
  if (!getenv("IMOD_NO_IMAGE_BACKUP") && imodBackupFile(argv[argc - 1]))
    exitError("Couldn't create backup file");
  mrcfp = fopen(argv[argc - 1], "wb");
  if (!mrcfp){
    perror("tif2mrc");
    exitError("Opening %s", argv[argc - 1]);
  }
  mrc_head_new(&hdata, xsize, ysize, argc - iarg - 1, mode);
  mrc_head_write(mrcfp, &hdata);

  mrcsizeset = iarg;

  /* Loop through all the tiff files adding them to the MRC stack. */
  for (; iarg < argc - 1 ; iarg++){
       
    /* Open the TIFF file. */
    if (tiff_open_file(argv[iarg], openmode, &tiff, anyTifPixel))
      exitError("Couldn't open %s.", argv[iarg]);
    printf("Opening %s for input\n", argv[iarg]);
    fflush(stdout);
    tiffp = tiff.fp;
    k = manageTVIPSdata(tiff.iifile, label, &tiltAngle);
    if (tiltFile) {
      if (k)
        exitError("There is no tilt angle value in this file");
      fprintf(tiltfp, "%7.2f\n", tiltAngle);
    }

    /* Decide whether to set up chunks */
    doChunks = 0;
    numChunks = 1;
    if (tiff.iifile && !bg) {
      xsize = tiff.iifile->nx;
      ysize = tiff.iifile->ny;
      k = (tiff.PhotometricInterpretation == 2) ? 3 : (tiff.BitsPerSample / 8);
      if ((!mrcxsize || (xsize == mrcxsize && ysize == mrcysize)) && 
          (double)xsize * ysize * k > chunkCriterion) {
        numChunks = 1 + (int)(((double)xsize * ysize * k) / chunkCriterion);
        doChunks = 1;
        linesPerChunk = (ysize + numChunks - 1) / numChunks;
        linesDone = 0;
        printf("Reading file in %d chunks of %d lines\n", numChunks,
               linesPerChunk);
      }
    }
     
    for (chunk =0; chunk < numChunks; chunk++) {
      nlines = 0;
      if (doChunks) {
        nlines = B3DMIN(linesPerChunk, ysize - linesDone);
        tiff.iifile->lly = linesDone;
        tiff.iifile->ury = linesDone + nlines - 1;
        linesDone += nlines;
      }
 
      /* Read in tiff file */
      tifdata = (unsigned char *)tiff_read_file(tiffp, &tiff);
      if (!tifdata)
        exitError("Reading %s.", argv[iarg]);

      xsize = tiff.directory[WIDTHINDEX].value;
      ysize = tiff.directory[LENGTHINDEX].value;
      if (!nlines)
        nlines = ysize;

      if (!chunk && mrcsizeset == iarg){
        if (!mrcxsize || !mrcysize) {
          mrcxsize = xsize;
          mrcysize = ysize;
        }
        manageMode(&tiff, keepUshort, forceSigned, makegray, &pixSize, &mode);

        /* Collect the pixel size the first time */
        if (tiff.iifile && !pixelEntered) {
          pixelSize = tiff.iifile->xscale;
          yPixelSize = tiff.iifile->yscale;
        }
      }

      if ((tiff.BitsPerSample == 16 && mode != MRC_MODE_SHORT && 
           mode != MRC_MODE_USHORT) ||
          (tiff.BitsPerSample == 32 &&  mode != MRC_MODE_FLOAT) ||
          (tiff.PhotometricInterpretation / 2 == 1 && !makegray && 
           mode != MRC_MODE_RGB) ||
          (((tiff.PhotometricInterpretation / 2 == 1 && makegray) ||
            (tiff.PhotometricInterpretation / 2 == 0 && tiff.BitsPerSample == 8)) &&
           mode != MRC_MODE_BYTE))
        exitError("All files must have the same data type.");

      if (tiff.PhotometricInterpretation == 3)
        expandIndexToRGB(&tifdata, tiff.iifile, 0);

      /* convert RGB to gray scale */
      if (tiff.PhotometricInterpretation / 2 == 1 && makegray)
        convertrgb(tifdata, xsize, nlines, useNTSC);
      
      /* Convert long ints to floats */
      convertLongToFloat(tifdata, tiff.iifile);
       
      /* Correct for bg */
      if (bg){
        if ((mode != MRC_MODE_SHORT && mode != MRC_MODE_USHORT && bgBits == 16)
            || (mode != MRC_MODE_BYTE && bgBits == 8))
          exitError("Background data must have "
                    " the same data type as the image files.");

        xdo = bgxsize < xsize ? bgxsize : xsize;
        ydo = bgysize < ysize ? bgysize : ysize;

        if (mode == MRC_MODE_BYTE) {
          for (y = 0; y < ydo; y++){
            for (x = 0; x < xdo; x++){
              tmpdata = tifdata[x + (y * xdo)] + 
                bgdata[x + (y * xdo)];
              if (tmpdata > 255)
                tmpdata = 255;
              tifdata[x + (y * xdo)] = (unsigned char)tmpdata;
            }
          }
        } else {
          sptr = (short *)tifdata;
          bgshort = (short *)bgdata;
          for (y = 0; y < ydo; y++)
            for (x = 0; x < xdo; x++)
              sptr[x + (y * xdo)] -= bgshort[x + (y * xdo)];
          
        }
      }
       
      tmean = minmaxmean(tifdata, mode, unsign, divide, xsize, nlines, &min,
                         &max);
      mean += (tmean * nlines) / ysize;

      if (!hdata.mode && hdata.bytesSigned)
        b3dShiftBytes(tifdata, (char *)tifdata, xsize, nlines, 1, 1);

      if ((xsize == mrcxsize) && (ysize == mrcysize)) {

        /* Write out mrc file */    
        b3dFwrite(tifdata , pixSize * xsize, nlines, mrcfp);

      } else {
        printf("WARNING: tif2mrc - File %s not same size.\n", argv[iarg]);
        
        /* Unequal sizes: set the fill value and pointer */
        fillVal = fillEntered ? userFill : tmean;
        switch (mode) {
        case MRC_MODE_BYTE:
          byteFill[0] = fillVal;
          if (hdata.bytesSigned)
            byteFill[0] = (unsigned char)(((int)fillVal - 128) & 255);
          fillPtr = &byteFill[0];
          break;
        case MRC_MODE_RGB:
          byteFill[0] = byteFill[1] = byteFill[2] = (fillEntered ? (int)userFill : 128);
          fillPtr = &byteFill[0];
          break;
        case MRC_MODE_SHORT:
          shortFill = fillVal;
          fillPtr = (unsigned char *)&shortFill;
          break;
        case MRC_MODE_USHORT:
          ushortFill = fillVal;
          fillPtr = (unsigned char *)&ushortFill;
          break;
        case MRC_MODE_FLOAT:
          fillPtr = (unsigned char *)&fillVal;
          break;
        }

        /* Output centered data */
        yoffset = (ysize - mrcysize) / 2;
        xoffset = (xsize - mrcxsize) / 2;
        for (y = 0; y < mrcysize; y++) {
          if (y + yoffset < 0 || y + yoffset >= ysize) {
            
            /* Do fill lines */
            for (x = 0; x < mrcxsize; x++)
              b3dFwrite(fillPtr, pixSize, 1, mrcfp);
          } else {
            
            /* Fill left edge if necessary, write data, fill right if needed */
            for (k = xoffset; k < 0; k++)
              b3dFwrite(fillPtr, pixSize, 1, mrcfp);
            xdo = pixSize * (B3DMAX(0, xoffset) + (y + yoffset) * xsize);
            b3dFwrite (&tifdata[xdo], pixSize, B3DMIN(xsize, mrcxsize), mrcfp);
            for (k = 0; k < mrcxsize - xsize + xoffset; k++)
              b3dFwrite(fillPtr, pixSize, 1, mrcfp);
          }
        }
      }
      if (tifdata)
        free (tifdata);
      tifdata = NULL;
    }

    tiff_close_file(&tiff);
  }
  
  /* write more info to mrc header. 1/17/04 eliminate unneeded rewind */
  hdata.nx = mrcxsize;
  hdata.ny = mrcysize;
  hdata.mx = hdata.nx;
  hdata.my = hdata.ny;
  hdata.mz = hdata.nz;
  hdata.xlen = hdata.nx * pixelSize;
  hdata.ylen = hdata.ny * yPixelSize;
  hdata.zlen = hdata.nz * pixelSize;
  if (mode == MRC_MODE_RGB) {
    hdata.amax = 255;
    hdata.amean = 128.0;
    hdata.amin = 0;
  } else {
    hdata.amax = max;
    hdata.amean = mean / hdata.nz;
    hdata.amin = min;
    printf("Min = %d, Max = %d, Mean = %g\n", min, max, hdata.amean);
  }
  hdata.mode = mode;
  mrc_head_label(&hdata, "tif2mrc: Converted to MRC format.");
  if (label[0] != 0x00) {
    for (k = strlen(label); k < MRC_LABEL_SIZE; k++)
      label[k] = ' ';
    memcpy(&hdata.labels[hdata.nlabl], label, MRC_LABEL_SIZE);
    hdata.nlabl++;
  }
  mrc_head_write(mrcfp, &hdata);

  /* cleanup */
  if (mrcfp)
    fclose(mrcfp);
  if (tiltFile)
    fclose(tiltfp);
  exit(0);

}

/* Figure out the mode and the bytes/pixel from various values and options */
static void manageMode(Tf_info *tiff, int keepUshort, int forceSigned, 
                       int makegray, int *pixSize, int *mode)
{
  *pixSize = 1;
  if (tiff->BitsPerSample == 16){
    *mode = MRC_MODE_SHORT;
    *pixSize = 2;
  }
  if (tiff->BitsPerSample == 32){
    *mode = MRC_MODE_FLOAT;
    *pixSize = 4;
  }

  /* Use unsigned mode either if user requested it or if the file 
     specifies unsigned and the user did not request signed */
  if (*mode == MRC_MODE_SHORT && 
      (keepUshort || (!forceSigned && tiff->iifile && 
                      tiff->iifile->type == IITYPE_USHORT)))
    *mode = MRC_MODE_USHORT;

  if (tiff->PhotometricInterpretation / 2 == 1 && !makegray){
    *mode = MRC_MODE_RGB;
    *pixSize = 3;
  }
}

/* Convert rgb to gray scale by old equal weighting or NTSC weighting */
static void convertrgb(unsigned char *tifdata, int xsize, int ysize, int ntsc)
{
  unsigned char *out, *in;
  int pixel;
  size_t x, xysize;
  float fpixel;
  in = tifdata;
  out = tifdata;
  xysize = (size_t)xsize * (size_t)ysize;
  if (ntsc) {
    for (x = 0; x < xysize; x++) {
      fpixel = *in++ * 0.3;
      fpixel += *in++ * 0.59;
      fpixel += *in++ * 0.11;
      *out++ = (int)(fpixel + 0.5f);
    }
  } else {
    for (x = 0; x < xysize; x++) {
      pixel = *in++;
      pixel += *in++;
      pixel += *in++;
      *out++ = pixel / 3;
    }
  }
}

/* Look up each index value in colormap and output r,g,b values */
static void expandIndexToRGB(unsigned char **datap, ImodImageFile *iifile, 
                              int section)
{
  unsigned char *indata = *datap;
  unsigned char *outdata, *map;
  size_t xysize, i;
  int ind;
  if (!iifile || !iifile->colormap)
    exitError("Colormap data not read in properly.");
  xysize = (size_t)iifile->nx * (size_t)iifile->ny;
  if (iifile->ury >= 0)
    xysize = (size_t)iifile->nx * (size_t)(iifile->ury + 1 - iifile->lly);
  outdata = (unsigned char *)malloc(3 * xysize);
  if (!outdata)
    exitError("Unable to allocate memory for expanding RGB data.");
  *datap = outdata;

  map = &iifile->colormap[768 * section];
  for (i = 0; i < xysize; i++) {
    ind = indata[i];
    *outdata++ = map[ind];
    *outdata++ = map[256 + ind];
    *outdata++ = map[512 + ind];
  }
}

/* Convert long int or uint to floats for now */
static void convertLongToFloat(unsigned char *tifdata, ImodImageFile *iifile)
{
  int *iptr = (int *)tifdata;
  unsigned int *uiptr = (unsigned int *)tifdata;
  float *fptr = (float *)tifdata;
  size_t i, xysize;
  if (!iifile || (iifile->type != IITYPE_UINT && iifile->type != IITYPE_INT))
    return;
  xysize = (size_t)iifile->nx * (size_t)iifile->ny;
  if (iifile->ury >= 0)
    xysize = (size_t)iifile->nx * (size_t)(iifile->ury + 1 - iifile->lly);
  if (iifile->type == IITYPE_UINT) {
    for (i = 0; i < xysize; i++)
      *fptr++ = *uiptr++;
  } else {    
    for (i = 0; i < xysize; i++)
      *fptr++ = *iptr++;
  }
}

/* Find the min/max/mean of the data for the given mode and manage subtraction/
   division for unsigned to signed conversion */
static float minmaxmean(unsigned char *tifdata, int mode, int unsign, 
                        int divide, int xsize, int ysize, int *min, int *max)
{
  double tmean = 0.;
  size_t x, xysize;
  int pixel;
  short *sptr;
  unsigned short *usptr;
  float *fptr;

  xysize = (size_t)xsize * (size_t)ysize;
  if (mode == MRC_MODE_BYTE)
    for (x = 0; x < xysize; x++) {
      pixel = tifdata[x];
      if (pixel < *min)
        *min = pixel;
      if(pixel > *max)
        *max = pixel;
      tmean += pixel;
    }

  if (mode == MRC_MODE_SHORT) {
    sptr = (short *)tifdata;

    /* DNM 11/17/01: if unsigned ints, either divide by 2 or 
       subtract 32768 to get into range of signed ints */
    if (unsign) {
      usptr = (unsigned short *)tifdata;
      if (divide) {
        for (x = 0; x < xysize; x++) {
          pixel = usptr[x];
          pixel /= 2;
          sptr[x] = pixel;
        }
      } else {
        for (x = 0; x < xysize; x++) {
          pixel = usptr[x];
          sptr[x] = pixel - 32768;
        }
      }
    }
    for (x = 0; x < xysize; x++) {
      pixel = sptr[x];
      if (pixel < *min)
        *min = pixel;
      if(pixel > *max)
        *max = pixel;
      tmean += pixel;
    }
  }
  if (mode == MRC_MODE_USHORT) {
    usptr = (unsigned short *)tifdata;
    for (x = 0; x < xysize; x++) {
      pixel = usptr[x];
      if (pixel < *min)
        *min = pixel;
      if(pixel > *max)
        *max = pixel;
      tmean += pixel;
    }
  }
  if (mode == MRC_MODE_FLOAT) {
    fptr = (float *)tifdata;
    for (x = 0; x < xysize; x++) {
      pixel = fptr[x];
      if (pixel < *min)
        *min = pixel;
      if(pixel > *max)
        *max = pixel;
      tmean += pixel;
    }
  }
  return (tmean / (xysize));
}

static int manageTVIPSdata(ImodImageFile *iifile, char *label, float *tiltAngle)
{
  float axisAngle, spotFloat;
  static float lastAxis;
  int spotInt, binning;
  static int lastSpot, lastBinning;
  static int sameSpot = -1, sameBin = -1, sameAxis = -1;
  if (!iifile || !iifile->userData || iifile->userCount < 4260 || 
      !(iifile->userFlags & IIFLAG_TVIPS_DATA))
    return 1;

  /* Fetch the items and swap them if necessary */
  memcpy(&axisAngle, &iifile->userData[3704], 4);
  memcpy(tiltAngle, &iifile->userData[3564], 4);
  memcpy(&spotFloat, &iifile->userData[3624], 4);
  memcpy(&binning, &iifile->userData[3944], 4);
  if (iifile->userFlags & IIFLAG_BYTES_SWAPPED) {
    mrc_swap_floats(&axisAngle, 1);
    mrc_swap_floats(tiltAngle, 1);
    mrc_swap_floats(&spotFloat, 1);
    mrc_swap_longs(&binning, 1);
  }
  spotInt = B3DNINT(spotFloat);

  /* Keep track of whether items have been the same every time */
  if (spotInt < 1)
    sameSpot = 0;
  if (binning < 1)
    sameBin = 0;
  if (sameSpot < 0)
    sameSpot = 1;
  else if (sameSpot > 0 && spotInt != lastSpot)
    sameSpot = 0;
  if (sameBin < 0)
    sameBin = 1;
  else if (sameBin > 0 && binning != lastBinning)
    sameBin = 0;
  if (sameAxis < 0)
    sameAxis = 1;
  else if (sameAxis > 0 && fabs((double)axisAngle - lastAxis) > 1.e-5)
    sameAxis = 0;
  lastAxis = axisAngle;
  lastSpot = spotInt;
  lastBinning = binning;

  /* Convert to angle relative to Y and print out whatever matches */
  axisAngle -= 90.;
  if (axisAngle < -180.)
    axisAngle += 360.;
  if (axisAngle > 180.)
    axisAngle -= 360.;

  if (sameAxis > 0 && sameSpot > 0 && sameBin > 0)
    sprintf(label, "    Tilt axis angle = %.1f, binning = %d  spot = %d", axisAngle, 
            binning, spotInt);
  else if (sameAxis > 0 && sameBin > 0)
    sprintf(label, "    Tilt axis angle = %.1f, binning = %d", axisAngle, binning);
  else if (sameAxis > 0)
    sprintf(label, "    Tilt axis angle = %.1f", axisAngle);
  else
    label[0] = 0x00;
  return 0;
}

