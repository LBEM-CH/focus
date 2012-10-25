/*
 *  tiff.c -- TIFF file routines.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "b3dtiff.h"

#define XSIZE 512
#define YSIZE 480
#define XVIEW 512
#define YVIEW 480

struct Image_data {
  b3dInt32 width;
  b3dInt32 length;
  b3dInt32 offset;
};

unsigned char *tiff_read_file(FILE *fp, Tf_info *tiff);


static void swap(char *ptr, unsigned size)

{
  unsigned char *begin;
  unsigned char *end;
  unsigned char tmp;
  int           i;

  if ((size % 2) != 0)
    size--;

  begin = ( unsigned char *)ptr;
  end   = ( unsigned char *)ptr + (size - 1);

  for (i = 0; i < (size/2); i++) {
    tmp = *begin;
    *begin = *end;
    *end   = tmp;

    begin++;
    end--;
  }
}

int isit_tiff(FILE *fp)
{

  unsigned short buf;
  rewind(fp);
  if (fread(&buf, sizeof(unsigned short), 1, fp) < 1)
    return(0);
  if ( (buf != 0x4949) && (buf != 0x4d4d))
    return(0);
  if (fread(&buf, sizeof(unsigned short), 1, fp) < 1)
    return(0);
  if ((buf != 0x0042) && (buf != 0x4200))
    return(0);
  return(1);
}


void tiff_write_entry(short tag, short type,
                      b3dInt32 length, b3dUInt32 offset, FILE *fout)
{
  fwrite(&tag, sizeof(short), 1, fout);
  fwrite(&type, sizeof(short), 1, fout);
  fwrite(&length, sizeof(b3dInt32), 1, fout);

  /* Looks like little endian has its advantages. */
  if ( M_BYTEORDER ==  BIGENDIAN){
    if (length == 1)
      switch(type){
      case 1:
        offset = offset << 24;
        break;
      case 3:
        offset = offset << 16;
        break;
      }
  }
  fwrite(&offset, sizeof(b3dInt32), 1, fout);
  return;
}



unsigned char *tiff_read_mrc(FILE *fp, struct MRCheader *hdata)
{
  Tf_info tiff;
  unsigned char *data;
     
  data = (unsigned char *)tiff_read_file(fp, &tiff);
  if (!data)
    return(NULL);

  mrc_head_new(hdata, 
               tiff.directory[WIDTHINDEX].value, 
               tiff.directory[LENGTHINDEX].value, 1,0);
  return(data);
}



unsigned char *tiff_read_section(FILE *fp, Tf_info *tiff, int section)
{
  size_t    data_size;
  int dpos = 0;
  int i;
  int pixSize = 1;
  int xsize, ysize;
  int realsize;
  int nleft;
  int ncopy, indto;
  char *tmpline;

  if (!tiff->iifile) {
    tiff->header.firstIFDoffset = tiffIFD(fp, section);

    fseek(fp, tiff->header.firstIFDoffset, SEEK_SET);
    fread(&(tiff->numentries), SHORTSZ, 1, fp);
         
    if (tiff->header.byteorder != M_BYTEORDER)
      swap((char *)&(tiff->numentries), SHORTSZ);

    if (!read_tiffentries(fp, tiff)){
      return(NULL);
    }
  }
     
  xsize = tiff->directory[WIDTHINDEX].value;
  ysize = tiff->directory[LENGTHINDEX].value;
  data_size = (size_t)xsize * (size_t)ysize;

  pixSize = tiff->BitsPerSample / 8;

  if (tiff->PhotometricInterpretation == 2)
    pixSize = 3;
  /* printf("xsize %d ysize %d datasize %d pixsize %d bits %d\n", xsize,ysize,
     data_size, pixSize, tiff->BitsPerSample); */

  /* add padding to data. */
  tiff->data = (unsigned char *)malloc((data_size + xsize + ysize) 
                                       * pixSize);
  if (!tiff->data)
    return(NULL);

  if (tiff->iifile) {
    if (tiffReadSection(tiff->iifile, (char *)tiff->data, section))
      return NULL;

    return (tiff->data);
  }

  /* binary image. */
  if (tiff->BitsPerSample == 1){
    char *bitdata = malloc(data_size);
    int cbyte, cbit;
    dpos = 0;
          
    for(i = 0; i < tiff->nstrip; i++){
      fseek(fp, tiff->stripoff[i], SEEK_SET);
      fread((&bitdata[dpos]), pixSize,
            tiff->stripsize[i], fp);
      dpos += tiff->stripsize[i];
    }
    dpos = 0;
          
    for(i = 0; i <data_size; i++){
      cbyte = bitdata[i/8];
      cbit = i % 8;
      if (cbyte & ((1 << 7) >> cbit)){
        tiff->data[i] = 0xff;
      }else{
        tiff->data[i] = 0x00;
      }
          
    }
    free(bitdata);
    tiff->BitsPerSample = 8;
  }else{
    nleft = xsize * ysize * pixSize;
    for(i = 0; i < tiff->nstrip; i++){
      fseek(fp, tiff->stripoff[i], SEEK_SET);
      /* DNM 11/17/01: Gatan image did not limit size of last strip */
      realsize = tiff->stripsize[i];
      if  (realsize > nleft)
        realsize = nleft;
      /* DNM 12/10/00: was pixSize, needed to be 1 because stripsize
         is in bytes regardless of pixel size */
      /* printf("%d %d %d %d %d\n",i,dpos, tiff->stripsize[i], 
         realsize, nleft); */
      fread(&(tiff->data[dpos]), 1, realsize, fp);
      /* DNM: swap the bytes if necessary */
      if (tiff->header.byteorder != M_BYTEORDER && pixSize == 2)
        mrc_swap_shorts((b3dInt16 *)&(tiff->data[dpos]),
                        realsize / 2);
      if (tiff->header.byteorder != M_BYTEORDER && pixSize == 4)
        mrc_swap_floats((b3dFloat *)&(tiff->data[dpos]),
                        realsize / 4);
      dpos += realsize;
      nleft -= realsize;
    }
  }


  /* transpose the data */
  ncopy = xsize * pixSize;
  tmpline = (char *)malloc(ncopy);
  if (!tmpline)
    return NULL;
  for (i = 0; i < ysize / 2; i++) {
    indto = ((ysize - 1) - i) * xsize * pixSize;
    memcpy(tmpline, &(tiff->data[i * xsize * pixSize]), ncopy);
    memcpy(&(tiff->data[i * xsize * pixSize]), &(tiff->data[indto]),
           ncopy);
    memcpy(&(tiff->data[indto]), tmpline, ncopy);
  }
  free(tmpline);
     
  return(tiff->data);    

}


unsigned char *tiff_read_file(FILE *fp, Tf_info *tiff)
{

  if (tiff->iifile)
    return(tiff_read_section(fp, tiff, 0));

  rewind(fp);
  if (! read_tiffheader(fp, &(tiff->header)))
    return((unsigned char *)NULL);
  return(tiff_read_section(fp, tiff, 0));

}

/* DNM: general routine to open file with the given mode */
int tiff_open_file(char *filename, char *mode, Tf_info *tiff, int anyTifPixel)
{
  /* first try to open file; return error if fails */
  tiff->fp = fopen(filename, mode);
  if (!tiff->fp)
    return 1;

  /* Then get an imodImageFile, use it to try to open through libraries */
  tiff->iifile = iiNew();
  if (tiff->iifile) {
    tiff->iifile->fp = tiff->fp;
    tiff->iifile->filename = strdup(filename);
    tiff->iifile->fmode = mode;
    tiff->iifile->anyTiffPixSize = anyTifPixel;
    if (iiTIFFCheck(tiff->iifile)) {

      /* If this fails, reset the iifile entry to indicate that, 
         and reopen the file for use by old routines */
      if (tiff->iifile->fp)
        tiff->fp = tiff->iifile->fp;
      else
        tiff->fp = fopen(filename, mode);
      if (tiff->iifile->filename)
        free(tiff->iifile->filename);
      free(tiff->iifile);
      tiff->iifile = NULL;
      if (!tiff->fp)
        return 1;
      /* This is needed to set swapping correctly */
      tiffFirstIFD(tiff->fp);
    } else {

      /* Now set up some data about the file in the tiff structure */
      tiff->BitsPerSample = 8;
      if (tiff->iifile->mode == MRC_MODE_SHORT || 
          tiff->iifile->mode == MRC_MODE_USHORT)
        tiff->BitsPerSample = 16;
      if (tiff->iifile->mode == MRC_MODE_FLOAT || 
          tiff->iifile->type == IITYPE_UINT || 
          tiff->iifile->type == IITYPE_INT)
        tiff->BitsPerSample = 32;
      tiff->PhotometricInterpretation = 
        (tiff->iifile->mode == MRC_MODE_RGB) ? 2 : 1;
      if (tiff->iifile->format == IIFORMAT_COLORMAP) 
        tiff->PhotometricInterpretation = 3;

      tiff->directory[WIDTHINDEX].value = tiff->iifile->nx;
      tiff->directory[LENGTHINDEX].value = tiff->iifile->ny;
      tiff->iifile->llx = 0;
      tiff->iifile->lly = 0;

      /* DNM 2/26/03: set upper right to -1 for later replacement */
      tiff->iifile->urx = -1;
      tiff->iifile->ury = -1;
    }
  }
  return 0;
}

void tiff_close_file(Tf_info *tiff)
{
  if (tiff->iifile) {
    tiffClose(tiff->iifile);
    iiDelete(tiff->iifile);
  } else
    fclose(tiff->fp);
}

int read_tiffheader(FILE *fp, Tf_header *header)
{

  if (fread(header, sizeof(Tf_header), 1, fp) < 1)
    return(0);

  if ((header->byteorder !=  BIGENDIAN) &&
      (header->byteorder !=  LTLENDIAN))
    return(0);


  if (header->byteorder != M_BYTEORDER){
    swap((char *)&(header->version), SHORTSZ);
    swap((char *)&(header->firstIFDoffset), LONGSZ);
  }


  header->firstIFDoffset = tiffFirstIFD(fp);

  if (header->version != TIFFVERSION)
    return(0);

  return(1);
}

static int swapData = 0;


unsigned int tiffFirstIFD(FILE *fp)
{
  unsigned short buf;
  unsigned int retval = 0;

  rewind(fp);
  if (fread(&buf, sizeof(unsigned short), 1, fp) < 1)
    return(retval);
  if ( (buf != 0x4949) && (buf != 0x4d4d))
    return(retval);

  /* DNM 12/10/00: was testing buf against 0x4d4d, needed M_BYTEORDER */
  if ( buf != M_BYTEORDER)
    swapData = 1;
  else
    swapData = 0;

  if (fread(&buf, sizeof(unsigned short), 1, fp) < 1)
    return(retval);

  if (swapData)
    swap((char *)&buf, 2);

  fread(&retval, sizeof(unsigned int), 1, fp);
  if (swapData)
    swap((char *)&retval, 4);
  return(retval);
    

}

unsigned int tiffIFD(FILE *fp, int section)
{
  unsigned int ifd = tiffFirstIFD(fp);
  int i;
  unsigned short entries;

  for(i = 0; i < section; i++){
    if (!ifd) return(0);

    fseek(fp, ifd,  SEEK_SET);
    fread(&entries, 2, 1, fp);
    if (swapData)
      swap((char *)&entries, 2);
        
    fseek(fp, (ifd + 2) + (entries * 12), SEEK_SET);
    fread(&ifd, 4, 1, fp);
    if (swapData)
      swap((char *)&ifd, 4);

  }

  return(ifd);
}



/* returns the number of IFD's */
int tiffIFDNumber(FILE *fp)
{
  unsigned short IFDentries;
  unsigned int IFDoffset = tiffFirstIFD(fp);
  int retval = 0;

  while (IFDoffset){
    fseek(fp, IFDoffset, SEEK_SET);
    fread(&IFDentries, 2, 1, fp);
    if (swapData)
      swap((char *)&IFDentries, 2);
    retval++;

    fseek(fp, (IFDoffset + 2) + (IFDentries * 12), SEEK_SET);
    fread(&IFDoffset, 4, 1, fp);
    if (swapData)
      swap((char *)&IFDoffset, 4);
  }
  return(retval);
}

int read_tiffentries(FILE *fp, Tf_info *tiff)
{
  int   i;
  unsigned short tag;
  /* DNM 8/9/01: make these unsigned per H.M. Kvasnicka */
  unsigned short type;
  b3dUInt32  len;
  b3dUInt32  value;
  int   pos;

  unsigned char numfields = 0;

  tiff->nstrip = 1;


  /* Read each TIFF Image file directory entry.
   * 
   * Each 12-byte IFD entry has the following format:
   * 
   * Bytes 0-1  contain the Tag for the field.
   * Bytes 2-3  contain the field Type.
   *            1 = BYTE, 2 = ASCII, 3 = SHORT, 4 = LONG, 5 = RATIONAL.
   * Bytes 4-7  contain the  Length of the field.
   * Bytes 8-11 contain the  Value or Value Offset.
   */

  fseek(fp, tiff->header.firstIFDoffset, SEEK_SET);
  fread(&tiff->numentries, SHORTSZ, 1, fp);
  if (swapData)
    swap((char *)&tiff->numentries, 2);
     
  if (tiff->header.byteorder != M_BYTEORDER)
    swap((char *)&tag,   SHORTSZ);
 
  for (i = 0; i < tiff->numentries; i++) {

    tag = type = len = value = 0;

    if (fread(&tag, SHORTSZ, 1, fp) < 1) {
      printf("ERROR: read_tiffentries() - reading tag");
      return(0);
    }

    if (fread(&type, SHORTSZ, 1, fp) < 1) {
      printf("ERROR: read_tiffentries() - reading type");
      return(0);
    }

    if (fread(&len, LONGSZ, 1, fp) < 1) {
      printf("ERROR: read_tiffentries() - reading length");
      return(0);
    }

    if (fread(&value, LONGSZ, 1, fp) < 1) {
      printf("ERROR: read_tiffentries() - reading value");
      return(0);
    }
          
    if (tiff->header.byteorder != M_BYTEORDER) {
      swap((char *)&tag,   SHORTSZ);
      swap((char *)&type,  SHORTSZ);
      swap((char *)&len,   LONGSZ);
      swap((char *)&value, LONGSZ);
    }
    /* DNM 12/10/00: this was done as else on the swap, but it needs to
       be done if data are big endian ? */
    if (tiff->header.byteorder == BIGENDIAN && 
        (type == SHORTTYPE) && (len < 3))
      value >>= 16;

    /*
      printf("i: %2d   ", i);
      printf("tag: %5u    ", tag);
      printf("type: %d   ", type);
      printf("len: %5d    ", len);
      printf("value: %d\n", value);
    */

    switch (tag) {

      /* NewSubfileType */
    case 254: /* subfile, LONG, 1 */
      /* Bit 0 : Reduced image. 
       * Bit 1 : single page in multipage image.
       * Bit 2 : Transparency mask.
       */
      break;
               
    case F_DATATYPE: /* 255 */
      break;
               
    case F_IMAGWIDTH: /* 256 */
      tiff->directory[WIDTHINDEX].value = value;
      tiff->width = value;
      numfields++;
      break;

    case F_IMAGLENGTH: /* 257 */
      tiff->directory[LENGTHINDEX].value = value;
      /*    im_data->length = value; */
      tiff->length = value;
      numfields++;
      break;
               
    case 258: /* BitsPerSample, SHORT, 
                 for RGB three values are given. */
      if (len == 1){
        tiff->BitsPerSample = value;
                    
        if (value == 16)
          tiff->mode = 2;
      }
      if (len == 3){
        short red,green,blue;
        pos = ftell(fp);

        fseek(fp, value, SEEK_SET);
        fread(&red, 2, 1, fp);
        fread(&green, 2, 1, fp);
        fread(&blue, 2, 1, fp);
        fseek(fp, pos, SEEK_SET);
        /* Assume red,green,blue same size*/
        tiff->BitsPerSample = red;
        tiff->mode = 16;
      }
      break;
               
    case 259: /* Compression, 1 = none */
      if (value != 1){
        char *compType;
        switch(value){
        case 2:
          compType = "CCITT 1D";
          break;
        case 3:
          compType = "Group 3 Fax";
          break;
        case 4:
          compType = "Group 4 Fax";
          break;
        case 5:
          compType = "LZW";
          break;
        case 6:
        case 7:
          compType = "JPEG";
          break;
        case 32773:
          compType = "PackBits";
          break;
        default:
          compType = "Unknown";
          break;
        }
        printf("ERROR: read_tiffentries - %s compressed tiff data "
                "not supported without tifflib.so. (%u)\n", compType, value);
        return(0);
      }
      break;
               
    case 262: /* PhotometricInterpretation */
      /* 0  (WhiteIsZero)
       * 1  (BlackIsZero)
       * 2  (RGB)
       * 3  (Palette color)
       * 4  (Transparency Mask)
       */
      tiff->PhotometricInterpretation = value;
      if (value == 3) {
        printf("ERROR: read_tiffentries -  color index data "
                "not supported without tifflib.so.\n");
        return(0);
      }
        
      break;
               

    case 266:
    case 269:
    case 270: /* ImageDescription, ASCII */
      break;

    case 273: /* Strip offsets */
      /* for each strip the location of that strip */
      tiff->strip_pos = value;
      tiff->nstrip = len;

      /*             tiff->stripoff = (long *)malloc(sizeof(long) * len);
                     if (len == 1)
                     tiff->stripoff[0] = value;
                     else{
                     pos =ftell(fp);
                     fseek(fp, value, SEEK_SET);
                     fread(tiff->stripoff, sizeof(long), len, fp);
                     fseek(fp, pos, SEEK_SET);
                     }
      */
      break;
                    
    case 274:
    case 277: /* SamplesPerPixel, SHORT, 1 */
      break;

    case 278:  /*RowsPerStrip, SHORT or LONG, 1 */
      tiff->rows_per_strip = value;
      break;
               
    case 279: /* StripByteCounts, SHORT or LONG,
               * N = StripsPerImage for PlanarConfiguration = 1
               *   = SamplesPerPixel * StripsPerImage
               *        for PlanarConfiguration = 2.
               * for each strip the number of bytes in that strip.
               */
      tiff->strip_byte_counts = value;

      /*             tiff->stripsize = (long *)malloc(sizeof(long)*len);

      if (len == 1){
      tiff->stripsize[0] = value;
      }
      else{
      pos = ftell(fp);
      fseek(fp, value, SEEK_SET);
      fread(tiff->stripsize, sizeof(long), len, fp);
      fseek(fp, pos, SEEK_SET);
      }
      */
      break;

    case 280: /* MinSampleValue, SHORT, N = SamplesPerPixel */
      break;
    case 281: /* MaxSampleValue, SHORT, N = SamplesPerPixel */
      break;
    case 282: /* XResolution, RATIONAL, N = 1 */
      break;
    case 283: /* YResolution, RATIONAL, N = 1 */
      break;
    case 284: /* PlanarConfiguration, SHORT, N = 1 */
      /*  1 : store contiguously. (Like MRC color)
       *  2 : store in planes.    (Like SGI RGB)
       */
      break;
    case 296: /* ResolutionUnit, SHORT, N = 1 */
      /*  1 =  None
       *  2 =  inch
       *  3 =  cm
       */
      break;

    case 301: /* ColorResponseCurves, SHORT, 3 * (2**BitsPerSample)*/
      break;

    case 305: /* Software, ASCII */
      break;
    case 306: /* DateTime, ASCII , N = 20 */
      break;

    case 320: /* ColorMap,  SHORT, 3 * (2**BitsPerSample) */
      break;

    case 324: /* Tile offsets */
    case 325: /* Tile byte counts */
      printf("ERROR: read_tiffentries - tiled data are not supported without"
              " libtiff.so;\n"
              " copy data to a file with strips (e.g., tiffcp -s)\n");
      return(0);

    }
  }


  tiff->stripoff = (b3dInt32 *)malloc(sizeof(b3dInt32) * tiff->nstrip);
  if (tiff->nstrip == 1)
    tiff->stripoff[0] = tiff->strip_pos;
  else{
    fseek(fp, tiff->strip_pos, SEEK_SET);
    for(i = 0; i < tiff->nstrip; i++){
      fread(&(tiff->stripoff[i]), LONGSZ, 1, fp);
      if (tiff->header.byteorder != M_BYTEORDER)
        swap((char *)&(tiff->stripoff[i]),   LONGSZ);
    }
  }

  tiff->stripsize = (b3dInt32 *)malloc(sizeof(b3dInt32)* tiff->nstrip);
  if (tiff->nstrip == 1){
    tiff->stripsize[0] = tiff->strip_byte_counts;
  }
  else{
    fseek(fp, tiff->strip_byte_counts, SEEK_SET);
    for(i = 0; i < tiff->nstrip; i++){
      fread(&(tiff->stripsize[i]), LONGSZ, 1, fp);
      if (tiff->header.byteorder != M_BYTEORDER)
        swap((char *)&(tiff->stripsize[i]),   LONGSZ);
    }
  }
     
  return(1);
}

/* Writes a single image to a file and maintains idfOffset and dataOffset so
   additional images can be added */
int tiff_write_image(FILE *fout, int xsize, int ysize, int mode,
                     unsigned char *pixels, b3dUInt32 *ifdOffset, 
                     b3dUInt32 *dataOffset, float dmin, float dmax)
{
  int pad;
  short tenum;
  b3dUInt32 pixel, ifd;
  b3dUInt32 dataSize, pixSize;
  b3dUInt32 *dminp = (b3dUInt32 *)(&dmin);
  b3dUInt32 *dmaxp = (b3dUInt32 *)(&dmax);
  int xysize = xsize * ysize;
  int y, sampleFormat;

  if (!*ifdOffset) {
    if ( M_BYTEORDER ==  BIGENDIAN)
      pixel = 0x4D4D002A;
    else
      pixel = 0x002A4949;
    fwrite(&pixel, 4, 1, fout);
    *ifdOffset = 4;
    *dataOffset = 8;
  }

  if (ferror(fout)) return(-1);

  dataSize = xsize * ysize;
  switch(mode){
  case MRC_MODE_BYTE:
    pixSize = 1; break;
  case MRC_MODE_SHORT:
    pixSize = 2; 
    sampleFormat = 2;
    break;
  case MRC_MODE_USHORT:
    pixSize = 2; 
    sampleFormat = 1;
    break;
  case MRC_MODE_RGB:
    pixSize = 3; break;
  case MRC_MODE_FLOAT:
    pixSize = 4;
    sampleFormat = 3;
    break;
  default:
    return(-50);
  }

  /* IFD must be on a word boundary */
  ifd = dataSize * pixSize;
  pad = -(ifd % 4) + 4;
  ifd += pad + 4 + *dataOffset;
  fseek(fout, *ifdOffset, SEEK_SET);
  if (!fwrite(&ifd, 4, 1, fout))
    return(-2);
  fseek(fout, *dataOffset, SEEK_SET);
     

  /* write image data. */
  /* To do: flip image from top to bottom. */
  for(y = ysize - 1; y >= 0; y--){
    fwrite(&pixels[xsize  * y * pixSize], pixSize, xsize, fout);
    if (ferror(fout)) return(-3);
  }

  pixel = 0;
  fwrite(&pixel, 4, 1, fout);
  if (ferror(fout)) return(-4);

  /* Write IFD */
  fseek(fout, ifd, SEEK_SET);

  /* Tags must be in ascending order! */
  if (mode != 16){
    tenum = mode ? 14 : 11; /* set to number of tiff entries. */
    fwrite(&tenum, 2, 1, fout);
    tiff_write_entry(254, 4, 1, 0, fout);
    tiff_write_entry(256, 3, 1, xsize, fout);
    tiff_write_entry(257, 3, 1, ysize, fout);
    tiff_write_entry(258, 3, 1, 8*pixSize, fout);  /* pixel size  1x8  */
    tiff_write_entry(259, 3, 1, 1, fout);  /* compression , none*/

    tiff_write_entry(262, 3, 1, 1, fout ); /* 0 is Black */
    tiff_write_entry(273, 4, 1, *dataOffset, fout);  /* image data start */
    tiff_write_entry(277, 3, 1, 1, fout);  /* samples / pixel */
    tiff_write_entry(278, 4, 1, ysize, fout);
    tiff_write_entry(279, 4, 1, xysize*pixSize, fout);
    tiff_write_entry(296, 3, 1, 1, fout);
    
    /* for everything but bytes, put out format and min/max */
    if (mode) {
      tiff_write_entry(339, 3, 1, sampleFormat, fout);   
      tiff_write_entry(340, 11, 1, *dminp, fout);
      tiff_write_entry(341, 11, 1, *dmaxp, fout);
    }
    fwrite(&pixel, 4, 1, fout);
    *ifdOffset = ifd + 2 + (tenum * 12);
    *dataOffset = ifd + 6 + (tenum * 12);
  }else{
    short bps = 8;
    tenum = 12; /* set to number of tiff entries. */
    fwrite(&tenum, 2, 1, fout);

    tiff_write_entry(254, 4, 1, 0, fout);
    tiff_write_entry(256, 3, 1, xsize, fout);
    tiff_write_entry(257, 3, 1, ysize, fout);
    tiff_write_entry(258, 3, 3, ifd + 6 + (tenum * 12), fout); 
    tiff_write_entry(259, 3, 1, 1, fout);  /* compression , none*/
          
    tiff_write_entry(262, 3, 1, 2, fout ); /* 0 is Black */
    tiff_write_entry(273, 4, 1, *dataOffset, fout);  /* image data start */
    tiff_write_entry(277, 3, 1, 3, fout);  /* samples / pixel */
    tiff_write_entry(278, 4, 1, ysize, fout);
    tiff_write_entry(279, 4, 1, xysize*3, fout);
    tiff_write_entry(284, 3, 1, 1, fout);
    tiff_write_entry(296, 3, 1, 1, fout);
    fwrite(&pixel, 4, 1, fout);

    fwrite(&bps, 2, 1, fout);
    fwrite(&bps, 2, 1, fout);
    fwrite(&bps, 2, 1, fout);
    *ifdOffset = ifd + 2 + (tenum * 12);
    *dataOffset = ifd + 12 + (tenum * 12);
  }
     
  if (ferror(fout)) return(-10);
  return(0);
}

/*

$Log$
Revision 3.10  2010/12/18 18:47:48  mast
Fixes for large file usage through libtiff

Revision 3.9  2009/06/19 20:48:10  mast
Added support for integer files

Revision 3.8  2008/05/23 22:23:50  mast
Error standardization and cleanup

Revision 3.7  2008/05/23 22:17:43  mast
Added ability to write stacks and support of floats

Revision 3.6  2006/08/28 05:26:27  mast
Add ability to handle colormapped images

Revision 3.5  2006/06/19 19:29:16  mast
Added ability to write unsigned ints from mode 6

Revision 3.4  2005/02/11 01:42:34  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.3  2004/09/10 21:33:31  mast
Eliminated long variables

Revision 3.2  2003/10/24 02:28:42  mast
strip directory from program name and/or use routine to make backup file

Revision 3.1  2003/02/27 20:14:38  mast
set default upper right values to -1

*/
