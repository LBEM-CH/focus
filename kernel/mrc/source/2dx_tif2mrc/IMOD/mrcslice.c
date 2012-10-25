/*
 *  mrcslice.c -- Library of image slice functions.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "b3dutil.h"

/*
 * Basic manipulation and information functions: DOC_SECTION BASIC
 */
/*!
 * Converts the data in slice [s] from its current mode to [mode], allocating 
 * a new data array as needed.  Complex values are converted to others by 
 * taking the magnitude.  Values are converted to complex modes by setting the
 * real component to the value, and the imaginary component to 0.  RGB values
 * are converted by taking a weighted sum of components.  When converting to a
 * mode with integer or byte values, the data are truncated to fit within the
 * range of the new mode.  Returns the new mode or -1 for error.
 */
int sliceNewMode(Islice *s, int mode)
{
  Islice *ns;
  Ival val;
  int i, j, k;
  int default_copy = 0;
  int limit_val = 0;
  float minval, maxval;

  if (!s)
    return(-1);
     
  if (s->mode == mode)
    return(mode);

  ns = sliceCreate(s->xsize, s->ysize, mode);

  /* Set up limiting values */
  if (mode == MRC_MODE_BYTE || mode == MRC_MODE_RGB) {
    limit_val = 1;
    minval = 0.;
    maxval = 255;
  } else if (mode == MRC_MODE_SHORT) {
    limit_val = 1;
    minval = -32768.;
    maxval = 32767.;
  } else if (mode == MRC_MODE_USHORT) {
    limit_val = 1;
    minval = 0.;
    maxval = 65535.;
  }

  if (!ns)
    return(-1);

  switch(s->mode){
  case MRC_MODE_BYTE:
  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
  case MRC_MODE_FLOAT:
    switch(mode){
    case MRC_MODE_BYTE:
    case MRC_MODE_SHORT:
    case MRC_MODE_USHORT:
    case MRC_MODE_FLOAT:
      default_copy = 1;
      break;
    case MRC_MODE_COMPLEX_FLOAT:
    case MRC_MODE_COMPLEX_SHORT:
      val[1] = 0;
      default_copy = 1;
      break;
    case MRC_MODE_RGB:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          if (limit_val)
            val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          val[2] = val[1] = val[0];
          slicePutVal(ns, i, j, val);
        }
      break;
    default:
      default_copy = 1;
      break;
    }
    break;

  case MRC_MODE_COMPLEX_FLOAT:
  case MRC_MODE_COMPLEX_SHORT:
    switch(mode){
    case MRC_MODE_BYTE:
    case MRC_MODE_SHORT:
    case MRC_MODE_USHORT:
    case MRC_MODE_FLOAT:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] = (float)sqrt(val[0] * val[0] + val[1] * val[1]);
          if (limit_val)
            val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          slicePutVal(ns, i, j, val);
        }
      break;
    case MRC_MODE_COMPLEX_FLOAT:
    case MRC_MODE_COMPLEX_SHORT:
      default_copy = 1;
      break;
    case MRC_MODE_RGB:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] = (float)sqrt(val[0] * val[0] + val[1] * val[1]);
          if (limit_val)
            val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          val[2] = val[1] = val[0];
          slicePutVal(ns, i, j, val);
        }
      break;
    }
    break;

  case MRC_MODE_RGB:
    switch(mode){
    case MRC_MODE_BYTE:
    case MRC_MODE_SHORT:
    case MRC_MODE_USHORT:
    case MRC_MODE_FLOAT:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] = (val[0] * 0.3f) + (val[1] * 0.59f) + (val[2] * 0.11f);
          if (limit_val)
            val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          slicePutVal(ns, i, j, val);
        }
      break;
           
    case MRC_MODE_COMPLEX_FLOAT:
    case MRC_MODE_COMPLEX_SHORT:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] = (val[0] * 0.3f) + (val[1] * 0.59f) + (val[2] * 0.11f);
          val[1] = 0;
          slicePutVal(ns, i, j, val);
        }
      break;
           
    default:
      default_copy = 1;
      break;
    }
    break;

  default:
    default_copy = 1;
    break;

  }

  if (default_copy) {
    for (j = 0; j < s->ysize; j++) {
      if (limit_val && s->csize > 1) {
        for (i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          for (k = 0; k < s->csize; k++)
            val[k] = B3DMIN(maxval, B3DMAX(minval, val[k]));
          slicePutVal(ns, i, j, val);
        }
      } else if (limit_val) {
        for (i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          slicePutVal(ns, i, j, val);
        }
      } else {
        for (i = 0; i < s->xsize; i++) {
          sliceGetVal(s,  i, j, val);
          slicePutVal(ns, i, j, val);
        }
      }
    }
  }
     
  free(s->data.b);

  /* 2/3/07: switch from copying ns to s to just setting data and mode */
  s->data.b = ns->data.b;
  s->mode = mode;
  free(ns);
  return(mode);
}

/*!
 * Converts the data in [slice] to float mode.  For complex data, the magnitude
 * is taken; for RGB data, a weighed sum of the components is taken.  This
 * should be slightly more efficient than @sliceNewMode is.  Returns -1 for
 * error.
 */
int sliceFloat(Islice *slice)
{
  Islice *tsl;
  Ival val;
  int i, j;

  switch(slice->mode){
  case SLICE_MODE_BYTE:
  case SLICE_MODE_SHORT:
  case SLICE_MODE_USHORT:
    tsl = sliceCreate(slice->xsize, slice->ysize, SLICE_MODE_FLOAT);
    if (!tsl)
      return(-1);
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        slicePutVal(tsl,   i, j, val);
      }
    free(slice->data.b);
    slice->data.f = tsl->data.f;
    slice->mode = SLICE_MODE_FLOAT;
    free(tsl);
    break;
  case SLICE_MODE_FLOAT:
    break;
  case SLICE_MODE_COMPLEX_SHORT:
  case SLICE_MODE_COMPLEX_FLOAT:
    tsl = sliceCreate(slice->xsize, slice->ysize, SLICE_MODE_FLOAT);
    if (!tsl)
      return(-1);
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] = (val[0] * val[0]) + (val[1] * val[1]);
        val[0] = sqrt(val[0]);
        slicePutVal(tsl,   i, j, val);
      }
    free(slice->data.b);
    slice->data.f = tsl->data.f;
    slice->mode = SLICE_MODE_FLOAT;
    free(tsl);
    break;
  case SLICE_MODE_RGB:
    tsl = sliceCreate(slice->xsize, slice->ysize, SLICE_MODE_FLOAT);
    if (!tsl)
      return(-1);
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] = val[0] * 0.3f + val[1] * 0.59f + val[2] * 0.11f;
        slicePutVal(tsl,   i, j, val);
      }
    free(slice->data.b);
    slice->data.f = tsl->data.f;
    slice->mode = SLICE_MODE_FLOAT;
    free(tsl);
    break;
  default:
    return(-1);
  }
  return(0);
}

/*!
 * Converts the data in [slice] from modes 0-3 or 6 to complex float.  For
 * modes 0-2 and 6, the value is placed in the real component and the imaginary
 * component is set to 0.  This should be slightly more efficient than
 * @sliceNewMode is.  Returns -1 for error.
 */
int sliceComplexFloat(Islice *slice)
{
  Islice *tsl;
  Ival val;
  int i, j;
     
  if (slice->mode > 3 && slice->mode != MRC_MODE_USHORT)
    return(-1);

  val[1] = 0;
  tsl = sliceCreate(slice->xsize, slice->ysize, MRC_MODE_COMPLEX_FLOAT);
  if (!tsl)
    return(-1);
  for(j = 0; j < slice->ysize; j++)
    for(i = 0; i < slice->xsize; i++){
      sliceGetVal(slice, i, j, val);
      slicePutVal(tsl,   i, j, val);
    }
  free(slice->data.b);
  slice->data.f = tsl->data.f;
  slice->mode = MRC_MODE_COMPLEX_FLOAT;
  free(tsl);
  return(0);
}

/*!
 * Calculates the min, max, and mean of slice [s] and fills in the structure
 * members.  Returns -1 for an empty slice.
 */
int sliceMMM(Islice *s)
{
  int i, j;
  Ival val;
  double tsum, sum;
  float temp;

  /* DNM 3/29/01: need to take magnitude for complex, and set mean to val */
  sliceGetVal(s, 0, 0, val);
  if (s->mode == MRC_MODE_COMPLEX_FLOAT){
    temp = (val[0] * val[0]) + (val[1] * val[1]);
    val[0] = (float)sqrt(temp);
  }
  s->max = s->min = val[0];

  /* 5/23/05: Huh?  sum needs to be 0 */
  sum = 0.;

  if ((!s->xsize) || (!s->ysize)){
    b3dError(stderr, "sliceMMM: Warning, empty slice.\n");
    return(-1);
  }

  for (j = 0; j < s->ysize; j++) {
    tsum = 0.;
    for (i = 0; i < s->xsize; i++){
      sliceGetVal(s, i, j, val);
           
      if (s->mode == MRC_MODE_COMPLEX_FLOAT){
        temp = (val[0] * val[0]) + (val[1] * val[1]);
        val[0] = (float)sqrt(temp);
      }

      if (s->min > val[0])
        s->min = val[0];
      if (s->max < val[0])
        s->max = val[0];
      tsum += val[0];
    }
    sum += tsum;
  }
  s->mean = sum / (float)(s->xsize * s->ysize);
  return(0);
}

/*!
 * Extracts a slice at section number [sno] from the volume in the
 * @@Istack structure@ [v], where [axis] is either x or X for a Y/Z slice or
 * y or Y for an X/Z slice.  Returns NULL for error.
 */
Islice *mrc_slice_getvol(Istack *v, int sno, char axis)
{
  Islice *sout;
  Ival val;
  int i, j, k;

  switch (axis){
  case 'y': case 'Y':
    sout = sliceCreate(v->vol[0]->xsize, v->zsize, v->vol[0]->mode);
    if (!sout)
      return(NULL);
    for (k = 0; k < sout->ysize; k++)
      for(i = 0; i < sout->xsize; i++){
        sliceGetVal(v->vol[k], i, sno, val);
        slicePutVal(sout, i, k, val);
      }
    break;

  case 'x': case 'X':
    sout = sliceCreate(v->vol[0]->ysize, v->zsize, v->vol[0]->mode);
    if (!sout)
      return(NULL);
    for (k = 0; k < sout->ysize; k++)
      for (j = 0; j <  sout->xsize; j++){
        sliceGetVal(v->vol[k], sno, j, val);
        slicePutVal(sout, j, k, val);
      }
    break;

  default:
    return(NULL);
  }
  return(sout);
}

/*!
 * Puts values from the slice [s] into the volume in
 * the @@Istack structure@ [v] at coordinate [sno] along the axis given by 
 * [axis], which must be one of x, X, y, Y, z, or Z.  For a Z slice,
 * the existing slice is freed and the supplied slice becomes part of the 
 * stack, so it should not be freed separately from the stack.  Returns 0.
 */
int mrc_slice_putvol(Istack *v, Islice *s, int sno, char axis)
{
  Ival val;
  int i, j, k;

  switch (axis){

  case 'z': case 'Z':
    sliceFree(v->vol[sno]);
    v->vol[sno] = s;
    break;

  case 'y': case 'Y':
    for (k = 0; k < s->ysize; k++)
      for(i = 0; i < s->xsize; i++){
        sliceGetVal(s, i, k, val);
        slicePutVal(v->vol[k], i, sno, val);
      }
    break;

  case 'x': case 'X':
    for (k = 0; k < s->ysize; k++)
      for (j = 0; j < s->xsize; j++){
        sliceGetVal(s, j, k, val);
        slicePutVal(v->vol[k], sno, j, val);
      }
    break;
       
  default:
    return(0);
  }
  return(0);
}

/*!
 * Computes the product of the complex values in [h] and the complex conjugate
 * of the values in [g] and places the results back in [g].  The number of
 * complex values in the arrays is given by [size].  Returns 0.
 */
int corr_conj(float *g, float *h, int size)
{
  int i;
  int real, imag;
  float temp, rtmp, itmp;

  for ( i = 0; i < size; i++){
    real = i * 2;
    imag = real + 1;
    temp = g[real];
    rtmp = h[real];
    itmp = h[imag];
    g[real] = rtmp * temp + itmp * g[imag];
    g[imag] = itmp * temp - rtmp * g[imag];
  }
  return(0);
}

/*
 * Scaling, boxing, wrapping, mirroring functions : DOC_SECTION SCALING_BOXING
 */
/*!
 * Adds the constant in the value array [c] to [slice].  Returns 0.
 */
int sliceAddConst(Islice *slice, Ival c)
{
  Ival val;
  unsigned int i, j;
  unsigned int xsize = slice->xsize;
  unsigned int ysize = slice->ysize;

  if (slice->csize == 1 ){
    for (j = 0; j < ysize; j++){
      for (i = 0; i < xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] += c[0];
        slicePutVal(slice, i, j, val);
      }
    }
    return(0);
  }
  if (slice->csize == 2 ){
    for (j = 0; j < ysize; j++){
      for (i = 0; i < xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] += c[0];
        val[1] += c[1];
        slicePutVal(slice, i, j, val);
      }
    }
    return(0);
  }
  if (slice->csize == 3 ){
    for (j = 0; j < ysize; j++){
      for (i = 0; i < xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] += c[0];
        val[1] += c[1];
        val[2] += c[2];
        slicePutVal(slice, i, j, val);
      }
    }
    return(0);
  }
  return(0);
}

/*!
 * Multiples all values in [slice] by the constants in the value array [c].
 * Returns 0.
 */
int sliceMultConst(Islice *slice, Ival c)
{
  int i, j;
  Ival val;
     
  switch(slice->csize){
  case 1:
    for (j = 0; j < slice->ysize; j++){
      for (i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] *= c[0];
        slicePutVal(slice, i, j, val);
      }
    }
    break;
  case 2:
    for (j = 0; j < slice->ysize; j++){
      for (i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] *= c[0];
        val[1] *= c[1];
        slicePutVal(slice, i, j, val);
      }
    }
  case 3:
    for (j = 0; j < slice->ysize; j++){
      for (i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] *= c[0];
        val[1] *= c[1];
        val[2] *= c[2];
        slicePutVal(slice, i, j, val);
      }
    }
    break;
  }
  return(0);
}

/*!
 * Multiplies all values in slice [s] by scale factor [inScale]. Returns 0.
 */
int mrc_slice_valscale(Islice *s, double inScale)
{
  Ival val;
  val[0] = val[1] = val[2] = inScale;
  return sliceMultConst(s, val);
}

/*!
 * Scales data in the slice by the factor [alpha] around the value [fixed];
 * i.e., a value of [fixed] is unchanged by the scaling.  Returns 0.
 */
int mrc_slice_lie(Islice *sin, double fixed, double alpha)
{
  int i, j, c;
  Ival val;
  float scale, offset, minval, maxval;
  
  minval = maxval = 0.;
  scale = (float)alpha;
  offset = (1.0f - scale) * (float)fixed;

  switch(sin->mode) {
  case MRC_MODE_BYTE:
  case MRC_MODE_RGB:
    maxval = 255.;
    break;
  case MRC_MODE_SHORT:
  case MRC_MODE_COMPLEX_SHORT:
    minval = -32768.;
    maxval = 32767.;
    break;
  case MRC_MODE_USHORT:
    maxval = 65535.;
    break;
  }

  if (sin->csize == 1) {
    for (j = 0; j < sin->ysize; j++)
      for (i = 0; i < sin->xsize; i++) {
        sliceGetVal(sin, i, j, val);
        val[0] = (offset + (scale * val[0]));
        if (maxval) {
          if (val[0] > maxval)
            val[0] = maxval;
          if (val[0] < minval)
            val[0] = minval;
        }
        slicePutVal(sin, i, j, val);
      }
  } else {
    for (j = 0; j < sin->ysize; j++)
      for (i = 0; i < sin->xsize; i++) {
        sliceGetVal(sin, i, j, val);
        for (c = 0; c < sin->csize; c++) {
          val[c] = (offset + (scale * val[c]));
          if (maxval) {
            if (val[c] > maxval)
              val[c] = maxval;
            if (val[c] < minval)
              val[c] = minval;
          }
        }
        slicePutVal(sin, i, j, val);
      }
  }
  return(0);
}

/*!
 * Extracts a subarea of slice [sl] into a new slice and returns the slice
 * or NULL for error.  The coordinates of the subarea are from [llx] to 
 * [urx] - 1 in X and [lly] to [ury] -1 in Y, inclusive.  For areas where      
 * there is no image data, the slice mean is used to fill only the first 
 * channel.
 */
Islice *sliceBox(Islice *sl, int llx, int lly, int urx, int ury)
{
  Islice *sout;
  int i, j, x, y;
  int nx, ny;
  Ival val;

  nx = urx-llx;
  ny = ury-lly;

  sout = sliceCreate(nx, ny, sl->mode);
  if (!sout)
    return(NULL);

  for(j = lly, y = 0; y < ny; y++, j++)
    for(i = llx, x = 0; x < nx; x++, i++){
      sliceGetVal(sl, i, j, val);
      slicePutVal(sout, x, y, val);
    }
  return(sout);
}

/*!
 * Replaces the slice [sl] with a subarea from [llx] to [urx] - 1 in X and
 * [lly] to [ury] -1 in Y, inclusive.  For areas where      
 * there is no image data, the slice mean is used to fill only the first 
 * channel.  Returns -1 for memory error.
 */
int sliceBoxIn(Islice *sl, int llx, int lly, int urx, int ury)
{
  Islice *sout;

  sout = sliceBox(sl, llx, lly, urx, ury);
  if (!sout)
    return(-1);
  if (sl->data.b)
    free(sl->data.b);
  memcpy(sl, sout, sizeof(Islice));
  free(sout);
  return(0);
}

/*! 
 * Resizes the slice [sl] in place to the new size [x], [y], with the center
 * at the old size placed in the center at the new size.  For areas where      
 * there is no image data, the slice mean is used to fill only the first 
 * channel.  Returns -1 for error.
 */
int sliceResizeIn(Islice *sl, int x, int y)
{
  int llx, lly, urx, ury;
  int cx, cy;
     
  if ((sl->xsize == x) && (sl->ysize == y))
    return(0);

  cx = sl->xsize / 2;
  cy = sl->ysize / 2;
  llx = cx - (x/2);
  lly = cy - (y/2);
  urx = llx + x;
  ury = lly + y;
  return(sliceBoxIn(sl, llx, lly, urx, ury));
}

/*!
 * Creates a new slice of size [nx], [ny] and resizes the input slice [slin]
 * into this slice, with the center of the old slice placed in the center of
 * the new one.  Fills areas with no data from the old slice with 
 * the slice mean for every channel of multi-channel data.  Returns new slice
 * or NULL for error.
 */
Islice *mrc_slice_resize(Islice *slin, int nx, int ny)
{
  Islice *sout;
  int i, j, x, y;
  int sx, sy;
  Ival pval, val;

  pval[0] = slin->mean;
  pval[1] = slin->mean;
  pval[2] = slin->mean;

  sout = sliceCreate(nx, ny, slin->mode);
  if (!sout)
    return(sout);

  sx = (slin->xsize - nx) / 2;
  sy = (slin->ysize - ny) / 2;

  for(j = 0, y = sy; j < ny; j++, y++)
    for(i = 0, x = sx; i < nx; i++, x++){
           
      if ( (x < 0) || (y < 0) || 
           (x >= slin->xsize) || (y >= slin->ysize )   )
        slicePutVal(sout, i, j, pval);
      else{
        sliceGetVal(slin, x, y, val);
        slicePutVal(sout, i, j, val);
      }
    }
  return(sout);
}

/*!
 * Mirrors the slice [s] about the given axis, where [axis] must be one of
 * x, X, y, or Y.  Returns 1 for error.
 */
int sliceMirror(Islice *s, char axis)
{
  int i, j;
  Ival val1;
  Ival val2;
  int lim;

  if (axis == 'x' || axis == 'X'){
    lim = s->ysize / 2;
    for(j = 0; j < lim; j++){
      for(i = 0; i < s->xsize; i++){
        sliceGetVal(s, i, j, val1);
        sliceGetVal(s, i, s->ysize - j - 1, val2);
        slicePutVal(s, i, j, val2);
        slicePutVal(s, i, s->ysize - j - 1, val1);
      }
    }
    return(0);
  }
  if (axis == 'y' || axis == 'Y'){
    lim = s->xsize / 2;
    for(i = 0; i < lim; i++)
      for(j = 0; j < s->ysize; j++){
        sliceGetVal(s, i, j, val1);
        sliceGetVal(s, s->xsize - 1 - i, j, val2);
        slicePutVal(s, i, j, val2);
        slicePutVal(s, s->xsize - 1 - i, j, val1);
      }
    return(0);
  }
  return(-1);
}

/*!
 * Wraps the lines of an FFT in slice [s] to bring the origin to the center
 * from the first line, or back.  Returns 1 for improper mode or 2 for memory
 * error.
 */
int sliceWrapFFTLines(Islice *s)
{
  int pixsize, i, ind1, ind2;
  unsigned char *buf;
  int nx = s->xsize;
  int ny = s->ysize;
    
  if (s->mode == MRC_MODE_COMPLEX_FLOAT)
    pixsize = 8;
  else if (s->mode == MRC_MODE_COMPLEX_SHORT)
    pixsize = 4;
  else
    return 1;

  buf = (unsigned char *)malloc(pixsize * nx);
  if (!buf)
    return 2;

  for (i = 0; i < ny / 2; i++) {
    ind1 = i * nx * 2;
    ind2 = (i + ny / 2) * nx * 2;
    memcpy(buf, &s->data.f[ind1], pixsize * nx);
    memcpy(&s->data.f[ind1], &s->data.f[ind2], pixsize * nx);
    memcpy(&s->data.f[ind2], buf, pixsize * nx);
  }
  free(buf);
  return(0);
}

/*!
 * Converts a slice with an old-style mirrored FFT to a standard FFT with
 * line length nx/2 + 1.  Returns 1 if the slice is not complex float.
 */
int sliceReduceMirroredFFT(Islice *s)
{
  int nfloats, i, j;
  int nx = s->xsize;
  float tmp1, tmp2;

  if (s->mode != MRC_MODE_COMPLEX_FLOAT)
    return 1;
  nfloats = nx + 2;
  for (j = 0; j < s->ysize; j++) {
    tmp1 = s->data.f[j * 2 * nx];
    tmp2 = s->data.f[j * 2 * nx + 1];
    for (i = 0; i < nx; i++)
      s->data.f[i + j * nfloats] = s->data.f[i + nx + j * 2 * nx];
    s->data.f[nx +  j * nfloats] = tmp1;
    s->data.f[nx + 1 + j * nfloats] = tmp2;
  }
  s->xsize = nx /2 + 1;
  return 0;
}

/*
 * I/O to MRC files : DOC_SECTION MRC_IO
 */
/*!
 * Writes the data from [slice] into a new MRC file whose name is given in 
 * [filename].  Returns -1 for error opening the file, -2 for error writing
 * header, or an error from ferror if there is an error writing the data.
 * Unused as of 2/3/07, but seems usable.
 */
int sliceWriteMRCfile(char *filename, Islice *slice)
{
  MrcHeader hout;
  FILE *fp = fopen(filename, "wb");
  int error;

  if (!fp) return -1;
  mrc_head_new(&hout, slice->xsize, slice->ysize, 1, slice->mode);
  sliceMMM(slice);
  hout.amin  = slice->min;
  hout.amax  = slice->max;
  hout.amean = slice->mean;
  if (mrc_head_write(fp, &hout)) {
    fclose(fp);
    return -2;
  }
  b3dFwrite(slice->data.b, slice->dsize, 
            slice->csize * slice->xsize * slice->ysize, fp);
  error = ferror(fp);
  fclose(fp);
  return(error);
}

/*!
 * Returns a slice with one plane of data from the file described by the
 * @@mrcfiles.html#MrcHeader structure@ [hin].  The coordinate of the plane is
 * [sno] along the axis given by [axis], which must be one of x, X, y, Y, z, 
 * or Z.  The file pointer in [hin] is used.  Bytes are swapped if necessary.
 * Returns NULL for errors.
 */
Islice *sliceReadMRC(MrcHeader *hin, int sno, char axis)
{
  Islice *slice;
  void *buf;
  int nx, ny;

  slice = (Islice *)malloc(sizeof(Islice));
  if (!slice)
    return(NULL);
  slice->mean = hin->amean;
  buf = mrc_mread_slice(hin->fp, hin, sno, axis);
  if (!buf) {
    free(slice);
    return(NULL);
  }
  switch(axis){
  case 'x':
  case 'X':
    nx = hin->nx;
    ny = hin->nz;
    break;
  case 'y':
  case 'Y':
    nx = hin->ny;
    ny = hin->nz;
    break;
  case 'z':
  case 'Z':
    nx = hin->nx;
    ny = hin->ny;
    break;
  default:
    break;
  }
  if (sliceInit(slice, nx, ny, hin->mode, buf)) {
    free(slice);
    return NULL;
  }
  return(slice);
}

/*!
 * Returns a slice with a subarea of one plane of data from the file described
 * by the @@mrcfiles.html#MrcHeader structure@ [hin].  The coordinate of the 
 * plane is [sno] along the axis given by [axis], which must be one of x, X, y,
 * Y, z, or Z.  The size of the subarea is given by [xsize] and [ysize] and its
 * center is given by [xcen], [ycen].  The entire slice is read in and then the
 * subarea is taken with @sliceBoxIn .  The file pointer in [hin] is used.
 * Bytes are swapped if necessary.  Returns NULL for errors.
 */
Islice *sliceReadSubm(MrcHeader *hin, 
                      int sno, char axis,
                      int xsize, int ysize, int xcen, int ycen)
{
  Islice *slice;
  void *buf;
  int nx, ny;
  int llx, lly, urx, ury;

  slice = (Islice *)malloc(sizeof(Islice));
  if (!slice)
    return(NULL);
  slice->mean = hin->amean;

  /* todo: just read in data that is needed. */
  buf = mrc_mread_slice(hin->fp, hin, sno, axis);
  if (!buf) {
    free(slice);
    return(NULL);
  }

  switch(axis){
  case 'x':
  case 'X':
    nx = hin->nx;
    ny = hin->nz;
    break;
  case 'y':
  case 'Y':
    nx = hin->ny;
    ny = hin->nz;
    break;
  case 'z':
  case 'Z':
    nx = hin->nx;
    ny = hin->ny;
    break;
  default:
    break;
  }
  sliceInit(slice, nx, ny, hin->mode, buf);

  llx = xcen - (xsize/2);
  lly = ycen - (ysize/2);
  urx = llx + xsize;
  ury = lly + ysize;
  sliceBoxIn(slice, llx, lly, urx, ury);

  return(slice);
}

/*!
 * Returns a slice with one Z plane of data at Z value [secno] from the file 
 * described by the @@mrcfiles.html#MrcHeader structure@ [hin].  
 * The file pointer in [hin] is used.  Bytes are swapped if necessary.
 * Returns NULL for errors.
 */
Islice *sliceReadFloat(MrcHeader *hin, int secno)
{
  Islice *slice;
  if (sliceModeIfReal(hin->mode) < 0) {
    b3dError(stderr, "ERROR: sliceReadFloat - file mode must be real");
    return NULL;
  }
  slice = sliceCreate(hin->nx, hin->ny, MRC_MODE_FLOAT);
  if (!slice)
    return NULL;
  if (mrcReadFloatSlice(slice->data.f, hin, secno)) {
    sliceFree(slice);
    return NULL;
  }
  return slice;
}

/*
 * Filtering and transformation : DOC_SECTION TRANSFORMS
 */
/*!
 * Returns a slice with the gradient of the input slice [sin], or NULL for 
 * error.  The gradient is the absolute value of the difference 
 * between the current and next pixel, averaged over the X and Y direction.
 */
Islice *sliceGradient(Islice *sin)
{
  Islice *s;
  int i, j;
  Ival val, nval, gval;

  s = sliceCreate(sin->xsize, sin->ysize, sin->mode);
  if (!s)
    return(NULL);

  /* Store gradient in X */
  for(j = 0; j < sin->ysize; j++){
    for(i = 0; i < sin->xsize - 1; i++){
      sliceGetVal(sin, i, j, val);
      sliceGetVal(sin, i+1, j, nval);
      val[0] = nval[0] - val[0];
      if (val[0] < 0)
        val[0] *= -1;
      slicePutVal(s, i, j, val);
    }
  }

  /* Get gradient in Y and average with the one in X, copy last line */
  for(i = 0; i < sin->xsize; i++){
    for(j = 0; j < sin->ysize - 1; j++){
      sliceGetVal(sin, i, j, val);
      sliceGetVal(sin, i, j + 1, nval);
      sliceGetVal(s, i, j, gval);
      val[0] = nval[0] - val[0];
      if (val[0] < 0)
        val[0] *= -1;
      gval[0] = (val[0] + gval[0]) / 2;
      slicePutVal(s, i, j, gval);
    }
    sliceGetVal(s, i, j - 1, val);
    slicePutVal(s, i, j, val);
  }

  /* Copy last column over too */
  for(j = 0; j < sin->ysize; j++){
    sliceGetVal(s, sin->xsize - 2, j, val);
    slicePutVal(s, sin->xsize - 1, j, val);
  }    
  sliceMMM(s);
  return(s);
}

/*!
 * Filters the FFT data in slice [sin] with a bandpass filter specified by
 * [low] and [high], in cycles/pixel (range 0 to 0.5).  The attenuation at
 * frequency {rad} is the product of 1/(1+(rad/low)**3) if [low] > 0 and 
 * 1/1+(high/rad)**3) if high > 0.  Returns -1 for mode not complex float.
 */
int mrc_bandpass_filter(struct MRCslice *sin, double low, double high)
{
  int i, j;
  double dist, mval, dx, dy, xscale, xadd, power = 3;
  Ival val;
     
  if (sin->mode != MRC_MODE_COMPLEX_FLOAT){
    b3dError(stderr, " mrc_band_filter: Only complex float mode.\n");
    return(-1);
  }

  /* Set up X coordinate scaling for odd or even (mirrored) FFTs */
  if (sin->xsize % 2) {
    xscale = 0.5 / (sin->xsize - 1.);
    xadd = 0.;
  } else {
    xscale = 1. / sin->xsize;
    xadd = -0.5;
  }

  for (j = 0; j < sin->ysize; j++)
    for(i = 0; i < sin->xsize; i++){
      dx = xscale * i + xadd;

      dy = (float)j / sin->ysize - 0.5;
      dist = sqrt(dx *dx + dy * dy);
      if (low > 0.) {
        if (dist < 0.00001)
          mval = 0;
        else
          mval = 1 / (1 + pow(low / dist, power));
      } else 
        mval = 1.0;
           
      if (high > 0.0)
        mval *= 1 / (1 + pow(dist / high, power));

      sliceGetVal(sin, i, j, val);
      val[0] *= mval;
      val[1] *= mval;
      slicePutVal(sin, i, j, val);
    }
  return(0);
}

/*!
 * Filters a slice [sin] by convolving with the square matrix [mat] of 
 * dimension [dim] and returns a float slice, or NULL for error.  Pixels outside
 * the image bounds are obtained by replicated pixels on the edge, so there is 
 * no need to set the {mean} value of the slice.  For a float input slice, it calls
 * @@cfutils.html#applyKernelFilter@ , otherwise it uses slower GetVal and PutVal based
 * operations.  Both approaches are parallelized with OpenMP, but the latter only for
 * kernel sizes up to 9.
 */
Islice *slice_mat_filter(Islice *sin, float *mat, int dim)
{
#define MAX_STATIC_KERNEL 9
  Islice *sout;
  float smat[MAX_STATIC_KERNEL * MAX_STATIC_KERNEL];
  float *imat;
  Ival val;
  int i,j, numThreads = 1;

  sout = sliceCreate(sin->xsize, sin->ysize, MRC_MODE_FLOAT);
  if (!sout)
    return NULL;

  if (sin->mode == SLICE_MODE_FLOAT) {
    applyKernelFilter(sin->data.f, sout->data.f, sin->xsize, sin->xsize, sin->ysize, mat,
                      dim);

  } else {
    if (dim <= MAX_STATIC_KERNEL) {
      numThreads = B3DNINT(0.04 * sqrt((double)sin->xsize * sin->ysize));
      numThreads = numOMPthreads(numThreads);
    }
    if (numThreads > 1) {

#pragma omp parallel for num_threads(numThreads)    \
  shared(sin, sout, dim, mat)                       \
  private(j, i, val, smat)
      for (j = 0; j < sin->ysize; j++) {
        for (i = 0; i < sin->xsize; i++) {
          mrc_slice_mat_getimat(sin, i, j, dim, smat);
          val[0] = mrc_slice_mat_mult(mat, smat, dim);
          slicePutVal(sout, i, j, val);
        }
      }
    } else {
      
      imat = (float *)malloc(dim * dim * sizeof(float));
      if (!imat)
        return(NULL);
      for (j = 0; j < sin->ysize; j++) {
        for (i = 0; i < sin->xsize; i++) {
          mrc_slice_mat_getimat(sin, i, j, dim, imat);
          val[0] = mrc_slice_mat_mult(mat, imat, dim);
          slicePutVal(sout, i, j, val);
        }
      }
      free(imat);
    }
  }
  return(sout);
}

/*!
 * Extracts a square matrix of size [dim] from the slice [sin], centered 
 * around position [x], [y] and places the result into the array [mat].
 */
void mrc_slice_mat_getimat(Islice *sin, int x, int y, int dim, 
                           float *mat)
{
  int xs, ys, xe, ye;
  int i, j, ic, jc;


  xs = x - (dim / 2);
  xe = xs + dim;
  ys = y - (dim / 2);
  ye = ys + dim;

  if (xs >= 0 && xe < sin->xsize && ys >= 0 && ye < sin->ysize) {
    for(j = ys; j < ye; j++)
      for(i = xs; i < xe; i++)
        mat[(i-xs )+ dim * (j-ys)] = sliceGetPixelMagnitude(sin, i, j);
  } else {

    /* If any pixels are out of bounds, replicate from inside pixels */
    for(j = ys; j < ye; j++)
      for(i = xs; i < xe; i++) {
        ic = B3DMAX(0, B3DMIN(sin->xsize - 1, i));
        jc = B3DMAX(0, B3DMIN(sin->ysize - 1, j));
        mat[(i-xs )+ dim * (j-ys)] = sliceGetPixelMagnitude(sin, ic, jc);
      }
  }
}

/*!
 * Forms a dot product between the two vectors [m1] and [m2], which are square
 * matrices of dimension [dim] 
 */
float mrc_slice_mat_mult(float *m1, float *m2, int dim)
{
  float rval = 0;
  int i, elements;

  elements = dim * dim;

  for(i = 0; i < elements; i++)
    rval += m1[i] * m2[i];

  return(rval);
}

/* USED ONLY BY clip_transform... */
/*!
 * Creates a slice of size [xsize], [ysize] and rotates the input slice [slin]
 * by the [angle] (in degrees) about the center point [cx], [cy].  Uses 
 * quadratic interpolation.  For areas where there is no image data, all
 * channels will be filled with the slice mean.  Returns the new slice or NULL
 * for error.
 */
Islice *mrc_slice_rotate(Islice *slin, double angle, int xsize, int ysize,
                         double cx, double cy)
{
  Islice *sout;
 
  sout = sliceCreate(xsize, ysize, slin->mode);
  if (sout == NULL)
    return(NULL);
     
  mrc_slice_rotates(slin, sout, angle, cx, cy);
  return(sout);
}

/*!
 * Rotates the input slice [slin] by the [angle] (in degrees) about the center
 * point [cx], [cy] and places the result into the output slice [sout].
 * Uses quadratic interpolation.  For areas where there is no image data, all
 * channels will be filled with the slice mean.  Returns 0.
 */
int mrc_slice_rotates(Islice *slin, Islice *sout, double angle, 
                      double cx, double cy)
{
  int i, j;
  Ival val;
  double x, y;
  double sino, coso;
  double x2, y2;

  angle *= 0.017453293;
 
  coso = cos(-angle);
  sino = sin(-angle);
  x2 = (double)sout->xsize * 0.5;
  y2 = (double)sout->ysize * 0.5;

  for(j = 0; j < sout->ysize; j++)
    for (i = 0; i < sout->xsize; i++){
      x = (((double)i - x2) * coso) - (((double)j - y2) * sino) + cx;
      y = (((double)i - x2) * sino) + (((double)j - y2) * coso) + cy;
      sliceQuadInterpolate(slin, x, y, val);
      slicePutVal(sout, i, j, val);
    }
  return(0);
}

/*!
 * Creates a slice of size [xsize], [ysize] and translates the input slice
 * [sin] by [dx], [dy] using bilinear interpolation, putting the result in
 * the new slice.  For areas where there is no image data, all channels will 
 * be filled with the slice mean.  Returns the new slice or NULL for error.
 */
Islice *mrc_slice_translate(Islice *sin, double dx, double dy,
                            int xsize, int ysize)
{
  int i, j;
  Islice *sout;
  Ival val;
  double x, y;

  sout = sliceCreate(xsize, ysize, sin->mode);

  if (sout == NULL)
    return(NULL);
     
  for(j = 0; j < ysize; j++){
    y = (double)j + dy;
    for (i = 0; i < xsize; i++){
      x = (double)i + dx;
      sliceQuadInterpolate(sin, x, y, val);
      slicePutVal(sout, i, j, val);
    }
  }
  return(sout);
}

/*!
 * Creates a slice of size [xsize], [ysize] and expands the input slice [sin]
 * by the factors [xz] in X and [yz] in Y about the center point [cx], [cy].
 * Uses quadratic interpolation.  For areas where      
 * there is no image data, all channels will be filled with the slice mean
 * Returns the new slice or NULL for error.
 */
Islice *mrc_slice_zoom(Islice *sin, double xz, double yz, 
                       int xsize, int ysize, double cx, double cy)
{
  Islice *sout;

  if ((!xz) || (!yz))
    return(NULL);

  sout = sliceCreate(xsize, ysize, sin->mode);
  if (sout == NULL)
    return(NULL);

  mrc_slice_zooms(sin, sout, xz, yz, cx, cy);
  return (sout);
}

/*!
 * Expands the input slice [sin] by the factors [xz] in X and [yz] in Y about
 * the center point [cx], [cy] and places the result in the slice [sout].
 * Uses quadratic interpolation.  For areas where      
 * there is no image data, the slice mean is used to fill only the first 
 * channel.  Returns the new slice or NULL for error.
 */
int mrc_slice_zooms(Islice *sin, Islice *sout, double xz, double yz, 
                    double cx, double cy)
{
  int i, j;
  Ival val;
  double x, y;
  double sbx, sby;

  if ((!xz) || (!yz))
    return(1);

  sbx = (xz * cx) - ((double)sout->xsize * 0.5);
  sby = (yz * cy) - ((double)sout->ysize * 0.5);

  for(j = 0; j < sout->ysize; j++){
    y = ((double)j / yz) + sby;
    for (i = 0; i < sout->xsize; i++){
      x = ((double)i / xz) + sbx;
      sliceQuadInterpolate(sin, x, y, val);
      slicePutVal(sout, i, j, val);
    }
  }
  return(0);
}

/*!
 * Fills the value array [val] with the interpolated value from position
 * [x], [y] in slice [sl], using quadratic interpolation.  For areas where
 * there is no image data, the slice mean is used for all elements of val.
 */
void sliceQuadInterpolate(Islice *sl, double x, double y, Ival val)
{
  int xi, yi;    /* nearest integer value to x, y */
  float dx, dy;  /* difference between nearest int val and actual value. */
  float a,b,c,d; /* coeffs for quad. */
  int i;
  Ival x1,x2,y1,y2;
  val[1] = val[2] = x1[1] = x1[2] = x2[1] = x2[2] = 0.;
  y1[1] = y1[2] = y2[1] = y2[2] = 0.;

  xi = (int)floor(x + 0.5);
  yi = (int)floor(y + 0.5);

  dx = x - xi;
  dy = y - yi;
     
  sliceGetVal(sl, xi, yi, val);
  sliceGetVal(sl, xi - 1, yi, x1);
  sliceGetVal(sl, xi + 1, yi, x2);
  sliceGetVal(sl, xi, yi - 1, y1);
  sliceGetVal(sl, xi, yi + 1, y2);

  for (i = 0; i < sl->csize; i++) {
    a = (x1[i] + x2[i]) * 0.5f - (float)val[i];
    b = (y1[i] + y2[i]) * 0.5f - (float)val[i];
    c = (x2[i] - x1[i]) * 0.5f;
    d = (y2[i] - y1[i]) * 0.5f;
    val[i] = (a * dx * dx) + (b * dy * dy) + (c * dx)+(d * dy) + val[i];
  }
  return;
}

/****************************************************************************/

/* reorders data so that fft center is at (datasize/2 <-> 0 )   
 UNUSED 2/2/07 except by unused mrc_vol_wrap */
int mrc_slice_wrap(Islice *s)
{
  int i,j;
  int mx, my;
  Ival val, tval;

  mx = s->xsize / 2;
  my = s->ysize / 2;
     
  for (j = 0; j < my; j++){
    for (i = 0; i < mx; i++){
      fflush(stdout);
      sliceGetVal(s, i, j, val);
      sliceGetVal(s, i + mx, j + my, tval);
      slicePutVal(s, i, j, tval);
      slicePutVal(s, i + mx, j + my, val);
    }
    for( i = mx; i < s->xsize; i++){
      sliceGetVal(s, i, j, val);
      sliceGetVal(s, i - mx, j + my, tval);
      slicePutVal(s, i, j, tval);
      slicePutVal(s, i - mx, j + my, val);
    }
  }
  return(0);
}

/* returns a float slice that is the real part of an image for complex data. */
/* UNUSED except by old mrcspectral 2/5/07 */
Islice *mrc_slice_real(Islice *sin)
{
  Islice *sout;
  int i, xysize;
      
  if (sin->mode != MRC_MODE_COMPLEX_FLOAT)
    return(sin);

  sout = sliceCreate(sin->xsize, sin->ysize, MRC_MODE_FLOAT);
  xysize = sin->xsize * sin->ysize;
  for(i = 0; i < xysize; i++){
    sout->data.f[i] = sin->data.f[i * 2];
  }
  return(sout);
}


/* Unused 11/10/05 */
int mrc_slice_lie_img(Islice *sin, 
                      Islice *mask, double alpha)
{
  int i, j;
  Ival val1, val2;
  float a, ma;

  a = alpha;
  ma = 1 - alpha;

  for(j = 0; j < sin->ysize; j++)
    for(i = 0; i < sin->xsize; i++){
      sliceGetVal(sin, i, j, val1);
      sliceGetVal(mask, i, j, val2);
      val1[0] = (ma * val1[0]) + (a * val2[0]);

      switch(sin->mode){
      case MRC_MODE_BYTE:
        if (val1[0] > 255)
          val1[0] = 255;
        if (val1[0] < 0)
          val1[0] = 0;
        break;
      case MRC_MODE_SHORT:
        if (val1[0] > 32767)
          val1[0] = 32767;
        if (val1[0] < -32768)
          val1[0] = -32768;
        break;
      case MRC_MODE_USHORT:
        if (val1[0] > 65535)
          val1[0] = 65535;
        if (val1[0] < 0)
          val1[0] = 0;
        break;
      }

      if (sin->csize == 3){
        val1[1] = (ma * val1[1]) + (a * val2[1]);
        val1[2] = (ma * val1[2]) + (a * val2[2]);
      }
      slicePutVal(sin, i, j, val1);
    }

  return(0);
}

/* UNUSED 2/2/07 */
int mrc_vol_wrap(struct MRCvolume *v)
{
  int k, z2;
  Islice *s;
  z2 = v->zsize / 2;

  for (k = 0; k < v->zsize; k++){
    mrc_slice_wrap(v->vol[k]);
  }
  for (k = 0; k < z2; k++){
    s = v->vol[k];
    v->vol[k] = v->vol[k + z2];
    v->vol[k + z2] = s;
  }
  return(0);
}
