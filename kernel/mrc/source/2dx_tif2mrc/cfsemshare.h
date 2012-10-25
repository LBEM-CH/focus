/*   cfsemshare.h   - functions in multiple files, mostly shared between C and
 *                      Fortran and/or IMOD and SerialEM
 *
 *   Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 */                                                                           

#ifndef CFSEMSHARE_H
#define CFSEMSHARE_H

#include "mrcslice.h"

#ifdef __cplusplus
extern "C" {
#endif

  /* parselist.c  - for parsing a list of integers */
  int *parselist (const char *line, int *nlist);

  /* amoeba.c - simplex minimization routine */
  void amoeba(float *p, float *y, int mp, int ndim, float ftol, 
              void (*funk)(float *, float *), int *iterP, float *ptol,
              int *iloP);
  void amoebaInit(float *p, float *y, int mp, int ndim, float delfac, 
                  float ptolFac, float *a, float *da, 
                  void (*funk)(float *, float *), float *ptol);

  /* samplemeansd.c - for computing mean and SD quickly by sampling */
  int sampleMeanSD(unsigned char **image, int type, int nx, int ny,
                   float sample, int nxMatt, int myMatt, int nxUse, int nyUse,
                   float *mean, float *sd);

  /* pctstretch.c - for computing percentile limits quickly by sampling */
  int percentileStretch(unsigned char **image, int mode, int nx, int ny, float sample,
                        int ixStart, int iyStart, int nxUse, int nyUse, 
                        float pctLo, float pctHi, float *scaleLo, float *scaleHi);

  /* colormap.c */
  int *cmapStandardRamp(void);
  int *cmapInvertedRamp(void);
  int cmapConvertRamp(int *rampData, unsigned char table[3][256]);
  int cmapReadConvert(char *filename, unsigned char table[3][256]);

  /* cubinterp.c */
  void cubinterp(float *array, float *bray, int nxa, int nya, int nxb, int nyb,
                 float amat[2][2], float xc, float yc, float xt, float yt,
                 float scale, float dmean, int linear);

  /* reduce_by_binning.c */
  int reduceByBinning(void *array, int type, int nxin, int nyin, int nbin, 
                      void *brray, int keepByte, int *nxr, int *nyr);

  /* filtxcorr.c */
  void XCorrSetCTF(float sigma1, float sigma2, float radius1, float radius2,
                   float *ctf, int nx, int ny, float *delta);
  void XCorrSetCTFnoScl(float sigma1, float sigma2, float radius1,
                        float radius2, float *ctf, int nx,int ny,
                        float *delta, int *nsizeOut);
  void XCorrFilterPart(float *fft, float *array, int nx, int ny, float *ctf, 
                       float delta);
  void XCorrMeanZero(float *array, int nxdim, int nx, int ny);
  void XCorrPeakFind(float *array, int nxdim, int ny, float  *xpeak,
                     float *ypeak, float *peak, int maxpeaks);
  double parabolicFitPosition(float y1, float y2, float y3);
  void conjugateProduct(float *array, float *brray, int nx, int ny);
  double XCorrCCCoefficient(float *array, float *brray, int nxdim, int nx,
                            int ny, float xpeak, float ypeak, int nxpad,
                            int nypad, int *nsum);
  void sliceGaussianKernel(float *mat, int dim, float sigma);
  void scaledGaussianKernel(float *mat, int *dim, int limit, float sigma);
  void applyKernelFilter(float *array, float *brray, int nxdim, int nx, int ny,
                         float *mat, int kdim);

  /* taperpad.c */
  void sliceTaperOutPad(void *array, int type, int nxbox, int nybox, 
                        float *brray, int nxdim, int nx, int ny, int ifmean,
                        float dmeanin);
  void sliceTaperInPad(void *array, int type, int nxdimin, int ix0, int ix1,
                       int iy0, int iy1, float *brray, int nxdim, int nx,
                       int ny, int nxtap, int nytap);
  double sliceEdgeMean(float *array, int nxdim, int ixlo, int ixhi, int iylo,
                       int iyhi);
  void sliceSplitFill(float *array, int nxbox, int nybox, float *brray,
                      int nxdim, int nx, int ny, int iffill, float fillin);
  void sliceSmoothOutPad(void *array, int type, int nxbox, int nybox, 
                           float *brray, int nxdim, int nx, int ny);

  /* taperatfill.c */
  int sliceTaperAtFill(Islice *sl, int ntaper, int inside);

  /* circlefit.c */
  int circleThrough3Pts(float x1, float y1, float x2, float y2, float x3, 
                             float y3, float *rad, float *xc, float *yc);
  int fitSphere(float *xpt, float *ypt, float *zpt, int numPts,
                     float *rad, float *xcen, float *ycen, float *zcen,
                     float *rmsErr);
  int fitSphereWgt(float *xpt, float *ypt, float *zpt, float *weights,
                   int numPts, float *rad, float *xcen, float *ycen,
                   float *zcen, float *rmsErr);

  /* insidecontour.c */
  int InsideContour(float *ptX, float *ptY, int np, float x, float y);

  /* scaledsobel.c */
  int scaledSobel(float *inImage, int nxin, int nyin, float scaleFac, 
                  float minInterp, int linear, float center, float *outImage,
                  int *nxout, int *nyout, float *xOffset, float *yOffset);

  /* histogram.c */
  void kernelHistogram(float *values, int numVals, float *bins, int numBins,
                       float firstVal, float lastVal, float h, int verbose);
  int scanHistogram(float *bins, int numBins, float firstVal, float lastVal,
                    float scanBot, float scanTop, int findPeaks, float *dip,
                    float *peakBelow, float *peakAbove);
  int findHistogramDip(float *values, int numVals, int minGuess, float *bins,
                       int numBins, float firstVal, float lastVal, 
                       float *histDip, float *peakBelow, float *peakAbove,
                       int verbose);
  /* simplestat.c */
  void avgSD(float *x, int n, float *avg, float *sd, float *sem);
  void sumsToAvgSD(float sx, float sxsq, int n, float *avg, float *sd);
  void sumsToAvgSDdbl(double sx8, double sxsq8, int n1, int n2, float *avg,
                      float *sd);
  void lsFit(float *x, float *y, int num, float *slope, float *intcp,
             float *ro);
  void lsFitPred(float *x, float *y, int n, float *slope, float *bint,
                 float *ro, float *sa, float *sb, float *se,
                 float xpred, float *ypred, float *prederr);
  void lsFit2(float *x1, float *x2, float *y, int n, float *a, float *b,
              float *c);
  void lsFit2Pred(float *x1, float *x2, float *y, int n, float *a, float *b, 
                  float *c, float x1pred, float x2pred, float *ypred,
                  float *prederr);
  void lsFit3(float *x1, float *x2, float *x3, float *y, int n, float *a1, 
              float *a2, float *a3, float *c);
  void eigenSort(double *val, double *vec, int n, int rowStride, int colStride,
                 int useAbs);

  /* robuststat.c */
  void rsSortFloats(float *x, int n);
  void rsSortIndexedFloats(float *x, int *index, int n);
  void rsMedianOfSorted(float *x, int n, float *median);
  void rsMedian(float *x, int n, float *tmp, float *median);
  void rsMADN(float *x, int n, float median, float *tmp, float *MADN);
  void rsMadMedianOutliers(float *x, int n, float kcrit, float *out);
  void rsTrimmedMean(float *x, int n, float gamma, float *xsort, 
                     float *trmean);
  void rsTrimmedMeanOfSorted(float *x, int n, float gamma, float *trmean);

  /* amat_to_rotamgstr.c */
  void amatToRotmagstr(float a11, float a12, float a21, float a22, 
                         float *theta, float *smag, float *str, float *phi);
  void rotmagstrToAmat(float theta, float smag, float str, float phi,
                       float *a11, float *a12, float *a21, float *a22);

  /* percentile.c */
  float percentileFloat(int s, float *r, int num);
  int percentileInt(int s, int *r, int num);

  /* convexbound.c */
  void convexBound(float *sx, float *syin, int npnts, float fracomit,
                   float pad, float *bx, float *by, int *nvert, float *xcen,
                   float *ycen, int maxverts);

  /* beadfind.c */
  void makeModelBead(int boxSize, float beadSize, float *array);
  double beadIntegral(float *array, int nxdim, int nx, int ny, float rCenter,
                      float rInner, float rOuter, float xcen, float ycen,
                      float *cenmean, float *annmean, float *temp, 
                      float annPct, float *median);

  /* parallelwrite.c */
  int parWrtInitialize(char *filename, int nxin, int nyin);
  int parWrtProperties(int *allSec, int *linesBound, int *nfiles);
  int parWrtFindRegion(int secNum, int lineNum, int nlWrite, char **filename, 
                       int *sections, int *startLines);
  int parWrtSetCurrent(int index);

  /* statfuncs.f */
  double tValue(double signif, int ndf);
  double fValue(double signif, int ndf1, int ndf2);
  double errFunc(double x);
  double incompBeta(double a, double b, double x);
  double betaFunc(double p, double q);
  double gammaFunc(double x);
  double lnGamma(double x);

  /* surfacesort.c */
  int surfaceSort(float *xyz, int numPts, int markersInGroup, int *group);
  int setSurfSortParam(int which, float value);

  /* gaussj.c */
  int gaussj(float *a, int n, int np, float *b, int m, int mp);
  int gaussjDet(float *a, int n, int np, float *b, int m, int mp, float *determ);

  /* find_piece_shifts.c */
  int findPieceShifts
  (int *ivarpc, int nvar, int *indvar, int *ixpclist, int *iypclist, 
   float *dxedge, float *dyedge, int idir, int *pieceLower, int *pieceUpper, 
   int *ifskipEdge, int edgeStep, float *dxyvar, int varStep, int *edgelower,
   int *edgeupper, int pcStep, int *work, int fort, int leaveInd, int skipCrit,
   float robustCrit, float critMaxMove, float critMoveDiff, int maxIter,
   int numAvgForTest, int intervalForTest, int *numIter, float *wErrMean, 
   float *wErrMax);

  /* zoomdown.c */
  int selectZoomFilter(int type, double zoom, int *outWidth);
  void setZoomValueScaling(float factor);
  int zoomWithFilter(unsigned char **slines, int sXsize, int sYsize, float sXoff,
                     float sYoff, int dXsize, int dYsize, int dXdim, int dXoff, int dtype,
                     void *outData, b3dUInt32 *cindex, unsigned char *bindex);
  int zoomFiltInterp(float *array, float *bray, int nxa, int nya, int nxb, int nyb,
                     float xc, float yc, float xt, float yt, float dmean);

  /* xformfuncs.f */
  void xfUnit(float *f, float val, int rows);
  void xfCopy(float *f1, int rows1, float *f2, int rows2);
  void xfMult(float *f1, float *f2, float *prod, int rows);
  void xfInvert(float *f, float *finv, int rows);
  void xfApply(float *f, float xcen, float ycen, float x, float y, float *xp, float *yp,
               int rows);

  /* piecefuncs.c */
  int checkPieceList(int *pclist, int stride, int npclist, int redfac, int nframe,
                     int *minpiece, int *npieces, int *noverlap);
  void adjustPieceOverlap(int *pclist, int stride, int npclist, int nframe, int minpiece,
                          int noverlap, int newOverlap);

  /* regression.c */
  void statMatrices(float *x, int xsize, int colFast, int m, int msize, int ndata,
                    float *sx, float *ss, float *ssd, float *d, float *r, float *xm,
                    float *sd, int ifdisp);
  int multRegress(float *x, int xsize, int colFast, int m, int ndata, int nbcol,
                  int wgtcol, float *b, int bsize, float *c, float *xm, float *sd,
                  float *work);
  int robustRegress(float *x, int xsize, int colFast, int m, int ndata, int nbcol,
                    float *b, int bsize, float *c, float *xm, float *sd, float *work,
                    float kfactor, int *numIter, int maxIter, float maxChange);

#ifdef __cplusplus
}
#endif


#endif
