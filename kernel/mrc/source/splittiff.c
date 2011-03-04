#include "tiffio.h"
int main(int argc, char *argv[])
{
	if(argc > 1)
	{
	    TIFF* tif = TIFFOpen(argv[1], "r");
	    if (tif) {
		uint32 w, h, samplepp, bitsps, orientation;
		size_t npixels;
		uint32* raster;
		TIFF *out= TIFFOpen("new.tif", "w");
		

		TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &w);
		TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h);
		TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplepp);
		TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitsps);
		TIFFGetField(tif, TIFFTAG_ORIENTATION, &orientation);
		if(out)
		{
			TIFFSetField (out, TIFFTAG_IMAGEWIDTH, w);  // set the width of the image
			TIFFSetField(out, TIFFTAG_IMAGELENGTH, h);    // set the height of the image
			TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, samplepp);   // set number of channels per pixel
			TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, bitsps);    // set the size of the channels
			TIFFSetField(out, TIFFTAG_ORIENTATION, orientation);    // set the origin of the image.
			npixels = w * h;
			raster = (uint32*) _TIFFmalloc(npixels * sizeof (uint32));
			if (raster != NULL) {
			    if (TIFFReadRGBAImage(tif, w, h, raster, 0)) {
				
			    }
			    _TIFFfree(raster);
			}
		}
		TIFFClose(tif);
	    }
	    return(0);
	}
	return(-1);
}

