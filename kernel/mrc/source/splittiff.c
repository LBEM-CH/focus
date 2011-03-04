#include "tiffio.h"
int main(int argc, char *argv[])
{
	if(argc > 1)
	{
	    TIFF* in = TIFFOpen(argv[1], "r");
	    if (in) {
		uint32 w, h, samplepp, bitsps, orientation;
		size_t npixels;
		uint32* raster;
		char* image;

		TIFFGetField(in, TIFFTAG_IMAGEWIDTH, &w);
		TIFFGetField(in, TIFFTAG_IMAGELENGTH, &h);
		TIFFGetField(in, TIFFTAG_SAMPLESPERPIXEL, &samplepp);
		TIFFGetField(in, TIFFTAG_BITSPERSAMPLE, &bitsps);
		TIFFGetField(in, TIFFTAG_ORIENTATION, &orientation);
		
		TIFF *out= TIFFOpen("new.tif", "w");
		if(out)
		{
			TIFFSetField(out, TIFFTAG_IMAGEWIDTH, w);  // set the width of the image
			TIFFSetField(out, TIFFTAG_IMAGELENGTH, h);    // set the height of the image
			TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, samplepp);   // set number of channels per pixel
			TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, bitsps);    // set the size of the channels
			TIFFSetField(out, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);    // set the origin of the image.
			npixels = w * h;
			raster = (uint32*) _TIFFmalloc(npixels * sizeof (uint32));
			if (raster != NULL) 
			{
			    if (TIFFReadRGBAImage(in, w, h, raster, 0)) 
			    {
				tsize_t linebytes = samplepp * w;     // length in memory of one row of pixel in the image.

				unsigned char *buf = NULL;        // buffer used to store the row of pixel information for writing to file
				//    Allocating memory to store the pixels of current row
				if (TIFFScanlineSize(out) != linebytes)
				    buf =(unsigned char *)_TIFFmalloc(linebytes);
				else
				    buf = (unsigned char *)_TIFFmalloc(TIFFScanlineSize(out));

				// We set the strip size of the file to be size of one row of pixels
				TIFFSetField(out, TIFFTAG_ROWSPERSTRIP, TIFFDefaultStripSize(out, w*samplepp));

				//Now writing image to the file one strip at a time
				uint32 row;
				for (row = 0; row < h; row++)
				{
				    memcpy(buf, &raster[(h-row-1)*linebytes], linebytes);    // check the index here, and figure out why not using h*linebytes
				    if (TIFFWriteScanline(out, buf, row, 0) < 0)
				    break;
				}	
			    }
			    _TIFFfree(raster);
			}
		}
		TIFFClose(in);
	    }
	    return(0);
	}
	return(-1);
}

