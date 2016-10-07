import Image

import os
import sys


if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Missuseage detected")

	outfile = sys.argv[1]
	
	new_im = Image.new('RGB', (4400,2600), "white")
	
	im = Image.open("frames/dia/cc_start.jpg")
	im.thumbnail((1000,1000))
	new_im.paste(im, (100,200))
	
	im = Image.open("frames/dia/cc_half.jpg")
	im.thumbnail((1000,1000))
	new_im.paste(im, (1200,200))
	
	im = Image.open("frames/dia/cc_end.jpg")
	im.thumbnail((1000,1000))
	new_im.paste(im, (2300,200))
	
	im = Image.open("frames/dia/prof_first.jpg")
	im = im.resize((1000,1295), Image.ANTIALIAS)
	new_im.paste(im, (100,1200))
	
	im = Image.open("frames/dia/prof_half.jpg")
	im = im.resize((1000,1295), Image.ANTIALIAS)
	new_im.paste(im, (1200,1200))
	
	im = Image.open("frames/dia/prof_last.jpg")
	im = im.resize((1000,1295), Image.ANTIALIAS)
	new_im.paste(im, (2300,1200))
	
	im = Image.open("frames/sd.jpg")
	im = im.resize((int(1.4*800),int(1.4*600)), Image.ANTIALIAS)
	new_im.paste(im, (3250,1600))
	
	
	new_im.save(outfile)
