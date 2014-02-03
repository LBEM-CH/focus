from image_plotter_func import *

	
	
def generatePerImagePlots(p_list, pdf_file):
	plotAngHists(p_list, pdf_file)

	
	
	
if __name__ == '__main__':

	if len(sys.argv) != 3:
		sys.exit("Usage: python angle_hist_plotter.py [filename] [filename out]")

	print "angle hist plotter launched"

	data = readFile(sys.argv[1])
	pp = PdfPages(sys.argv[2])
	
	generatePerImagePlots(data, pp)
	
	pp.close()
	
	print "angle hist plotter finished"
