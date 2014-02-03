import sys
from pylab import *
from matplotlib import rc
from matplotlib.cm import *

from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import NullFormatter

import math

class Particle:
	
	def __init__(self, data_string):
		self.part_nb = float(data_string[0])
		self.im_nb = float(data_string[1])
		self.posx = float(data_string[2])
		self.posy = float(data_string[3])
		self.used = int(data_string[4])
		self.last_change = float(data_string[5])
		self.shiftx = float(data_string[6])
		self.shifty = float(data_string[7])
		self.init_tltaxis = float(data_string[8])
		self.init_tltang = normalizeTilt(float(data_string[9]))
		self.init_taxa = float(data_string[10])
		self.old_tltaxis = float(data_string[11])
		self.old_tltang = normalizeTilt(float(data_string[12]))
		self.old_taxa = float(data_string[13])
		self.new_tltaxis = float(data_string[14])
		self.new_tltang = normalizeTilt(float(data_string[15]))
		self.new_taxa = float(data_string[16])
		self.sim_measure = float(data_string[17])
		self.qual_measure = float(data_string[18])
		self.cons_measure = float(data_string[19])
		self.weight = float(data_string[20])
		self.container = float(data_string[21])
		self.is_cm_part = float(data_string[22])
		self.density = float(data_string[23])
		self.class_mra = float(data_string[24])
		self.class_true = float(data_string[25])


def normalizeTilt(tilt):
	if tilt > 90:
		tilt -= 180
	tilt = math.fabs(tilt)
	return tilt
	
	
	
def readFile(filename):
	part_list = []
	infile = open( filename, "r" )
	infile.readline()
	for line in infile:
		part_list.append(Particle(line.split()))
	return part_list
	
	
def generatePerImageLists(part_list):
	image_lists = []
	old_image_num = -1
	for p in part_list:
		if old_image_num != p.im_nb:
			old_image_num = p.im_nb
			image_lists.append([])
		image_lists[-1].append(p)
	return image_lists
	
	
def plotPositions(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		z.append('b')
	scatter(x,y,s=20,c=z, marker = 'o');
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Particle Positions")
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	
	
def plotUsedForReconstruction(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		if (p.used == 1):
			z.append(1)
		else:
			z.append(0)
	scatter(x,y,s=20,c=z, marker = 'o', cmap=RdYlBu_r);
	plt.colorbar()
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Particles used for reconstruction")
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()


def plotIsCMParticle(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		if (p.is_cm_part == 1):
			z.append(1)
		else:
			z.append(0)
	scatter(x,y,s=20,c=z, marker = 'o', cmap=RdYlBu_r);
	plt.colorbar()
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": CM Particle")
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()


def plotShifts(p_list, pdf_file):
	frame1 = plt.gca()
	dx = 25
	x=[]; y=[]
	for p in p_list:
		if (p.used == 1):
			x.append(p.posx)
			y.append(p.posy)
	plot(x, y, '.', linewidth=1, c='r')
	for p in p_list:
		if (p.used == 1):
			plot([p.posx, p.posx+dx*p.shiftx], [p.posy, p.posy+dx*p.shifty], c='b')
			x.append(p.posx)
			y.append(p.posy)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Shifts")
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	
	
def plotSimilarity(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	sum=0;
	count=0;
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		z.append(p.sim_measure)
		sum += p.sim_measure
		count += 1
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Similarity (mean=" + str(sum/count) + ")" )
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	
	
def plotClass(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		z.append(p.class_mra)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Class (tiltang = " + str(normalizeTilt(p_list[0].new_tltang)) + ")" )
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	
def plotClassHist(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		z.append(p.class_mra)
	hist(z)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Class Hist (tiltang = " + str(normalizeTilt(p_list[0].new_tltang)) + ")" )
	pdf_file.savefig()
	plt.clf()


def plotDensity(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	sum=0;
	count=0;
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		z.append(p.density)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Density")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	
	
def plotQuality(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		z.append(p.qual_measure)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Quality")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()


def plotConsistency(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		z.append(p.cons_measure)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Consistency")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()


def plotWeightAll(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		z.append(p.weight)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Weight All")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()


def plotWeight(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		if (p.used == 1):
			x.append(p.posx)
			y.append(p.posy)
			z.append(p.weight)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Weight")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	
	
def plotContainerNumber(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		x.append(p.posx)
		y.append(p.posy)
		z.append(p.container)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Container Number")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	
	
def plotTLTAXIS(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		#if (p.used == 1):
			x.append(p.posx)
			y.append(p.posy)
			z.append(p.new_tltaxis)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": TLTAXIS")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	

def plotTLTANG(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		#if (p.used == 1):
		if True:
			x.append(p.posx)
			y.append(p.posy)
			z.append(p.new_tltang)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": TLTANG")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()


def plotTAXA(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		#if (p.used == 1):
			x.append(p.posx)
			y.append(p.posy)
			z.append(p.new_taxa)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = RdYlBu_r)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": TAXA")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()


def plotAngularChange(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		if (p.used == 1):
			x.append(p.posx)
			y.append(p.posy)
			change = sqrt( (p.new_tltaxis-p.init_tltaxis)**2 + (p.new_tltang-p.init_tltang)**2 + (p.new_taxa-p.init_taxa)**2)
			z.append(change)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = jet);
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Changes")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	
	
def plotLastAngularChange(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; y=[]; z=[]
	for p in p_list:
		if (p.used == 1):
			x.append(p.posx)
			y.append(p.posy)
			z.append(p.last_change)
	scatter(x,y,s=20,c=z, marker = 'o', cmap = jet)
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Last Changes")
	plt.colorbar()
	frame1.axes.get_xaxis().set_visible(False)
	frame1.axes.get_yaxis().set_visible(False)
	pdf_file.savefig()
	plt.clf()
	

def plotSimMeasureHist(p_list, pdf_file):
	frame1 = plt.gca()
	x=[]; x1=[]; x2=[];
	for p in p_list:
		x.append(p.sim_measure)
		if (p.container == 1):
			x1.append(p.sim_measure)
		if (p.container == 2):
			x2.append(p.sim_measure)
	n, bins, patches = hist(x, fill=True, color=['grey'], alpha=0.5)
	hist([x1,x2], bins, color=['blue', 'red'])
	plt.title('Image ' + str(int(p_list[0].im_nb)) + ": Histogram of Similarity Measures")
	pdf_file.savefig()
	plt.clf()

	
def plotShiftDetails(p_list, pdf_file):
	frame1 = plt.gca()
	x = []
	y = []

	
	for p in p_list:
		if (p.used == 1):
			x.append(p.shiftx)
			y.append(p.shifty)
	
	nullfmt = NullFormatter()
	
	# definitions for the axes
	left, width = 0.1, 0.65
	bottom, height = 0.1, 0.65
	bottom_h = left_h = left+width+0.02

	rect_scatter = [left, bottom, width, height]
	rect_histx = [left, bottom_h, width, 0.2]
	rect_histy = [left_h, bottom, 0.2, height]

	# start with a rectangular Figure
	plt.figure(1, figsize=(8,8))

	axScatter = plt.axes(rect_scatter)
	axHistx = plt.axes(rect_histx)
	axHisty = plt.axes(rect_histy)

	# no labels
	axHistx.xaxis.set_major_formatter(nullfmt)
	axHisty.yaxis.set_major_formatter(nullfmt)

	# the scatter plot:
	
	z = []	
	for i in range(0,len(x)):
		count = 0
		for j in range(0,len(x)):
			if ( x[i]==x[j] and y[i]==y[j] ):
				count +=1
		z.append( count )
		
	max_z = max(z)
	max_size = 750.
	scale = max_size/max_z
		
	for i in range(0,len(x)) :
		z[i] = z[i] * scale
	
	axScatter.scatter(x,y,s=z,c=z, marker = 'o', cmap = RdYlBu_r)
#	axScatter.scatter(x,y,s=z, marker = 'o')

	# now determine nice limits by hand:
	binwidth = 0.25
	xymax = np.max( [np.max(np.fabs(x)), np.max(np.fabs(y))] )
	lim = ( int(xymax/binwidth) + 1) * binwidth

	axScatter.set_xlim( (-lim, lim) )
	axScatter.set_ylim( (-lim, lim) )

	bins = np.arange(-lim, lim + binwidth, binwidth)
	axHistx.hist(x, bins=bins)
	axHisty.hist(y, bins=bins, orientation='horizontal')

	axHistx.set_xlim( axScatter.get_xlim() )
	axHisty.set_ylim( axScatter.get_ylim() )
	
	pdf_file.savefig()
	plt.clf()


def plotHists(p_list, pdf_file):
	frame1 = plt.gca()
	ang1_1=[]; ang1_2=[]; ang1=[];
	ang2_1=[]; ang2_2=[]; ang2=[];
	ang3_1=[]; ang3_2=[]; ang3=[];
	sx_1=[]; sx_2=[]; sx=[]; 
	sy_1=[]; sy_2=[]; sy=[]; 
	sr_1=[]; sr_2=[]; sr=[];
	for p in p_list:
		if (p.used == 1):
			ang1.append(p.new_tltaxis)
			ang2.append(p.new_tltang)
			ang3.append(p.new_taxa)
			sx.append(p.shiftx)
			sy.append(p.shifty)
			sr.append(p.sim_measure)
			if (p.container == 1):
				ang1_1.append(p.new_tltaxis)
				ang2_1.append(p.new_tltang)
				ang3_1.append(p.new_taxa)
				sx_1.append(p.shiftx)
				sy_1.append(p.shifty)
				sr_1.append(sqrt(p.shiftx**2 + p.shifty**2))
			if (p.container == 2):
				ang1_2.append(p.new_tltaxis)
				ang2_2.append(p.new_tltang)
				ang3_2.append(p.new_taxa)
				sx_2.append(p.shiftx)
				sy_2.append(p.shifty)
				sr_2.append(sqrt(p.shiftx**2 + p.shifty**2))
									
	matplotlib.rc('xtick', labelsize=5) 
	matplotlib.rc('ytick', labelsize=5)
	
	suptitle('Image ' + str(int(p_list[0].im_nb)) + ": Distributions", fontsize=20)
			
	subplot(231)
	n, bins, patches = hist(ang1, bins=30, fill=True, color=['grey'], alpha=0.5)
	hist([ang1_1,ang1_2], bins, color=['blue', 'red'])
	plt.title('TLTAXIS distribution', fontsize=10)
	
	subplot(232)
	n, bins, patches = hist(ang2, bins=30, fill=True, color=['grey'], alpha=0.5)
	hist([ang2_1,ang2_2], bins, color=['blue', 'red'])
	plt.title('TLTANG distribution', fontsize=10)
	
	subplot(233)
	n, bins, patches = hist(ang3, bins=30, fill=True, color=['grey'], alpha=0.5)
	hist([ang3_1,ang3_2], bins, color=['blue', 'red'])
	plt.title('TAXA distribution', fontsize=10)
	
	subplot(234)
	#bins = list(range(-5,6,1))
	n, bins, patches = hist(sx, fill=True, color=['grey'], alpha=0.5)
	hist([sx_1,sx_2], bins, color=['blue', 'red'])
	plt.title('x shift distribution', fontsize=10)
	
	subplot(235)
	n, bins, patches = hist(sy, fill=True, color=['grey'], alpha=0.5)
	hist([sy_1,sy_2], bins, color=['blue', 'red'])
	plt.title('y shift distribution', fontsize=10)
	
	subplot(236)
	n, bins, patches = hist(sr, fill=True, color=['grey'], alpha=0.5)
	hist([sr_1,sr_2], bins, color=['blue', 'red'])
	plt.title('sim measure distribution', fontsize=10)
	
	subplots_adjust(hspace=.2)
	pdf_file.savefig()
	plt.clf()


def plotAngCorr(p_list, pdf_file):
	frame1 = plt.gca()
	
	ang1 = []; ang2 = []; ang3 = [];
	sim = []
	s_x = []
	s_y = []
	
	for p in p_list:
		if (p.used == 1):
			ang1.append(p.new_tltaxis)
			ang2.append(p.new_tltang)
			ang3.append(p.new_taxa)
			sim.append(p.sim_measure)
			s_x.append(p.shiftx)
			s_y.append(p.shifty)
	
	subplot(231)
	hist2d(ang1, ang2, bins=20)
	plt.title('tiltaxis - tltang', fontsize=10)

	subplot(232)
	hist2d(ang1, ang3, bins=20)
	plt.title('tiltaxis - taxa', fontsize=10)
	
	subplot(233)
	hist2d(ang2, ang3, bins=20)
	plt.title('tiltang - taxa', fontsize=10)
	
	subplot(234)
	hist2d(s_x, sim, bins=21)
	plt.title('sx - sim', fontsize=10)

	subplot(235)
	hist2d(s_y, sim, bins=21)
	plt.title('sy - sim', fontsize=10)
	

	
	subplots_adjust(hspace=.2)
	pdf_file.savefig()
	plt.clf()
		
	
	
def plotAngHists(p_list, pdf_file):
	frame1 = plt.gca()
	ang1_1=[]; ang1_2=[]; ang1=[];
	ang2_1=[]; ang2_2=[]; ang2=[];
	ang3_1=[]; ang3_2=[]; ang3=[];
	for p in p_list:
		if (p.used == 1):
			ang1.append(p.new_tltaxis)
			ang2.append(normalizeTilt(p.new_tltang))
			ang3.append(p.new_taxa)
			

	matplotlib.rc('xtick', labelsize=5) 
	matplotlib.rc('ytick', labelsize=5)

	suptitle('Angular Distributions', fontsize=20)

	subplot(131)
	n, bins, patches = hist(ang1, fill=True, bins=90, color=['grey'], alpha=0.5)
	hist([ang1_1,ang1_2], bins, color=['blue', 'red'])
	plt.title('TLTAXIS distribution', fontsize=10)

	subplot(132)
	n, bins, patches = hist(ang2, fill=True, bins=40, color=['grey'], alpha=0.5)
	hist([ang2_1,ang2_2], bins, color=['blue', 'red'])
	plt.title('TLTANG distribution', fontsize=10)

	subplot(133)
	n, bins, patches = hist(ang3, fill=True, bins=90, color=['grey'], alpha=0.5)
	hist([ang3_1,ang3_2], bins, color=['blue', 'red'])
	plt.title('TAXA distribution', fontsize=10)

	subplots_adjust(hspace=.2)
	pdf_file.savefig()
	plt.clf()
	
