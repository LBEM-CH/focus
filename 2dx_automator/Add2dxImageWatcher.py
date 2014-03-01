from WatcherBaseClass import *
from EMAN2  import *
from sparx  import *


class Add2dxImageWatcher(WatcherBase):
	
	def __init__(self, refresh_time, wait_time, infolder, outfolder, log_file_name):
		self.refresh_time = refresh_time
		self.wait_time = wait_time
		self.infolder = infolder
		self.outfolder = outfolder
		self.log_file_name = log_file_name
		self.protein_name = "auto_"
		self.showSecondLattice = 1
		
		self.lock_2dx = threading.Lock()
		self.lock_eman2 = threading.Lock()
	
	
	def file_filter(self, filename):
		return filename.endswith(".mrc")
		
	def setShowSecondLattice(self, rhs):
		self.showSecondLattice = rhs
		print self.showSecondLattice
		
		
	def getNumberOfDoneImages(self, filename):
		return len(read_text_row(self.log_file_name))
		
		
	def getImageNumber(self, filename):
		data = read_text_file(self.log_file_name)
		print data
		return data.index(filename)
		
		
	def write_log(self, filename):
		image_count = len(read_text_row(self.log_file_name)) + 1
		image_2dx_name = self.outfolder + "/automatic/" + self.protein_name + str(image_count)
		open(self.log_file_name,"a").write(filename + "\t" + image_2dx_name + "\t" + time.strftime("%c") + '\n')
	
	def setDoMask(self, folder):
		f = open(folder + "/2dx_image.cfg", "r")
		lines = f.readlines()
		lines_out = []
		for l in lines:
			if not l.startswith("set domask ="):
				lines_out.append(l)
			else:
				lines_out.append('set domask = "y"')
		f.close()
		f_out = open(folder + "/2dx_image.cfg", "w")
		f_out.writelines(lines_out)
		f_out.close()
		
	def set_image_number(self, folder, number):
		f = open(folder + "/2dx_image.cfg", "r")
		lines = f.readlines()
		lines_out = []
		for l in lines:
			if not l.startswith("set imagenumber ="):
				lines_out.append(l)
			else:
				number_as_string = str(number)
				leading_zeros = 7 - len(number_as_string)
				number_as_string = leading_zeros * "0" + str(number) + "000"
				toadd = "set imagenumber = " + '"' + number_as_string + '"'
				lines_out.append(toadd)
				print toadd
		f.close()
		f_out = open(folder + "/2dx_image.cfg", "w")
		f_out.writelines(lines_out)
		f_out.close()
		
	def set_orignal_filename(self, folder, name):
		f = open(folder + "/2dx_image.cfg", "r")
		lines = f.readlines()
		lines_out = []
		for l in lines:
			if not l.startswith("set imagename_original ="):
				lines_out.append(l)
			else:
				toadd = "set imagename_original = " + '"' + name + '"'
				lines_out.append(toadd)
				print toadd
		f.close()
		f_out = open(folder + "/2dx_image.cfg", "w")
		f_out.writelines(lines_out)
		f_out.close()
		
		
	def image_added(self, filename, do_wait = True):
		print "2dx automatic processing launched for", filename
		time.sleep(15)
		if not os.path.exists(self.outfolder + "/automatic"):
			os.makedirs(self.outfolder + "/automatic" )
			shutil.copyfile( self.outfolder + "/2dx_master.cfg", self.outfolder + "/automatic/2dx_master.cfg" )
		image_count = self.getImageNumber(filename) + 1
		image_2dx_name = self.outfolder + "/automatic/" + self.protein_name + str(image_count)
		os.makedirs(image_2dx_name)
		#shutil.copyfile( self.infolder + "/" + filename, image_2dx_name + "/" + filename )
		shutil.copyfile( self.infolder + "/" + filename, image_2dx_name + "/" + self.protein_name + str(image_count) + ".mrc" )
		shutil.copyfile( self.outfolder + "/2dx_master.cfg", image_2dx_name + "/2dx_image.cfg" )
		
		self.lock_2dx.acquire()
		old_path = os.getcwd()
		os.chdir(self.outfolder)
		
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_initialize"' + "'" 
		os.system(command_2dx_image)
		
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_initialize_files"' + "'" 
		os.system(command_2dx_image)
		
		self.set_image_number(image_2dx_name, image_count)
		self.set_orignal_filename(image_2dx_name, filename)
		
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_initialize"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_initialize_files"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_fftrans"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_getDefTilt"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_getLattice"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_getspots1"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_unbend1"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_getspots"' + "'" 
		os.system(command_2dx_image)
		
		self.setDoMask(image_2dx_name)
		
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_unbend2"' + "'" 
		os.system(command_2dx_image)
		
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_fftrans"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_getspots1"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_unbend1"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_getspots"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_unbend2"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_applyCTF"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_generateMAP"' + "'" 
		os.system(command_2dx_image)
		
		os.chdir(old_path)
		self.lock_2dx.release()
		
		#filename_core = filename.split(".")[0]
		filename_core = image_2dx_name.split("/")[-1]
		dia_folder = image_2dx_name + "/automation_output"
		os.makedirs(dia_folder)
		self.copy_image_if_there(image_2dx_name + "/ManualMasking-CCmap.mrc", dia_folder + "/image.mrc")
		self.copy_image_if_there(image_2dx_name + "/CUT/" + filename_core + ".marked.merge.ps.mrc", dia_folder + "/defoci.mrc")
		self.copy_image_if_there(image_2dx_name + "/FFTIR/" + filename_core + ".red.fft.mrc", dia_folder + "/fft.mrc")
		self.copy_image_if_there(image_2dx_name + "/m" + filename_core + "-p1.mrc", dia_folder + "/map.mrc")
		
		self.lock_eman2.acquire()
		
		try:
			self.convert_mrc_to_png(dia_folder)
			self.fix_fft(dia_folder)
		except:
			pass
			
		self.lock_eman2.release()

		try:
			lattice = self.get_lattice_from_config(image_2dx_name + "/2dx_image.cfg")
			self.drawLattice(dia_folder + "/fft.png", dia_folder + "/fft_lattice.gif", lattice)
			
			if self.showSecondLattice == 1:
				lattice2 = self.get_second_lattice_from_config(image_2dx_name + "/2dx_image.cfg")
				if not ( (abs(lattice2[0]<0.1)) and (abs(lattice2[1]<0.1)) and (abs(lattice2[2]<0.1)) and (abs(lattice2[3]<0.1)) ):
					self.drawSecondLattice(dia_folder + "/fft_lattice.gif", dia_folder + "/fft_lattice.gif", lattice2)
		
			ctf = self.get_ctf_from_config(image_2dx_name + "/2dx_image.cfg")
			self.drawCTF(dia_folder + "/fft.png", dia_folder + "/fft_ctf.gif", ctf)
		
			tltaxis = self.get_tltaxis_from_config(image_2dx_name + "/2dx_image.cfg")
			self.drawTiltAxisOnDefo(dia_folder + "/defoci.png", dia_folder + "/defoci_axis.gif", tltaxis)
		except:
			pass
			
		print "2dx automatic processing done for", filename
		
	def copy_image_if_there(self, src, dest):
		if os.path.exists(src):
			shutil.copyfile( src, dest )
		else:
			print "file not there:", src
			
			
	def convert_mrc_to_png(self, folder):
		old_path = os.getcwd()
		os.chdir(folder)
		content = os.listdir(".")
		for f in content:
			if f.split(".")[-1] == "mrc":
				os.system("e2proc2d.py " + f + " " + f.split(".")[0] + ".png" )
				os.system("convert " + f.split(".")[0] + ".png " + f.split(".")[0] + ".gif" )
		os.chdir(old_path)
		
		
	def drawCTF(self, infile, outfile, ctf):
		from PIL import Image, ImageDraw
		im = Image.open(infile)
		nx = im.size[0]
		ny = im.size[1]
		im2 = Image.new("RGB", (nx,ny), "white")
		dx = 16
		max_pixel = float(max(im.getdata()))
		for x in range(nx):
			for y in range(ny):
				col = int(im.getpixel((x,y))/max_pixel*255)
				im2.putpixel((x,y), (col,col,col))
		draw = ImageDraw.Draw(im2)
		
		defocusX = float(ctf['defX'])
		defocusY = float(ctf['defY'])
		astigmatism = float(ctf['ast'])
		
		n = 20
		stepSize = 2*float(ctf['step'])
		phacon = float(ctf['phacon'])
		Cs = float(ctf['CS'])
		kV = float(ctf['KV'])
		magnification = float(ctf['mag'])
		
		N = 2000
		dz = 0.0
		theta = 0.0
		k0 = 0.0
		cost=0.0
		sint=0.0
		imageWidth = nx

		kV*=1000.0
		stepSize*=1.0e4
		lam = 12.3/sqrt(kV+kV*kV*1.0e-6)
		Cs*=1.0e7;
		astigmatism*=pi/180.0;
		
		for i in range(0,N):
			theta = i/float(N)*2.0*pi;
			cost = cos(theta);
			sint = sin(theta);
			dz = 0.5*(defocusX+defocusY+cos(2.0*(theta-astigmatism))*(defocusX-defocusY));
			
			for j in range(1,n+1):
				
				if dz>0.0:
					k0 = imageWidth/magnification*stepSize/(lam)*sqrt(abs(1.0/Cs*(dz-sqrt(abs(dz*dz+2.0*lam*Cs*(acos(phacon)/pi-float(j)))))))
				else:
					k0 = imageWidth/magnification*stepSize/(lam)*sqrt(abs(1.0/Cs*(dz+sqrt(abs(dz*dz+2.0*lam*Cs*(acos(phacon)/pi+float(j)))))));
			
				u = k0*cost
				v = k0*sint
				
				if u>0.0:
					u+=0.5
				else:
					u-=0.5
					
				if v>0.0:
					v+=0.5
				else:
					v-=0.5

				u = int(u) + nx/2
				v = -int(v) + ny/2
				if u>1 and u<(nx-1) and v>1 and v<(ny-1):
					im2.putpixel((u,v), (0,255,17))
					im2.putpixel((u+1,v+1), (0,255,17))
					im2.putpixel((u+1,v), (0,255,17))
					im2.putpixel((u+1,v-1), (0,255,17))
					im2.putpixel((u,v-1), (0,255,17))
					im2.putpixel((u,v+1), (0,255,17))
					im2.putpixel((u-1,v-1), (0,255,17))
					im2.putpixel((u-1,v), (0,255,17))
					im2.putpixel((u-1,v+1), (0,255,17))
					
		w = 650
		im2.crop((nx/2-w, ny/2-w, nx/2+w, ny/2+w)).save(outfile)
		
		
	def drawTiltAxisOnDefo(self, infile, outfile, tltaxis):
		from PIL import Image, ImageDraw, ImageFont
		im = Image.open(infile)
		nx = im.size[0]
		ny = im.size[1]
		im2 = Image.new("RGB", (nx,ny), "white")
		max_pixel = float(max(im.getdata()))
		for x in range(nx):
			for y in range(ny):
				col = int(im.getpixel((x,y))/max_pixel*255)
				im2.putpixel((x,y), (col,col,col))
		draw = ImageDraw.Draw(im2)
		
		tltaxis = -tltaxis
		color = (1,3,230)
		
		if abs(tltaxis)<45:
			dy = tan(tltaxis*pi/180.0) * nx/2
			draw.line( (0,ny/2-dy,nx-1,ny/2+dy), fill=color, width=4)
		else:
			if tltaxis<0:
				tltaxis = -90 + tltaxis
				dx = tan(tltaxis*pi/180.0) * nx/2
				draw.line( (nx/2+dx,0,nx/2-dx,ny-1), fill=color, width=4)
			else:
				tltaxis = 90 - tltaxis
				dx = tan(tltaxis*pi/180.0) * nx/2
				draw.line( (nx/2-dx,0,nx/2+dx,ny-1), fill=color, width=4)
			
		png_name = outfile.split(".")[0] + ".png"
		im2.save(png_name)
		os.system("convert " + png_name + " " + outfile )
	
	
	def drawLattice(self, infile, outfile, lattice):
		from PIL import Image, ImageDraw
		im = Image.open(infile)
		nx = im.size[0]
		ny = im.size[1]
		im2 = Image.new("RGB", (nx,ny), "white")
		dx = 16
		max_pixel = float(max(im.getdata()))
		for x in range(nx):
			for y in range(ny):
				col = int(im.getpixel((x,y))/max_pixel*255)
				im2.putpixel((x,y), (col,col,col))
		draw = ImageDraw.Draw(im2)
		order = 12
		for i in range(-order,order+1,1):
			for j in range(-order,order+1,1):
				
				if i==0 and j==0:
					color = (0,0,0)
				elif i==1 and j==0:
					color = (230,26,13)
				elif i==0 and j==1:
					color = (1,3,230)
				else:
					color = (0,255,17)
					
				u = lattice[0]*float(i) + lattice[2]*float(j) - dx/2
				v = lattice[1]*float(i) + lattice[3]*float(j) - dx/2
				draw.ellipse((u+nx/2, -v+ny/2-dx, u+dx+nx/2, -v+ny/2), outline=color, fill=None)
				draw.ellipse((u+nx/2-1, -v+ny/2-dx-1, u+dx+nx/2+1, -v+ny/2+1), outline=color, fill=None)
		w = 260
		im2.crop((nx/2-w, ny/2-w, nx/2+w, ny/2+w)).save(outfile)
		
		
	def drawSecondLattice(self, infile, outfile, lattice):
		from PIL import Image, ImageDraw
		im = Image.open(infile)
		nx = im.size[0]
		ny = im.size[1]
		im2 = Image.new("RGB", (nx,ny), "white")
		dx = 16
		max_pixel = float(max(im.getdata()))
		for x in range(nx):
			for y in range(ny):
				col = int(im.getpixel((x,y))/max_pixel*255)
				im2.putpixel((x,y), (col,col,col))
		draw = ImageDraw.Draw(im2)
		order = 12
		for i in range(-order,order+1,1):
			for j in range(-order,order+1,1):
				
				if i==0 and j==0:
					color = (0,0,0)
				elif i==1 and j==0:
					color = (230,26,13)
				elif i==0 and j==1:
					color = (1,3,230)
				else:
					color = (255,0,17)
					
				color = (255,255,255)
					
				u = lattice[0]*float(i) + lattice[2]*float(j) - dx/2
				v = lattice[1]*float(i) + lattice[3]*float(j) - dx/2
				draw.ellipse((u+nx/2, -v+ny/2-dx, u+dx+nx/2, -v+ny/2), outline=color, fill=None)
				draw.ellipse((u+nx/2-1, -v+ny/2-dx-1, u+dx+nx/2+1, -v+ny/2+1), outline=color, fill=None)
		w = 260
		im2.crop((nx/2-w, ny/2-w, nx/2+w, ny/2+w)).save(outfile)
		
		
	def fix_fft(self, dia_folder):
		image = get_image(dia_folder + "/fft.mrc").get_fft_amplitude()
		image_new = model_blank(image.get_xsize(), image.get_ysize())
		
		nx = image.get_xsize()
		for i in range(nx/2):
			for j in range(nx/2):
				image_new.set_value_at(i, j+nx/2, image(i,j))
				image_new.set_value_at(i, j, image(i,j+nx/2))
				image_new.set_value_at(i+nx/2, j+nx/2, image(i+nx/2,j))
				image_new.set_value_at(i+nx/2, j, image(i+nx/2,j+nx/2))
	
		image_new.write_image( dia_folder + "/fft.png")
		
		
	def get_ctf_from_config(self, config_name):
		content = read_text_row(config_name)
		for c in content:
			if len(c)>1 and c[1] == "defocus":
				defocus = c[3].split(",")

		defocus[0] = float(defocus[0][1:])
		defocus[1] = float(defocus[1])
		defocus[2] = float(defocus[2][:-1])
		
		for c in content:
			if len(c)>1 and c[1] == "stepdigitizer":
				step = float(c[3][1:-1])
			if len(c)>1 and c[1] == "phacon":
				phacon = float(c[3][1:-1])
			if len(c)>1 and c[1] == "CS":
				CS = float(c[3][1:-1])
			if len(c)>1 and c[1] == "KV":
				KV = float(c[3][1:-1])
			if len(c)>1 and c[1] == "magnification":
				magnification = float(c[3][1:-1])
				
		result = {'defX': defocus[0], 'defY': defocus[1], 'ast': defocus[2], 'step': step, 'phacon': phacon, 'CS': CS, 'KV': KV, 'mag': magnification}
		return result
		
		
	def get_tltaxis_from_config(self, config_name):
		content = read_text_row(config_name)
		for c in content:
			if len(c)>1 and c[1] == "TLTAXIS":
				tltaxis = float(c[3][1:-1])
		return tltaxis
		
		
	def get_lattice_from_config(self, config_name):
		content = read_text_row(config_name)
		lattice = []
		for c in content:
			
			if len(c) > 2 and (c[0] != "#"):
				c_string = ""
				for c_loc in c:				
					c_string += str(c_loc)
				line_without_space = c_string.replace(" ", "")
				if line_without_space.startswith("setlattice"):
					data = line_without_space.split("=")[1][1:-1]
					for d in data.split(","):
						try:
							lattice.append(float(d))
						except:
							lattice.append(0)
		return lattice
	
	
	def get_second_lattice_from_config(self, config_name):
		content = read_text_row(config_name)
		lattice = []
		for c in content:
			
			if len(c) > 2 and (c[0] != "#"):
				c_string = ""
				for c_loc in c:				
					c_string += str(c_loc)
				line_without_space = c_string.replace(" ", "")
				if line_without_space.startswith("setsecondlattice"):
					data = line_without_space.split("=")[1][1:-1]
					for d in data.split(","):
						try:
							lattice.append(float(d))
						except:
							lattice.append(0)
		return lattice
		
	
