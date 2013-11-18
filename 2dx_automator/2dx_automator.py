#!/usr/bin/python

from Tkinter import *
import tkFileDialog
import tkMessageBox

from pylab import plt, plot, subplot, figure, hist

#from threading import Thread
import thread

from PIL import Image, ImageDraw, ImageTk

from EMAN2  import *
from sparx  import *

from Add2dxImageWatcher import *

import os
import datetime


n = 508
n_small = 400

import tkSimpleDialog
		

def bind(widget, event):
	def decorator(func):
		widget.bind(event, func)
		return func
	return decorator
	
	
def tbox(master):
	class TBOX(object):
		def __init__(self, info):
			self.msgbox = Toplevel(master)
			
			self.msgbox.title("2dx_automator Project Statistics Overview")
			
			#label0 = Label(self.msgbox, text=info)
			#label0.pack()
			#self.entry0 = Entry(self.msgbox)
			#self.entry0.pack()
			
			nx = int(800 * 0.7)
			ny = int(600 * 0.7)
				
			self.def_label = Label(self.msgbox, image=master.default_tkimage)
			self.def_label.grid(row=0, column=0, padx=10, pady=20)
		
			self.def_image = ImageTk.PhotoImage(Image.open(master.output_dir + "/automatic/tilts.tif").resize((nx,ny),Image.ANTIALIAS))
			self.def_label.configure(image=self.def_image)
			
			self.def_label2 = Label(self.msgbox, image=master.default_tkimage)
			self.def_label2.grid(row=0, column=1, padx=10, pady=20)
		
			self.def_image2 = ImageTk.PhotoImage(Image.open(master.output_dir + "/automatic/defoci.tif").resize((nx,ny),Image.ANTIALIAS))
			self.def_label2.configure(image=self.def_image2)
			
			self.def_label3 = Label(self.msgbox, image=master.default_tkimage)
			self.def_label3.grid(row=0, column=2, padx=10, pady=20)
		
			self.def_image3 = ImageTk.PhotoImage(Image.open(master.output_dir + "/automatic/qval.tif").resize((nx,ny),Image.ANTIALIAS))
			self.def_label3.configure(image=self.def_image3)
			
			self.def_label4 = Label(self.msgbox, image=master.default_tkimage)
			self.def_label4.grid(row=1, column=0, padx=10, pady=10)
		
			self.def_image4 = ImageTk.PhotoImage(Image.open(master.output_dir + "/automatic/tilt_qval.tif").resize((nx,ny),Image.ANTIALIAS))
			self.def_label4.configure(image=self.def_image4)
			
			self.def_label5 = Label(self.msgbox, image=master.default_tkimage)
			self.def_label5.grid(row=1, column=1, padx=10, pady=10)
		
			self.def_image5 = ImageTk.PhotoImage(Image.open(master.output_dir + "/automatic/defocis.tif").resize((nx,ny),Image.ANTIALIAS))
			self.def_label5.configure(image=self.def_image5)
			
			self.def_label6 = Label(self.msgbox, image=master.default_tkimage)
			self.def_label6.grid(row=1, column=2, padx=10, pady=10)
		
			self.def_image6 = ImageTk.PhotoImage(Image.open(master.output_dir + "/automatic/qvals.tif").resize((nx,ny),Image.ANTIALIAS))
			self.def_label6.configure(image=self.def_image6)
			
			button0 = Button(self.msgbox, text='Close',command=self.b0_action)
			button0.grid(row=2, column=4, padx=10, pady=10)
			
		def b0_action(self):
			#master.username = self.entry0.get()
			self.msgbox.destroy()
	return TBOX

	
class Auto2dxGUI(Frame):
	def __init__(self, parent):		
		Frame.__init__(self, parent, background="white")
		self.index_selected = 0
		self.parent = parent
		self.initUI()
		
		
		
	def getFolders(self):
		self.input_dir = tkFileDialog.askdirectory(parent=self.parent, title="Please select an input directory for 2dx_automator")
		if len(self.input_dir)==0:
			raise SystemExit("No input directory selected")
		
		self.output_dir = tkFileDialog.askdirectory(parent=self.parent, title="Please select an output directory for 2dx_automator")
		if len(self.output_dir)==0:
			raise SystemExit("No output directory selected")
			
			
	def initLayout(self):
		
		self.pack(fill=BOTH, expand=1)
		self.topframe = Frame(self, relief=RAISED, borderwidth=1)
		self.topframe.pack(fill=BOTH)
		
		self.topleftframe = Frame(self.topframe)
		self.topleftframe.pack(side=LEFT)
		
		self.toprightframe = Frame(self.topframe)
		self.toprightframe.pack(side=RIGHT)
		
		self.centralframe = Frame(self)
		self.centralframe.pack(expand=1, fill=BOTH)
		
		self.centralleftframe = Frame(self.centralframe)
		self.centralleftframe.pack(side=LEFT)
		
		self.lowleftframe = Frame(self.centralleftframe)
		self.lowleftframe.pack(side=BOTTOM)
		
		self.centralrightframe = Frame(self.centralframe, relief=RAISED, borderwidth=2)
		self.centralrightframe.pack(pady=5)
		
		self.centralrightframe3 = Frame(self.centralrightframe)
		self.centralrightframe3.pack(side=LEFT)
		
		self.centralrightframe1 = Frame(self.centralrightframe)
		self.centralrightframe1.pack()
		
		self.centralrightframe2 = Frame(self.centralrightframe)
		self.centralrightframe2.pack()
		
		
	def getAllQVals(self):
		qvals = []
		for f in self.image_dirs:
			config_name = f + "/2dx_image.cfg"
			content = read_text_row(config_name)
			for c in content:
				if len(c)>1 and c[1] == "QVAL":
					qvals.append(abs(float(c[3][1:-1])))
		return qvals	

	def getAllTiltAngles(self):
		tilt_anlges = []
		for f in self.image_dirs:
			config_name = f + "/2dx_image.cfg"
			content = read_text_row(config_name)
			for c in content:
				if len(c)>1 and c[1] == "TLTANG":
					tilt_anlges.append(abs(float(c[3][1:-1])))
		return tilt_anlges
	
	def getAllDefoci(self):
		defoci = []
		for f in self.image_dirs:
			config_name = f + "/2dx_image.cfg"
			content = read_text_row(config_name)
			for c in content:
				if len(c)>1 and c[1] == "defocus":
					defocus = c[3].split(",")
					defoci.append(float(defocus[0][1:]))
		return defoci
			
		
	def getProjectStat(self):
		tilts = self.getAllTiltAngles()
		ang_min = 0
		ang_max = 70
		ang_dx = 2
		
		hist(tilts, bins=(ang_max-ang_min)/ang_dx, range=(ang_min,ang_max), facecolor='blue', alpha=0.5)
		plt.title('Tilt Angle Histogram')
		plt.xlabel('Tilt Angle')
		plt.ylabel('Counts')
		plt.savefig(self.output_dir + "/automatic/tilts.tif")
		plt.close()
		
		def_min = 0
		def_max = 50000
		def_dx = 1000
		defs = self.getAllDefoci()
		qvals = self.getAllQVals()
		
		defs_pertilt = [[],[],[],[]]
		angs_pertilt = [[],[],[],[]]
		qvals_pertilt = [[],[],[],[]]
		
		for i in range(len(defs)):
			if abs(tilts[i]) < 10:
				angs_pertilt[0].append(tilts[i])
				defs_pertilt[0].append(defs[i])
				qvals_pertilt[0].append(qvals[i])
			elif abs(tilts[i]) < 20 and abs(tilts[i])>=10:
				angs_pertilt[1].append(tilts[i])
				defs_pertilt[1].append(defs[i])
				qvals_pertilt[1].append(qvals[i])
			elif abs(tilts[i]) < 30 and abs(tilts[i])>=20:
				angs_pertilt[2].append(tilts[i])
				defs_pertilt[2].append(defs[i])
				qvals_pertilt[2].append(qvals[i])
			else:
				angs_pertilt[3].append(tilts[i])
				defs_pertilt[3].append(defs[i])
				qvals_pertilt[3].append(qvals[i])
		
		hist(defs, bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
		plt.title('Defoci Histogram')
		plt.xlabel('Defocus')
		plt.ylabel('Counts')
		plt.savefig(self.output_dir + "/automatic/defoci.tif")
		plt.close()
		
		qval_min = min(qvals)
		qval_max = max(qvals)
		qval_n_bins = 40
		
		hist(qvals, bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
		plt.title('QVal Histogram')
		plt.xlabel('QVal')
		plt.ylabel('Counts')
		plt.savefig(self.output_dir + "/automatic/qval.tif")
		plt.close()
		
		plt.subplots_adjust(hspace=0.6)
		if len(qvals_pertilt[0])>0:
			plt.subplot(411)
			hist(qvals_pertilt[0], bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
			plt.title('QVAL Histogram (0-10 degrees)')
			plt.ylabel('Counts')
			if len(qvals_pertilt[1])==0 and len(qvals_pertilt[2])==0 and len(qvals_pertilt[3])==0:
				plt.xlabel('QVAL')
		if len(qvals_pertilt[1])>0:
			plt.subplot(412)
			hist(qvals_pertilt[1], bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
			plt.title('QVAL Histogram (10-20 degrees)')
			plt.ylabel('Counts')
			if len(qvals_pertilt[2])==0 and len(qvals_pertilt[3])==0:
				plt.xlabel('QVAL')
		if len(qvals_pertilt[2])>0:
			plt.subplot(413)
			hist(qvals_pertilt[2], bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
			plt.title('QVAL Histogram (20-30 degrees)')
			plt.ylabel('Counts')
			if len(qvals_pertilt[3])==0:
				plt.xlabel('QVAL')
		if len(qvals_pertilt[3])>0:
			plt.subplot(414)
			hist(qvals_pertilt[3], bins=qval_n_bins, range=(qval_min,qval_max), facecolor='red', alpha=0.5)
			plt.title('QVAL Histogram (>30 degrees)')
			plt.ylabel('Counts')
			plt.xlabel('QVAL')
		
		plt.savefig(self.output_dir + "/automatic/qvals.tif")
		plt.close()
		
		plt.subplot(111)
		plt.plot(tilts, qvals, 'o', alpha=0.6)
		plt.title('Tilt angle vs. QVAL')
		plt.ylabel('QVAL')
		plt.xlabel('Tilt angle')
		plt.savefig(self.output_dir + "/automatic/tilt_qval.tif")
		plt.close()
		
		plt.subplots_adjust(hspace=0.6)
		if len(defs_pertilt[0])>0:
			plt.subplot(411)
			hist(defs_pertilt[0], bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
			plt.title('Defocus Histogram (0-10 degrees)')
			plt.ylabel('Counts')
			if len(defs_pertilt[1])==0 and len(defs_pertilt[2])==0 and len(defs_pertilt[3])==0:
				plt.xlabel('Defocus')
		if len(defs_pertilt[1])>0:
			plt.subplot(412)
			hist(defs_pertilt[1], bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
			plt.title('Defocus Histogram (10-20 degrees)')
			plt.ylabel('Counts')
			if len(defs_pertilt[2])==0 and len(defs_pertilt[3])==0:
				plt.xlabel('Defocus')
		if len(defs_pertilt[2])>0:
			plt.subplot(413)
			hist(defs_pertilt[2], bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
			plt.title('Defocus Histogram (20-30 degrees)')
			plt.ylabel('Counts')
			if len(defs_pertilt[3])==0:
				plt.xlabel('Defocus')
		if len(defs_pertilt[3])>0:
			plt.subplot(414)
			hist(defs_pertilt[3], bins=(def_max-def_min)/def_dx, range=(def_min,def_max), facecolor='green', alpha=0.5)
			plt.title('Defocus Histogram (20-30 degrees)')
			plt.ylabel('Counts')
			plt.xlabel('Defocus')
		
		plt.savefig(self.output_dir + "/automatic/defocis.tif")
		plt.close()
		
		
		
		
	def updateImages(self):
		print "update called"
		
		i = self.index_selected
		#print self.image_names[i][:-4], self.image_dirs[i]
		dia_folder = self.image_dirs[i] + "/automation_output"
		#print dia_folder
		core_name = self.image_names[i].split(".")[0]
		self.watcher.copy_image_if_there(self.image_dirs[i] + "/SCRATCH/" + core_name + ".red8.mrc", dia_folder + "/image.mrc")
		self.watcher.copy_image_if_there(self.image_dirs[i] + "/CUT/" + core_name + ".marked.merge.ps.mrc", dia_folder + "/defoci.mrc")
		self.watcher.copy_image_if_there(self.image_dirs[i] + "/FFTIR/" + core_name + ".red.fft.mrc", dia_folder + "/fft.mrc")
		self.watcher.copy_image_if_there(self.image_dirs[i] + "/" + core_name + "-p1.mrc", dia_folder + "/map.mrc")
		self.watcher.convert_mrc_to_png(dia_folder)
		self.watcher.fix_fft(dia_folder)
		
		lattice = self.watcher.get_lattice_from_config(self.image_dirs[i] + "/2dx_image.cfg")
		self.watcher.drawLattice(dia_folder + "/fft.png", dia_folder + "/fft_lattice.gif", lattice)
		ctf = self.watcher.get_ctf_from_config(self.image_dirs[i] + "/2dx_image.cfg")
		self.watcher.drawCTF(dia_folder + "/fft.png", dia_folder + "/fft_ctf.gif", ctf)
		
		tltaxis = self.watcher.get_tltaxis_from_config(self.image_dirs[i] + "/2dx_image.cfg")
		self.watcher.drawTiltAxisOnDefo(dia_folder + "/defoci.png", dia_folder + "/defoci_axis.gif", tltaxis)
		
		self.indexChanged()
	
	def getInfoString(self, folder):
		config_name = folder + "/2dx_image.cfg"
		content = read_text_row(config_name)
		for c in content:
			if len(c)>1 and c[1] == "defocus":
				defocus = c[3].split(",")

		defocus[0] = float(defocus[0][1:])
		defocus[1] = float(defocus[1])
		defocus[2] = float(defocus[2][:-1])
		
		for c in content:
			if len(c)>1 and c[1] == "magnification":
				magnification = float(c[3][1:-1])
			if len(c)>1 and c[1] == "QVAL":
				qval = float(c[3][1:-1])
			if len(c)>1 and c[1] == "TLTAXIS":
				tltaxis = float(c[3][1:-1])
			if len(c)>1 and c[1] == "TLTANG":
				tltang = float(c[3][1:-1])
			if len(c)>1 and c[1] == "U2_IQ1":
				iq1 = int(c[3][1:-1])
			if len(c)>1 and c[1] == "U2_IQ2":
				iq2 = int(c[3][1:-1])
			if len(c)>1 and c[1] == "U2_IQ3":
				iq3 = int(c[3][1:-1])
			if len(c)>1 and c[1] == "U2_IQ4":
				iq4 = int(c[3][1:-1])
			if len(c)>1 and c[1] == "comment":
				comment = c[3:]
				comment_string = ""
				string_len = 0
				for c in comment:
					string_len += len(c)
					comment_string += c + " "
					if string_len > 30:
						string_len = 0
						comment_string += "\n"
	
		result = "Image Statistics:\n\n\n"
		
		result += "2dx image folder: " + folder.split("/")[-2] + "/" + folder.split("/")[-1] + "\n"
		
		file_time = datetime.datetime.fromtimestamp(os.path.getmtime(folder)) 
		result += "Time: " + str(file_time) + "\n\n"
		
		result += "Defocus: " + str(defocus[0]) + "\n"
		result += "Defocus (astigmatic): " + str(defocus[1]) + "\n"
		result += "Astigmatism Angle: " + str(defocus[2]) + "\n\n"
		
		result += "Magnification: " + str(magnification) + "\n\n"
		
		result += "TLTAXIS: " + str(tltaxis) + "\n"
		result += "TLTANG: " + str(tltang) + "\n\n"
		
		result += "QVAL: " + str(qval) + "\n"
		result += "IQ1: " + str(iq1) + "\n"
		result += "IQ2: " + str(iq2) + "\n"
		result += "IQ3: " + str(iq3) + "\n"
		result += "IQ4: " + str(iq4) + "\n\n"
		
		result += "Comment: " + comment_string + "\n\n"
		
		return result
		
	
	def resetResultOverview(self):
		self.listbox.delete(0, END)
		self.image_dirs = []
		self.image_names = []
		if os.path.exists(self.output_dir + "/automatic_import_log.txt"):
			f = open( self.output_dir + "/automatic_import_log.txt", 'r')
			for line in f:
				self.listbox.insert(END, line.split()[0])
				self.image_dirs.append(line.split()[1])
				self.image_names.append(line.split()[0])
		self.listbox.selection_set(END)
		#self.index_selected = len(self.image_dirs)-1
		self.indexChanged()

		
	def runAutomation(self):
		if not self.is_running:
			
			if tkMessageBox.askyesno("Automation Launch", "Relaunching may take a while as all new images are processed!\n\nDo you want to continue?"):
				self.status.configure(text="Automation starting up...", fg="orange")
				self.parent.update()
				time.sleep(2)
				if self.watcher.restart():
					self.resetResultOverview()
				
				#self.thread_running = thread.start_new_thread(self.watcher.run, ())
				
				#os.chdir("/run/user/1000/gvfs/smb-share:server=cina-home,share=scherers$/automation/")

				#self.proc = subprocess.Popen("cd /run/user/1000/gvfs/smb-share:server=cina-home,share=scherers$/automation/; python auto_2dx.py " + self.input_dir + " " + self.output_dir, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
				#self.subproc = subprocess.Popen(["sleep 10; ls"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
				#self.subproc = subprocess.Popen(self.watcher.run(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
				
				
				self.status.configure(text="Automation running", fg="green")
				self.is_running = True
				
				#print self.subproc.stdout.readline()
	
			
	def lauch2dxImage(self):
		print self.index_selected
		print self.image_dirs[self.index_selected]
		os.system("2dx_image " + self.image_dirs[self.index_selected])
		time.sleep (1)
		self.indexChanged()
		
	def editComment(self):
		folder = self.image_dirs[self.index_selected]
		config_name = folder + "/2dx_image.cfg"
		content = read_text_row(config_name)
				
		for c in content:
			if len(c)>1 and c[1] == "comment":
				comment = c[3:]
				comment_string = ""
				for c in comment:
					comment_string += c + " "
					
		
		data = tkSimpleDialog.askstring('Edit Comment', 'Please enter your new comment', initialvalue=comment_string[1:-2])
		
		if data==None:
			return
			
		new_content = []
		for c in content:
			if len(c)>1 and c[1] == "comment":
				new_string = 'set comment = "' + data + '"'
				new_c = new_string.split()
				new_content.append(new_c)
			else:
				new_content.append(c)
		
		f = open(config_name, 'w')
		for c in new_content:
			line = " ".join(map(str, c))
			f.write(line + "\n")
			
		self.indexChanged()
		
	def indexChanged(self):
		
		all_fine = True
		
		if os.path.exists(self.image_dirs[self.index_selected] + "/automation_output/fft_lattice.gif"):
			self.lattice_image = ImageTk.PhotoImage(Image.open(self.image_dirs[self.index_selected] + "/automation_output/fft_lattice.gif").resize((n,n),Image.ANTIALIAS))
			self.lattice_label.configure(image=self.lattice_image)
		else:
			self.lattice_label.configure(image=self.default_tkimage)
			all_fine = False
		
		if os.path.exists(self.image_dirs[self.index_selected] + "/automation_output/fft_ctf.gif"):
			self.ctf_image = ImageTk.PhotoImage(Image.open(self.image_dirs[self.index_selected] + "/automation_output/fft_ctf.gif").resize((n,n),Image.ANTIALIAS))
			self.ctf_label.configure(image=self.ctf_image)
		else:
			self.ctf_label.configure(image=self.default_tkimage)
			all_fine = False
		
		if os.path.exists(self.image_dirs[self.index_selected] + "/automation_output/map.gif"):
			self.map_image = ImageTk.PhotoImage(Image.open(self.image_dirs[self.index_selected] + "/automation_output/map.gif").resize((n_small,n_small),Image.ANTIALIAS))
			self.map_label.configure(image=self.map_image)
		else:
			self.map_label.configure(image=self.default_tkimage_small)
		
		if os.path.exists(self.image_dirs[self.index_selected] + "/automation_output/defoci_axis.gif"):
			self.def_image = ImageTk.PhotoImage(Image.open(self.image_dirs[self.index_selected] + "/automation_output/defoci_axis.gif").resize((n,n),Image.ANTIALIAS))
			self.def_label.configure(image=self.def_image)
		else:
			self.def_label.configure(image=self.default_tkimage)
		
		if os.path.exists(self.image_dirs[self.index_selected] + "/automation_output/image.gif"):
			self.image_image = ImageTk.PhotoImage(Image.open(self.image_dirs[self.index_selected] + "/automation_output/image.gif").resize((n,n),Image.ANTIALIAS))
			self.image_label.configure(image=self.image_image)
		else:
			self.image_label.configure(image=self.default_tkimage)
			all_fine = False
			
		if all_fine:
			info = self.getInfoString(self.image_dirs[self.index_selected])
		else:
			info = "Image Statistics:\n\n\n"
			info += "2dx image folder: " + self.image_dirs[self.index_selected].split("/")[-2] + "/" + self.image_dirs[self.index_selected].split("/")[-1] + "\n\n\n"
			info += "processing..."
		self.info_label.configure(text=info)

	
	def check_for_new_images(self):
		if os.path.exists(self.output_dir + "/automatic_import_log.txt"):
			f = open( self.output_dir + "/automatic_import_log.txt", 'r')
			num = len(f.readlines())
			if num>self.count:
				self.count = num
				self.resetResultOverview()
		#self.after(1000, self.check_for_new_images)
		
	def autom_do_check(self):
		#print "checked"
		if self.is_running:
			if self.watcher.test4new():
				self.resetResultOverview()
		self.after(2000, self.autom_do_check)
		
	def switchAutomationOff(self):
		if tkMessageBox.askyesno("Stop Automation", "Do you realy want to stop the automation?"):
			self.is_running = False
			self.status.configure(text="Automation not running", fg="red")
		
		
	def initUI(self):
		self.parent.title("2dx_automator")
	
		self.getFolders()
		
		self.input_dir = "/mnt/46652e86-6bef-4a8e-9cbb-013b4afb9aea/scherers/k2_mlok1/test_in"
		self.output_dir = "/mnt/46652e86-6bef-4a8e-9cbb-013b4afb9aea/scherers/k2_mlok1/test_out"
		
		in_folder = self.input_dir
		out_folder = self.output_dir
		logfile = out_folder + "/automatic_import_log.txt"
		wait = 1
		refresh = 10
		self.watcher = Add2dxImageWatcher(refresh, wait, in_folder, out_folder, logfile)

		
		self.initLayout()
		
		self.is_running = False
		
		Label(self.topleftframe, text="Input: " + self.input_dir).pack()
		Label(self.topleftframe, text="Output: " + self.output_dir).pack()
		
		run_button = Button(self.toprightframe ,text='Launch Automation', command=self.runAutomation, width=30)
		run_button.pack(padx=5, pady=2)
		
		stop_button = Button(self.toprightframe ,text='Stop Automation', command=self.switchAutomationOff, width=30)
		stop_button.pack(padx=5, pady=2)
		
		Label(self.centralleftframe, text="Images").pack()		
		
		self.scrollbar = Scrollbar(self.centralleftframe)
		self.scrollbar.pack(side=RIGHT, fill=Y)

		self.listbox = Listbox(self.centralleftframe, width=45)
		self.listbox.pack(padx=14)

		# attach listbox to scrollbar
		self.listbox.config(yscrollcommand=self.scrollbar.set, height=30)
		self.scrollbar.config(command=self.listbox.yview)
		
		@bind(self.listbox, '<<ListboxSelect>>')
		def onselect(evt):
			w = evt.widget
			index = int(w.curselection()[0])
			value = w.get(index)
			print 'You selected item %d: "%s"' % (index, value)
			self.index_selected = index
			self.indexChanged()
			
		self.lauch_2dx_image_button = Button(self.lowleftframe ,text='Lauch 2dx_image', width=40, command=self.lauch2dxImage)
		self.lauch_2dx_image_button.pack(padx=20, pady=20)
		
		self.comment_button = Button(self.lowleftframe ,text='Edit Comment', width=40, command=self.editComment)
		self.comment_button.pack(padx=20, pady=5)
		
		self.reload_button = Button(self.lowleftframe ,text='Regenerating Diagnostic Images', width=40, command=self.updateImages)
		self.reload_button.pack(padx=20, pady=5)
		
		self.box_test = tbox(self)
		#self.button1 = Button(self, text='Logged in',command=lambda: self.box("asdf"))
		#self.button1.pack()
		
		#self.stat_button = Button(self.lowleftframe ,text='Show Project Statistics', width=40, command=self.getProjectStat)
		
		
		def test_func():
			self.getProjectStat()
			#lambda: self.box_test("asdf")
			self.box_test("asdf")
		
		self.stat_button = Button(self.lowleftframe ,text='Show Project Statistics', width=40, command=test_func)
		self.stat_button.pack(padx=20, pady=60)
		
		self.default_image = Image.new("RGB", (n,n), "white")
		self.default_tkimage = ImageTk.PhotoImage(self.default_image)
		
		self.lattice_label = Label(self.centralrightframe1, image=self.default_tkimage)
		self.lattice_label.pack(side=RIGHT, pady=5, padx=5)
		
		self.ctf_label = Label(self.centralrightframe1, image=self.default_tkimage)
		self.ctf_label.pack(side=LEFT)
		
		self.def_label = Label(self.centralrightframe2, image=self.default_tkimage)
		self.def_label.pack(side=RIGHT, padx=5, pady=5)
		
		self.image_label = Label(self.centralrightframe2, image=self.default_tkimage)
		self.image_label.pack(side=LEFT)
		
		self.info_label = Label(self.centralrightframe3, text="Image Staticstics:\n", height=28)
		self.info_label.pack()
		
		self.default_image_small = Image.new("RGB", (n_small,n_small), "white")
		self.default_tkimage_small = ImageTk.PhotoImage(self.default_image_small)
		self.map_label = Label(self.centralrightframe3, image=self.default_tkimage_small)
		self.map_label.pack(side=RIGHT, padx=5, pady=5)
		
		self.status = Label(self.parent, text="Automation not running", bd=1, relief=SUNKEN, anchor=W, fg="red")
		self.status.pack(side=BOTTOM, fill=X)
		
		self.count = 0
		self.check_for_new_images()
		self.autom_do_check()
		
		
		
def main():
	root = Tk()
	root.geometry("850x650+300+300")
	app = Auto2dxGUI(root)
	root.mainloop()
	
if __name__ == '__main__':
	main() 
