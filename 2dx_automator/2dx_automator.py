#!/usr/bin/python


from Tkinter import *
import tkFileDialog
import tkMessageBox

import thread

from PIL import Image, ImageDraw, ImageTk

from EMAN2  import *
from sparx  import *

from Add2dxImageWatcher import *

import os
import datetime

n = 508
n_small = 390

import tkSimpleDialog
		

def bind(widget, event):
	def decorator(func):
		widget.bind(event, func)
		return func
	return decorator
	
	

def about(master):
	class ABOUT(object):
		def __init__(self):
			self.msgbox = Toplevel(master)
			self.msgbox.title("2dx_automator About")
		def b0_action(self):
			self.msgbox.destroy()
	return ABOUT


def helpbox(master):
	class HELP(object):
		def __init__(self):
			self.msgbox = Toplevel(master)
			self.msgbox.title("2dx_automator Help")
		def b0_action(self):
			self.msgbox.destroy()
	return HELP
	
	
def tbox(master):
	class TBOX(object):
		def __init__(self, info):
			
			self.msgbox = Toplevel(master)
			
			self.msgbox.title("2dx_automator Project Statistics Overview")
			
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
			self.msgbox.destroy()
	return TBOX


class Auto2dxGUI(Frame):
	def __init__(self, parent):
		Frame.__init__(self, parent, background="white")
		self.parent = parent
		self.index_selected = 0
		self.ref_mode = True
		self.initUI()	
		self.resetResultOverview()
		self.listbox.selection_clear(0, END)
		self.listbox.selection_set(END)
		self.listbox.activate(END)
		self.index_selected = self.count-1
		self.indexChanged()
		
		
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
		qvals = ""
		for f in self.image_dirs:
			config_name = f + "/2dx_image.cfg"
			skip_file = f + "/automation_scipt_flag"
			if not os.path.exists(skip_file):
				content = read_text_row(config_name)
				for c in content:
					if len(c)>1 and c[1] == "QVAL":
						qvals += str(abs(float(c[3][1:-1]))) + " "
		return qvals	


	def getAllTiltAngles(self):
		tilt_anlges = ""
		for f in self.image_dirs:
			config_name = f + "/2dx_image.cfg"
			skip_file = f + "/automation_scipt_flag"
			if not os.path.exists(skip_file):
				content = read_text_row(config_name)
				for c in content:
					if len(c)>1 and c[1] == "TLTANG":
						tilt_anlges += str(abs(float(c[3][1:-1]))) + " "
		return tilt_anlges
	
	
	def getAllDefoci(self):
		defoci = ""
		for f in self.image_dirs:
			config_name = f + "/2dx_image.cfg"
			skip_file = f + "/automation_scipt_flag"
			if not os.path.exists(skip_file):
				content = read_text_row(config_name)
				for c in content:
					if len(c)>1 and c[1] == "defocus":
						defocus = c[3].split(",")
						defoci += str(float(defocus[0][1:])) + " "
		return defoci
			
		
	def getProjectStat(self):
		tilts = self.getAllTiltAngles()
		defs = self.getAllDefoci()
		qvals = self.getAllQVals()
		
		out_file = open( self.output_dir + "/automatic/stat_data.txt", 'w')
		out_file.write(tilts + "\n")
		out_file.write(defs + "\n")
		out_file.write(qvals + "\n")
		out_file.close()
		
		command = "crystalplotter.py " + self.output_dir
		os.system(command)
		
		
	def updateImages(self, i):
		print "update called"
		
		self.watcher.lock_eman2.acquire()
		
		dia_folder = self.image_dirs[i] + "/automation_output"
		
		if os.path.exists(dia_folder):
			shutil.rmtree(dia_folder)
				
		os.makedirs(dia_folder)
		
		core_name = self.image_dirs[i].split("/")[-1]
		self.watcher.copy_image_if_there(self.image_dirs[i] + "/ManualMasking-CCmap.mrc", dia_folder + "/image.mrc")
		self.watcher.copy_image_if_there(self.image_dirs[i] + "/CUT/" + core_name + ".marked.merge.ps.mrc", dia_folder + "/defoci.mrc")
		self.watcher.copy_image_if_there(self.image_dirs[i] + "/FFTIR/" + core_name + ".red.fft.mrc", dia_folder + "/fft.mrc")
		
		if os.path.exists(self.image_dirs[i] + "/m" + core_name + "-p1.mrc"):
			self.watcher.copy_image_if_there(self.image_dirs[i] + "/m" + core_name + "-p1.mrc", dia_folder + "/map.mrc")
		else:
			self.watcher.copy_image_if_there(self.image_dirs[i] + "/" + core_name + "-p1.mrc", dia_folder + "/map.mrc")
		
		try:
			self.watcher.convert_mrc_to_png(dia_folder)
			self.watcher.fix_fft(dia_folder)
		except:
			pass
		
		lattice = self.watcher.get_lattice_from_config(self.image_dirs[i] + "/2dx_image.cfg")
		self.watcher.drawLattice(dia_folder + "/fft.png", dia_folder + "/fft_lattice.gif", lattice)
		ctf = self.watcher.get_ctf_from_config(self.image_dirs[i] + "/2dx_image.cfg")
		self.watcher.drawCTF(dia_folder + "/fft.png", dia_folder + "/fft_ctf.gif", ctf)
		
		tltaxis = self.watcher.get_tltaxis_from_config(self.image_dirs[i] + "/2dx_image.cfg")
		self.watcher.drawTiltAxisOnDefo(dia_folder + "/defoci.png", dia_folder + "/defoci_axis.gif", tltaxis)
		
		self.watcher.lock_eman2.release()
	
	
	def regenerateDiaImages(self):
		i = self.index_selected
		self.updateImages(i)
		self.indexChanged()
		
		
	def regenerateALLMaps(self):
		if tkMessageBox.askyesno("Map regeneration", "Do you want to regenerate all image maps?\n\nDo you want to continue?"):
			for i in range(len(self.image_dirs)):
				print self.image_dirs[i]
				dia_folder = self.image_dirs[i] + "/automation_output"
			
				if not os.path.exists(dia_folder):
					continue
			
				core_name = self.image_dirs[i].split("/")[-1]
				if os.path.exists(self.image_dirs[i] + "/m" + core_name + "-p1.mrc"):
					self.watcher.copy_image_if_there(self.image_dirs[i] + "/m" + core_name + "-p1.mrc", dia_folder + "/map.mrc")
				else:
					self.watcher.copy_image_if_there(self.image_dirs[i] + "/" + core_name + "-p1.mrc", dia_folder + "/map.mrc")
				old_path = os.getcwd()
				os.chdir(dia_folder)
				if os.path.exists(dia_folder + "/map.mrc"):
					os.chdir(dia_folder)
					os.system("e2proc2d.py " + "map.mrc" + " " + "map.png" )
					os.system("convert " + "map.png " + "map.gif" )
					os.chdir(old_path)
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
		
		u2_iqs = []
		u2_rb = []
		
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
			if len(c)>1 and c[1] == "RP_1":
				u2_iqs.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RP_2":
				u2_iqs.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RP_3":
				u2_iqs.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RP_4":
				u2_iqs.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RP_5":
				u2_iqs.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RP_6":
				u2_iqs.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RB_1":
				u2_rb.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RB_2":
				u2_rb.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RB_3":
				u2_rb.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RB_4":
				u2_rb.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RB_5":
				u2_rb.append(int(c[3][1:-1]))
			if len(c)>1 and c[1] == "RB_6":
				u2_rb.append(int(c[3][1:-1]))
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
		
		result += "Defocus (mean): " + str((defocus[0]+defocus[1])/2) + " A\n"
		result += "Astigmatism : " +  str((defocus[0]-defocus[1])/2) + " A\n"
		result += "Astigmatism Angle: " + str(defocus[2]) + " deg\n\n"
		
		result += "Magnification: " + str(magnification) + "\n\n"
		
		result += "TLTAXIS: " + str(tltaxis) + " deg\n"
		result += "TLTANG: " + str(tltang) + " deg\n\n"
		
		result += "QVAL: " + str(qval) + "\n"
		result += "IQ1: " + str(iq1) + "\n"
		result += "IQ2: " + str(iq2) + "\n"
		result += "IQ3: " + str(iq3) + "\n"
		result += "IQ4: " + str(iq4) + "\n\n\n"
		
		result += "PowerBins\t[A] " + str(u2_rb[0]) + "\t" + str(u2_rb[1]) + "\t" + str(u2_rb[2]) + "\t" + str(u2_rb[3]) + "\t" + str(u2_rb[4]) + "\t" + str(u2_rb[5])  + "\n"
		result += "(noise=7)\t #  " + str(u2_iqs[0]) + "\t" + str(u2_iqs[1]) + "\t" + str(u2_iqs[2]) + "\t" + str(u2_iqs[3]) + "\t" + str(u2_iqs[4]) + "\t" + str(u2_iqs[5])  + "\n\n\n"
				
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
		
		self.image_count_label.configure(text=str(len(self.image_names)) + " Images")
		
		self.listbox.selection_clear(0, END)
		self.listbox.selection_set(self.index_selected)
		self.listbox.activate(self.index_selected)
		self.indexChanged()

		
	def runAutomation(self):
		if not self.is_running:
			if tkMessageBox.askyesno("Automation Launch", "Relaunching may take a while as all new images are processed!\n\nDo you want to continue?"):
				self.status.configure(text="Automation starting up...", fg="orange")
				self.parent.update()
				time.sleep(2)
				if self.watcher.restart():
					self.resetResultOverview()
				self.status.configure(text="Automation running", fg="green")
				self.is_running = True
				
			
	def launch2dxImage(self):
		print self.index_selected
		print self.image_dirs[self.index_selected]
		os.system("2dx_image " + self.image_dirs[self.index_selected])
		time.sleep (1)
		self.indexChanged()
		
		
	def launch2dxMerge(self):
		thread.start_new_thread(os.system, ("2dx_merge " + self.output_dir,))
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
		
		if len(self.image_dirs) == 0:
			return
		
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
		
		skip_file = self.image_dirs[self.index_selected] + "/automation_scipt_flag"
		if os.path.exists(skip_file):
			self.usebox.deselect()
			self.info_label.configure(state=DISABLED)
		else:
			self.usebox.select()
			self.info_label.configure(state=NORMAL)

	
	def check_for_new_images(self):
		if os.path.exists(self.output_dir + "/automatic_import_log.txt"):
			f = open( self.output_dir + "/automatic_import_log.txt", 'r')
			num = len(f.readlines())
			if num>self.count:
				self.count = num
				self.resetResultOverview()
		
		
	def autom_do_check(self):
		if self.is_running:
			if self.watcher.test4new():
				self.resetResultOverview()
		self.after(2000, self.autom_do_check)
		
		
	def switchAutomationOff(self):
		if tkMessageBox.askyesno("Stop Automation", "Do you realy want to stop the automation?"):
			self.is_running = False
			self.status.configure(text="Automation not running", fg="red")

		
	def reprocess2dx(self, i):
		self.watcher.lock_2dx.acquire()
		old_path = os.getcwd()
		os.chdir(self.watcher.outfolder)
		
		image_count = i + 1
		protein_name = "auto_"
		
		image_2dx_name = self.watcher.outfolder + "/automatic/" + protein_name + str(image_count)
		os.makedirs(image_2dx_name)
		shutil.copyfile( self.watcher.infolder + "/" + self.image_names[i], image_2dx_name + "/" + protein_name + str(image_count) + ".mrc" )
		shutil.copyfile( self.watcher.outfolder + "/2dx_master.cfg", image_2dx_name + "/2dx_image.cfg" )
		
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_initialize"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " + image_2dx_name + " '" + '"2dx_initialize_files"' + "'" 
		os.system(command_2dx_image)
		
		self.watcher.set_image_number(image_2dx_name, image_count)
		self.watcher.set_orignal_filename(image_2dx_name, self.image_names[i])
		
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_initialize"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_initialize_files"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_fftrans"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_getDefTilt"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_getLattice"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_getspots1"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_unbend1"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_getspots"' + "'" 
		os.system(command_2dx_image)
		
		self.watcher.setDoMask( image_2dx_name )
		
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_unbend2"' + "'" 
		os.system(command_2dx_image)
		
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_fftrans"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_getspots1"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_unbend1"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_getspots"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_unbend2"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_applyCTF"' + "'" 
		os.system(command_2dx_image)
		command_2dx_image = "2dx_image " +  image_2dx_name + " '" + '"2dx_generateMAP"' + "'" 
		os.system(command_2dx_image)
		
		os.chdir(old_path)
		self.watcher.lock_2dx.release()
		
		
	def reprocessCaller(self,i):
		if os.path.exists(self.image_dirs[i]):
			shutil.rmtree(self.image_dirs[i])
		
		self.reprocess2dx(i)
		self.updateImages(i)
		

	def reprocessImage(self):
		i = self.index_selected
		dia_folder = self.image_dirs[i] + "/automation_output"
		
		if os.path.exists(dia_folder):
			shutil.rmtree(dia_folder)
		
		os.makedirs(dia_folder)
		self.indexChanged()
		
		thread.start_new_thread(self.reprocessCaller, (i,))
		
	
	def useButtonClicked(self):
		skip_file = self.image_dirs[self.index_selected] + "/automation_scipt_flag"
				
		if self.usevar.get() == 1:
			if os.path.exists(skip_file):
				os.remove(skip_file)
		else:
			if not os.path.exists(skip_file):
				skip = open(skip_file, "w")
				skip.write("This image is skipped by the 2dx_automator\n")
				skip.close()
		self.indexChanged()
		
	def select_image(self):
		self.usebox.toggle()
		self.useButtonClicked()

	def showIQ(self):
		i = self.index_selected
		command = "evince " + self.image_dirs[i] + "/PS/2dx_plotreska_canonical.ps"
		os.system(command)
		
	def showAbout(self):
		self.about_box()
	
	def showHelp(self):
		self.help_box()	
		
	def initUI(self):
		self.parent.title("2dx_automator GUI (alpha_2)")
	
		self.getFolders()
	
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
		
		self.image_count_label = Label(self.centralleftframe, text="Images")
		self.image_count_label.pack()
		
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
			
		self.lauch_2dx_image_button = Button(self.lowleftframe ,text='Lauch 2dx_image', width=40, command=self.launch2dxImage)
		self.lauch_2dx_image_button.pack(padx=20, pady=20)
		
		self.comment_button = Button(self.lowleftframe ,text='Edit Comment', width=40, command=self.editComment)
		self.comment_button.pack(padx=20, pady=5)
		
		self.reload_button = Button(self.lowleftframe ,text='Regenerate Diagnostic Images', width=40, command=self.regenerateDiaImages)
		self.reload_button.pack(padx=20, pady=5)
		
		self.reload_maps_button = Button(self.lowleftframe ,text='Regenerate ALL Maps', width=40, command=self.regenerateALLMaps)
		self.reload_maps_button.pack(padx=20, pady=5)
		
		self.reprocess_button = Button(self.lowleftframe ,text='Reprocess Image', width=40, command=self.reprocessImage)
		self.reprocess_button.pack(padx=20, pady=5)
		
		self.box_test = tbox(self)
		self.about_box = about(self)
		self.help_box = helpbox(self)
		
		def test_func():
			self.getProjectStat()
			self.box_test("asdf")
		
		self.stat_button = Button(self.lowleftframe ,text='Show Project Statistics', width=40, command=test_func)
		self.stat_button.pack(padx=20, pady=50)
		
		self.merge_button = Button(self.lowleftframe ,text='Launch 2dx_merge', width=40, command=self.launch2dxMerge)
		self.merge_button.pack(padx=20, pady=50)
		
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
		
		self.info_label = Label(self.centralrightframe3, text="Image Staticstics:\n", height=30)
		self.info_label.pack()

		self.iqplot_button = Button(self.centralrightframe3 ,text='Show IQ-Plot', width=20, command=self.showIQ)
		self.iqplot_button.pack(side=BOTTOM, pady=5)		

		self.usevar = IntVar()
		self.usebox = Checkbutton(self.centralrightframe3, variable=self.usevar, text="Use image", command=self.useButtonClicked)
		self.usebox.pack(side=BOTTOM, pady=10)
		self.usebox.select()
		
		self.default_image_small = Image.new("RGB", (n_small,n_small), "white")
		self.default_tkimage_small = ImageTk.PhotoImage(self.default_image_small)
		
		self.map_label = Label(self.centralrightframe3, image=self.default_tkimage_small)
		self.map_label.pack(side=BOTTOM, padx=40, pady=5)
				
		self.status = Label(self.parent, text="Automation not running", bd=1, relief=SUNKEN, anchor=W, fg="red")
		self.status.pack(side=BOTTOM, fill=X)
		
		self.count = 0
		self.check_for_new_images()
		self.autom_do_check()
		
		
		
def main():
	root = Tk()
	root.geometry("1900x1200+0+0")
	
	app = Auto2dxGUI(root)
	
	def greet(*ignore):
		app.select_image()
	
	root.bind('<s>', greet)
	
	menubar = Menu(root)
	filemenu = Menu(menubar, tearoff=0)
	filemenu.add_separator()
	filemenu.add_command(label="Exit", command=root.quit)
	menubar.add_cascade(label="File", menu=filemenu)
	
	editmenu = Menu(menubar, tearoff=0)
	editmenu.add_command(label="Show Config")
	menubar.add_cascade(label="Edit", menu=editmenu)
	
	helpmenu = Menu(menubar, tearoff=0)
	helpmenu.add_command(label="Help...", command=app.showHelp)
	helpmenu.add_command(label="About...", command=app.showAbout)
	menubar.add_cascade(label="Help", menu=helpmenu)

	root.config(menu=menubar)
	
	root.mainloop()
	
	root.mainloop()
	
	
if __name__ == '__main__':
	main() 
