from WatcherBaseClass import *
from EMAN2  import *
from sparx  import *

from pylab import plt, plot, subplot, figure, hist

import os

class MotionCorrectionWatcher(WatcherBase):
	
	def generateDriftPlot(self, filename):
		command = "driftplotter.py " + filename + " " + self.infolder
		print command
		os.system(command)
		
	def __init__(self, refresh_time, wait_time, infolder, outfolder, log_file_name, first_frame=0, last_frame=0):
		self.refresh_time = refresh_time
		self.wait_time = wait_time
		self.infolder = infolder
		self.outfolder = outfolder
		self.log_file_name = log_file_name
		self.first_frame = first_frame
		self.last_frame = last_frame
		self.lock_compute = threading.Lock()
		self.fod = 7
		self.waittime = 40
		self.binning = 1
		self.bfac = 150
		self.mode = 0
	
	def file_filter(self, filename):
		return ((filename.endswith(".mrc")) and not (filename.endswith("_ready.mrc")) and not (filename.endswith("_ready_SumCorr.mrc")))
	
	def getFileCoreName(self, filename):
		return filename[:-4]
	
	def image_added(self, filename, do_wait = True):
		print "motion_correction for", filename
		filecorename = self.getFileCoreName(filename)
		
		if do_wait:
			#waiting_time = self.waittime
			#print "Waiting for", waiting_time, "seconds to make sure that the copy is done"
			#time.sleep(waiting_time)
			old_size = os.path.getsize(filename)
			new_size = old_size + 1
			while old_size != new_size:
				print "file stil changing"
				time.sleep(2)
				old_size = new_size
				new_size = os.path.getsize(filename)
			
		print "*** In case you see this for a long time consider troubleshooting the automation ***"
		
		self.lock_compute.acquire()
		shutil.copyfile(self.infolder + "/" + filename, "/tmp/mc_tmp.mrc")
		eman2_command = "e2proc2d.py " + "/tmp/mc_tmp.mrc" + " " + "/tmp/mc_ready.mrc --threed2threed"
		os.system(eman2_command)
		
		old_path = os.getcwd()
		os.chdir("/tmp")
		
		# introduce align to first 
		
		if self.mode == 0:
			align_to = 1
		else:
			align_to = 0
		
		motion_command = "motioncorr " + "/tmp/mc_ready.mrc" + " -nst " + str(self.first_frame) + " -ned " + str(self.last_frame) + " -fod " + str(self.fod) + " -bin " + str(self.binning) + " -bft " + str(self.bfac) + " -atm " + str(align_to)
		os.system(motion_command)
		shutil.copyfile("/tmp/mc_ready_SumCorr.mrc", self.outfolder + "/" + filecorename + "_aligned.mrc")
	
		if self.mode == 1:
			motion_command = "motioncorr " + "/tmp/mc_ready.mrc" + " -nst " + str(self.first_frame) + " -ned " + str(0) + " -fod " + str(self.fod) + " -bin " + str(self.binning) + " -bft " + str(self.bfac) + " -atm " + str(align_to)
			os.system(motion_command)
			shutil.copyfile("/tmp/mc_ready_SumCorr.mrc", self.outfolder + "/" + filecorename + "_fullaligned.mrc")
	
		os.chdir(old_path)
		
		if not os.path.exists(self.infolder + "/dosef_quick"):
			os.makedirs(self.infolder + "/dosef_quick")
		
		if self.binning == 1:
			#shutil.copyfile("/tmp/mc_ready_SumCorr.mrc", self.outfolder + "/" + filecorename + "_aligned.mrc")
			shutil.copyfile("/tmp/mc_ready_Log.txt", self.infolder + "/" + filecorename + "_ready_Log.txt")
			shutil.copyfile("/tmp/dosef_quick/mc_ready_CorrFFT.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_CorrFFT.mrc")
			shutil.copyfile("/tmp/dosef_quick/mc_ready_CorrSum.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_CorrSum.mrc")
			shutil.copyfile("/tmp/dosef_quick/mc_ready_RawFFT.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_RawFFT.mrc")
			
			try:
				os.remove("/tmp/mc_ready_SumCorr.mrc")
			except:
				pass
				
			try:
				os.remove("/tmp/mc_ready_Log.txt")
			except:
				pass
				
			try:
				os.remove("/tmp/dosef_quick/mc_ready_CorrFFT.mrc")
			except:
				pass
				
			try:
				os.remove("/tmp/dosef_quick/mc_ready_CorrSum.mrc")
			except:
				pass
				
			try:
				os.remove("/tmp/dosef_quick/mc_ready_RawFFT.mrc")
			except:
				pass
				
				
		else:
			#shutil.copyfile("/tmp/mc_ready_2x_SumCorr.mrc", self.outfolder + "/" + filecorename + "_aligned.mrc")
			shutil.copyfile("/tmp/mc_ready_2x_Log.txt", self.infolder + "/" + filecorename + "_ready_Log.txt")
			shutil.copyfile("/tmp/dosef_quick/mc_ready_2x_CorrFFT.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_CorrFFT.mrc")
			shutil.copyfile("/tmp/dosef_quick/mc_ready_2x_CorrSum.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_CorrSum.mrc")
			shutil.copyfile("/tmp/dosef_quick/mc_ready_2x_RawFFT.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_RawFFT.mrc")
			
			try:
				os.remove("/tmp/mc_ready_2x_SumCorr.mrc")
			except:
				pass
				
			try:
				os.remove("/tmp/mc_ready_2x_Log.txt")
			except:
				pass
				
			try:
				os.remove("/tmp/dosef_quick/mc_ready_2x_CorrFFT.mrc")
			except:
				pass
				
			try:
				os.remove("/tmp/dosef_quick/mc_ready_2x_CorrSum.mrc")
			except:
				pass
				
			try:
				os.remove("/tmp/dosef_quick/mc_ready_2x_RawFFT.mrc")
			except:
				pass
		
		self.convert_mrc_to_png(filename)
		self.generateDriftPlot(filename)
				
		self.lock_compute.release()
				
		print "motion_correction done for", filename
		
	def setFirstFrame(self, rhs):
		self.first_frame = rhs
		
	def setLastFrame(self, rhs):
		self.last_frame = rhs
		
	def setFOD(self, rhs):
		self.fod = rhs

	def setWaittime(self, rhs):
		self.waittime = rhs
		
	def setBinning(self, rhs):
		self.binning = rhs
		
	def setBFactor(self, rhs):
		self.bfac = rhs
		
	def setMode(self, rhs):
		self.mode = rhs
		
		
	def convert_mrc_to_png(self, filename):
		old_path = os.getcwd()
		os.chdir(self.infolder + "/dosef_quick")
		
		corename = self.getFileCoreName(filename)
		
		os.system("e2proc2d.py " + corename + "_ready_CorrFFT.mrc" + " " + corename + "_ready_CorrFFT.png" )
		os.system("convert " + corename + "_ready_CorrFFT" + ".png " + corename + "_ready_CorrFFT" + ".gif" )
		
		os.system("e2proc2d.py " + corename + "_ready_RawFFT.mrc" + " " + corename + "_ready_RawFFT.png" )
		os.system("convert " + corename + "_ready_RawFFT" + ".png " + corename + "_ready_RawFFT" + ".gif" )
		
		os.system("e2proc2d.py " + corename + "_ready_CorrSum.mrc" + " " + corename + "_ready_CorrSum.png" )
		os.system("convert " + corename + "_ready_CorrSum" + ".png " + corename + "_ready_CorrSum" + ".gif" )
		
		os.chdir(old_path)
		
	
