from WatcherBaseClass import *
from EMAN2  import *
from sparx  import *

from pylab import plt, plot, subplot, figure, hist

class MotionCorrectionWatcher(WatcherBase):
	
	def generateDriftPlot(self, filename):
		command = "python driftplotter.py " + filename + " " + self.infolder
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
		self.lock_eman2 = threading.Lock()
		self.lock_convert = threading.Lock()
		self.lock_motion = threading.Lock()
	
	def file_filter(self, filename):
		return ((filename.endswith(".mrc")) and not (filename.endswith("_ready.mrc")) and not (filename.endswith("_ready_SumCorr.mrc")))
		
	def image_added(self, filename, do_wait = True):
		print "motion_correction for", filename
		filecorename = filename.split(".")[0]
		
		if do_wait:
			time.sleep(30)
		
		self.lock_eman2.acquire()
		eman2_command = "e2proc2d.py " + self.infolder + "/" + filename + " " + self.infolder + "/" + filecorename + "_ready.mrc --threed2threed"
		os.system(eman2_command)
		self.lock_eman2.release()
		
		self.lock_motion.acquire()
		old_path = os.getcwd()
		os.chdir(self.infolder)
		motion_command = "motioncorr " + self.infolder + "/" + filecorename + "_ready.mrc -nst " + str(self.first_frame) + " -ned " + str(self.last_frame)
		os.system(motion_command)
		shutil.move(filecorename + "_ready_SumCorr.mrc", self.outfolder + "/" + filecorename + "_aligned.mrc")
		os.chdir(old_path)
		
		
		#self.lock_convert.acquire()
		self.convert_mrc_to_png(filename)
		self.generateDriftPlot(filename)
		#self.lock_convert.release()
		
		self.lock_motion.release()
		
		print "motion_correction done for", filename
		
	def setFirstFrame(self, rhs):
		self.first_frame = rhs
		
	def setLastFrame(self, rhs):
		self.last_frame = rhs
		
	def convert_mrc_to_png(self, filename):
		old_path = os.getcwd()
		os.chdir(self.infolder + "/dosef_quick")
		
		corename = filename.split(".")[0]
		
		os.system("e2proc2d.py " + corename + "_ready_CorrFFT.mrc" + " " + corename + "_ready_CorrFFT.png" )
		os.system("convert " + corename + "_ready_CorrFFT" + ".png " + corename + "_ready_CorrFFT" + ".gif" )
		
		os.system("e2proc2d.py " + corename + "_ready_RawFFT.mrc" + " " + corename + "_ready_RawFFT.png" )
		os.system("convert " + corename + "_ready_RawFFT" + ".png " + corename + "_ready_RawFFT" + ".gif" )
		
		os.system("e2proc2d.py " + corename + "_ready_CorrSum.mrc" + " " + corename + "_ready_CorrSum.png" )
		os.system("convert " + corename + "_ready_CorrSum" + ".png " + corename + "_ready_CorrSum" + ".gif" )
		
		os.chdir(old_path)
		
	
