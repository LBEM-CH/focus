from WatcherBaseClass import *
from EMAN2  import *
from sparx  import *

from pylab import plt, plot, subplot, figure, hist

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
		self.fod = 2
		self.waittime = 30
	
	def file_filter(self, filename):
		return ((filename.endswith(".mrc")) and not (filename.endswith("_ready.mrc")) and not (filename.endswith("_ready_SumCorr.mrc")))
	
	def getFileCoreName(self, filename):
		return filename[:-4]
	
	def image_added(self, filename, do_wait = True):
		print "motion_correction for", filename
		filecorename = self.getFileCoreName(filename)
		
		if do_wait:
			waiting_time = self.waittime
			print "Waiting for", waiting_time, "seconds to make sure that the copy is done"
			time.sleep(waiting_time)
			
		print "*** In case you see this for a long time consider troubleshooting the automation ***"
		
		self.lock_compute.acquire()
		shutil.copyfile(self.infolder + "/" + filename, "/tmp/mc_tmp.mrc")
		eman2_command = "e2proc2d.py " + "/tmp/mc_tmp.mrc" + " " + "/tmp/mc_ready.mrc --threed2threed"
		os.system(eman2_command)
		
		old_path = os.getcwd()
		os.chdir("/tmp")
		motion_command = "motioncorr " + "/tmp/mc_ready.mrc" + " -nst " + str(self.first_frame) + " -ned " + str(self.last_frame) + " -fod " + str(self.fod)
		os.system(motion_command)
		os.chdir(old_path)
		
		if not os.path.exists(self.infolder + "/dosef_quick"):
			os.makedirs(self.infolder + "/dosef_quick")
		
		shutil.copyfile("/tmp/mc_ready_SumCorr.mrc", self.outfolder + "/" + filecorename + "_aligned.mrc")
		
		shutil.copyfile("/tmp/mc_ready_Log.txt", self.infolder + "/" + filecorename + "_ready_Log.txt")
		
		shutil.copyfile("/tmp/dosef_quick/mc_ready_CorrFFT.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_CorrFFT.mrc")
		shutil.copyfile("/tmp/dosef_quick/mc_ready_CorrSum.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_CorrSum.mrc")
		shutil.copyfile("/tmp/dosef_quick/mc_ready_RawFFT.mrc", self.infolder + "/dosef_quick/" + filecorename + "_ready_RawFFT.mrc")
		
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
		
	
