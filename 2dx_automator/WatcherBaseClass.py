import os, time, sys
import shutil

import thread

class WatcherBase:
	
	refresh_time = 10
	wait_time = 10
	infolder = "invalid"
	outfolder = "invalid"
	log_file_name = "invalid"
	
	def file_filter(self, filename):
		raise NotImplementedError("Please Implement this method")
		
	def image_added(self, filename):
		raise NotImplementedError("Please Implement this method")
		
	def write_log(self, filename):
		open(self.log_file_name,"a").write(filename + "\t" + time.strftime("%c") + '\n')
		
	def restart(self):
		self.before = dict ([(f, None) for f in os.listdir (self.infolder)])
		if not os.path.isfile(self.log_file_name):
			tmp = open(self.log_file_name, "w")
			tmp.close()
		done = []
		log_file = open(self.log_file_name, "r")
		for line in log_file:
			done.append(line.split("\t")[0])
		some_thing_changed = False
		for d in self.before.keys():
			if d in done:
				print d, "already processed"
			else:
				if self.file_filter(d):
					some_thing_changed = True
					print d, "not yet processed"
					self.write_log(d)
					thread.start_new_thread(self.image_added, (d,))
					time.sleep(5)
		self.before = dict ([(f, None) for f in os.listdir (self.infolder)])
		return some_thing_changed
					
	def test4new(self, run_in_background=True):
		self.after = dict ([(f, None) for f in os.listdir (self.infolder)])
		added = [f for f in self.after if not f in self.before]
		if added:
			print "Added: ", ", ".join (added)
			for f in added:
				if self.file_filter(f) == True:
					print "Waiting for", self.wait_time, "seconds to make sure that the copy is done"
					time.sleep(self.wait_time)
					self.write_log(f)
					thread.start_new_thread(self.image_added, (f,))	
		self.before = self.after
		if added:
			return True
		else:
			return False
		
	def run(self):
		while True:
			time.sleep (self.refresh_time)
			self.after = dict ([(f, None) for f in os.listdir (self.infolder)])
			added = [f for f in self.after if not f in self.before]
			if added:
				print "Added: ", ", ".join (added)
			
				for f in added:
					if self.file_filter(f) == True:
						print "Waiting for", self.wait_time, "seconds to make sure that the copy is done"
						time.sleep(self.wait_time)
						self.image_added(f)
						self.write_log(f)
			self.before = self.after
