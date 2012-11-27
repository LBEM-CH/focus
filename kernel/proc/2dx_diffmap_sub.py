from __future__ import division
import sys
import os 
from numpy import * 
from scipy import stats
from math import copysign 
no_of_elements = 10

class Reflection:

	param_labels =  ('h', 'k', 'z', 'amp', 'phase', 'number', 'iq', 'weight', 'background', 'ctf', 'fom')

	def __init__(self, h, k, z, amp, phase, number='1', iq='0', weight='0.0', background='0.0', ctf='0.0', fom='0.0'):
		self.params = {}
		self.h = int(h)
		self.params['h'] = self.h
		self.k = int(k)
		self.params['k'] = self.k
		self.z = float(z)
		self.params['z'] = self.z
		self.amp = float(amp)
		self.params['amp'] = self.amp
		self.phase = float(phase)
		self.params['phase'] = self.phase
		self.number = int(number)
		self.params['number'] = self.number
		self.iq = abs(int(iq))
		self.params['iq'] = self.iq
		self.weight = float(weight)
		self.params['weight'] = self.weight
		self.background = float(background)
		self.params['background'] = self.background
		self.ctf = float(ctf)
		self.params['ctf'] = self.ctf
		self.fom = float(fom)
		self.params['fom'] = self.fom


	
#	def __init__(self, param_list):
#		assert len(param_list) >= len(self.param_labels), 'Reflection parameters mssing!'
#		self.params = {}	
#		for i in range(len(self.param_labels)):
#			if i in (0,1,5,6):
#				value = int(param_list[i])
#			else:
#				value = float(param_list[i])
#			self.params[self.param_labels[i]]=value
		

	def __str__(self):
		params_string = ''
		for key in self.param_labels:
			params_string += str(key+' = '+str(self.params[key])+' ')
		return params_string

class MergedReflection:

	param_labels =  ('h', 'k', 'l', 'amp', 'phase','n','var', 'fom')

	def __init__(self, h, k, l, amp, phase, n = 0, var = 0.0,  fom = 99.0) :
		self.params = {}
		self.h = int(h)
		self.params['h'] = self.h
		self.k = int(k)
		self.params['k'] = self.k
		self.l = int(l)
		self.params['l'] = self.l
		self.amp = float(amp)
		self.params['amp'] = self.amp
		self.phase = float(phase)
		self.params['phase'] = self.phase
		self.fom = float(fom)
		self.params['fom'] = self.fom
		self.n = int(n)
		self.params['n'] = self.n
		self.var = float(var)
		self.params['var'] = self.var


	
#	def __init__(self, param_list):
#		assert len(param_list) >= len(self.param_labels), 'Reflection parameters mssing!'
#		self.params = {}	
#		for i in range(len(self.param_labels)):
#			if i in (0,1,5,6):
#				value = int(param_list[i])
#			else:
#				value = float(param_list[i])
#			self.params[self.param_labels[i]]=value
		

	def __str__(self):
		params_string = ''
		for key in self.param_labels:
			params_string += str(key+' = '+str(self.params[key])+' ')
		return params_string




def parse_aph_file(aph_filepath):
	with open(aph_filepath,'r') as aph_file:
		reflection_list = [] 
		line_no = 0 
		# skip first line
		next(aph_file)
		for line in aph_file:
			el = line.split()
			if len(el) >= no_of_elements:
				#rint('['+str(line_no)+']\t'+line)
				r =  Reflection(el[0],el[1],el[2],el[3],el[4],el[5],el[6],el[7],el[8],el[9]);
				#r =  Reflection(el)
				reflection_list.append(r)
				#print(str(r))
				line_no += 1

		return reflection_list

def get_reference_amplitude(reflection_list):
	reference_no = reflection_list[0].number
	ref_sum_amp = 0
	ref_reflections = 0
	print('reference number: '+str(reference_no))
	for ref in reflection_list:
		# check for reference amplutide
		# should only be done once per miler index
		if ref.number == reference_no:
			ref_sum_amp+=ref.amp
			ref_reflections+=1
	return ref_sum_amp/ref_reflections


def scale_amplitudes(r_list, scale):
	scaled_list = []
	for ref in r_list:
		scaled_list.append(Reflection(ref.h,ref.k,ref.z, ref.amp*scale, ref.phase, ref.number, ref.iq, ref.weight, ref.background, ref.ctf, ref.fom))
	return scaled_list



def merge_phase(reflections):
	combphase = 360.0
	iq_weights  = [49.00,27.56,8.51,4.17,2.48,1.65,1.17,0.25]
	if reflections:
		phase = radians(array([ref.phase for ref in reflections]))
		weights = array([iq_weights[ref.iq-1] for ref in reflections])
		sumsin = sum(weights*sin(phase)) 
		sumcos = sum(weights*cos(phase)) 
		combphase = degrees(arctan2(sumsin,sumcos))
	return combphase
 


def merge_reflection(reflections):
	#print('number of reflections: '+str(len(reflections)))
	n = len(reflections)
	amp = array([ref.amp for ref in reflections])
	phase = array([ref.phase for ref in reflections])
	ctf = array([ref.ctf for ref in reflections])
	back = array([ref.background for ref in reflections])
	sumamp = sum(amp*abs(ctf)/back**2)
	weight = ctf**2/back**2
	sumampw = sum(weight)
	weight = weight / sumampw
	combamp = sumamp/sumampw
	mu = combamp
	xi = amp/ctf
	#scale = 1/(1-sum(weight**2))
	if n>1 :
		var = sum(weight * ((xi - mu)**2))
	else:
		var = 0.0
	combphase = merge_phase(reflections)
	ref = reflections[0];
	merged = MergedReflection(ref.h, ref.k, int(abs(ref.z)), combamp, combphase, n, var)
	return merged
		#for ref in reflections:
			#print(str(ref.h)+','+str(ref.k)+'|')
		#print(combamp)

def merge_reflections(reflection_list, z_max = 0.0025, iq_max = 6, max_amp_correction = 0.2):
	print('zmax = '+str(z_max))
	h_prev = 0
	k_prev = 0
	same_reflection = []
	reflections_by_index = {} 
	no_skipped = 0
	for ref in reflection_list:
		if -z_max <= ref.z <= z_max and ref.iq <= iq_max:
				# maximum CTF correction
			if abs(ref.ctf) < max_amp_correction:
				#print('correcting ctf '+str(ref.ctf)+' to '+str(max_amp_correction))
				ref.ctf = copysign(max_amp_correction, ref.ctf)
			# same miller index
			if  (ref.h == h_prev and ref.k == k_prev):
				same_reflection.append(ref)
			else:
				if same_reflection:
					reflections_by_index[h_prev,k_prev] = same_reflection
				h_prev = ref.h
				k_prev = ref.k
				same_reflection = []
				same_reflection.append(ref)
		else:
			no_skipped +=1
	#print(reflections_by_index.keys())
	return reflections_by_index


def determine_significance(reflections1, reflections2, confidence=1.0):
	p_threshold = confidence/100.0
	ref_no_compared = 0
	ref_no_significant = 0
	ref_unmatched = 0
	merged_reflections1 = []
	merged_reflections2 = []
	for key in sorted(reflections1):
		if key in sorted(reflections2):
			ref_no_compared += 1
			a =  array([ref.amp/ref.ctf for ref in reflections1[key]])
			b =  array([ref.amp/ref.ctf for ref in reflections2[key]])
			#[k2_a, p_normal_a] = stats.mstats.normaltest(a,None)
			#[k2_b, p_normal_b] = stats.mstats.normaltest(b,None)
			[t, p] = stats.ttest_ind(a, b, False)
			if p < p_threshold:
				print('p = '+str(p))
				print('t = '+str(t))
				merged_reflections1.append(merge_reflection(reflections1[key]))
				merged_reflections2.append(merge_reflection(reflections2[key]))
				ref_no_significant += 1
		else:
			print('the reflection with index '+str(key)+'is not present in the both coformations')
			ref_unmatched +=1
	print('compared reflections: '+str(ref_no_compared)+'\n')
	print('significant reflections: '+str(ref_no_significant)+'\n')
	if ref_no_compared > 0:
		print('percentage of significance: '+str(float(ref_no_significant)/ref_no_compared)+'\n')
	print('unmatched reflections: '+str(ref_unmatched)+'\n')
	return [merged_reflections1, merged_reflections2]


	

def avramphs(reflection_list, z_max = 0.0025, iq_max = 6, max_amp_correction = 0.2):
	print('zmax = '+str(z_max))
	h_prev = 0
	k_prev = 0
	same_reflection = []
	merged_reflections = []
	no_skipped = 0
	for ref in reflection_list:
		if -z_max <= ref.z <= z_max and ref.iq <= iq_max:
				# maximum CTF correction
			if abs(ref.ctf) < max_amp_correction:
				#print('correcting ctf '+str(ref.ctf)+' to '+str(max_amp_correction))
				ref.ctf = copysign(max_amp_correction, ref.ctf)
			# same miller index
			if  (ref.h == h_prev and ref.k == k_prev):
				same_reflection.append(ref)
			else:
				if same_reflection:
					merged_reflections.append(merge_reflection(same_reflection))
				h_prev = ref.h
				k_prev = ref.k
				same_reflection = []
				same_reflection.append(ref)
		else:
			no_skipped +=1
	return merged_reflections

def write_hkl_file(merged_reflections, filename):
	with open(filename,'w') as hkl_file:
		hkl_file.write('      1001\n')
		for ref in merged_reflections:
			line= '  {0:4d}  {1:4d}  {2:4d}     {3:8.1f}    {3:8.1f}    {5:8.3f}'.format(ref.h, ref.k, ref.l,ref.amp, ref.phase, ref.fom)
			print("%s" % line)
			hkl_file.write("%s\n" % line)

if __name__ == '__main__':
	aph_file1=sys.argv[1]
	aph_file2=sys.argv[2]
	file_path1=os.path.dirname(sys.argv[1])
	file_path2=os.path.dirname(sys.argv[2])
	no_args = len(sys.argv)
	reflection_list1 = parse_aph_file(aph_file1)
	reflection_list2 = parse_aph_file(aph_file2)
	amp1 = get_reference_amplitude(reflection_list1)
	amp2 = get_reference_amplitude(reflection_list2)
	amp_scale = amp1/amp2
	print('amplitude scale: '+str(amp_scale))
	print(reflection_list2[0])
	reflections = scale_amplitudes(reflection_list2, amp_scale)
	print(reflections[0])
	print(str(amp1)+" is the amplitude mean.")
	if no_args == 3:
		merged_refs1 = merge_reflections(reflection_list1)
		merged_refs2 = merge_reflections(reflection_list2)
	elif no_args == 4:
		merged_refs1 = merge_reflections(reflection_list1, float(sys.argv[3]))
		merged_refs2 = merge_reflections(reflection_list2, float(sys.argv[3]))
	elif no_args == 5:
		merged_refs1 = merge_reflections(reflection_list1, float(sys.argv[3]),int(sys.argv[4]))
		merged_refs2 = merge_reflections(reflection_list2, float(sys.argv[3]),int(sys.argv[4]))
	elif no_args == 6:
		merged_refs1 = merge_reflections(reflection_list1, float(sys.argv[3]),int(sys.argv[4]),1.0/float(sys.argv[5]))
		merged_refs2 = merge_reflections(reflection_list2, float(sys.argv[3]),int(sys.argv[4]),1.0/float(sys.argv[5]))
	[sig_refs1, sig_refs2] = determine_significance(merged_refs1, merged_refs2)
	hkl_file1 = os.path.join(file_path1,'avrg2D_sig.hkl')
	hkl_file2 = os.path.join(file_path2,'avrg2D_sig.hkl')
	write_hkl_file(sig_refs1, hkl_file1)	
	#print('reflections: '+str(len(reflection_list)))
	#print('**************************************************')
	#print('Merged Reflections Conformation 1 ****************')
	#print('**************************************************')
	#for item in merged_refs1:
	#	print(str(item)+'\n')
	#print('**************************************************')
	#print('Merged Reflections Conformation 2 ****************')
	#print('**************************************************')
	#for item in merged_refs2:
	#	print(str(item)+'\n')
	#print([str(item) for item in merged_refs])

