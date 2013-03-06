"""Framework for calculating difference maps.

This program compares the merged reflections (merge.aph) of two different
conformations. The differnce is a simple subtraction of the second map
from the first map. I addition we calculate the significance of the
differences with a t-Welch's test, where only the significant differences
are ploted in the resulting map.
"""


__author__ = "Marcel Arheit (marcel.arheit@unibas.ch)"
__version__ = "$Revision: 1.3 $"
__date__ = "$Date: 27.11.2012 $"
__copyright__ = "Copyright (c) 2013 Marcel Arheit"
__license__ = "GNU GENERAL PUBLIC LICENSE (GPL) Version 2"

import sys
import os 
from decimal import *
from operator import attrgetter
from numpy import * 
from scipy import stats, special
from UserDict import UserDict

#global static variables
no_of_elements = 10
default_amplitude_mean = 200.0

class Reflection(UserDict):
    """the parent class for all reflections"""
    def __init__(self, h, k, amp, phase):
        UserDict.__init__(self)
        self.h = int(h)
        self['h'] = self.h
        self.k = int(k)
        self['k'] = self.k
        self.amp = float(amp)
        self['amp'] = self.amp
        self.phase  = float(phase)
        self['phase'] = self.phase

        

class OrigtiltReflection(Reflection):

	param_labels =  ('h', 'k', 'z', 'amp', 'phase', 'number', 'iq', 'weight', 'background', 'ctf', 'fom')

	def __init__(self, h, k, z, amp, phase, 
                number='1', iq='0', weight='0.0', 
                background='0.0', ctf='0.0', fom='0.0'):
            Reflection.__init__(self, h, k, amp, phase)
            self.z = float(z)
            self['z'] = self.z
	    self.number = int(number)
	    self['number'] = self.number
	    self.iq = abs(int(iq))
	    self['iq'] = self.iq
	    self.weight = float(weight)
	    self['weight'] = self.weight
	    self.background = float(background)
	    self['background'] = self.background
	    self.ctf = float(ctf)
	    self['ctf'] = self.ctf
	    self.fom = float(fom)
	    self['fom'] = self.fom
	
"""	def __repr__(self):
		return repr((self.h, self.k, self.z, self.amp, self.phase, self.number, self.iq, self.weight, self.background, self.ctf, self.fom))
	
	def __str__(self):
		params_string = ''
		for key in self.param_labels:
			params_string += str(key+' = '+str(self.params[key])+' ')
		return params_string
        """

class AvramphsReflection(Reflection):

	param_labels =  ('h', 'k', 'l', 'amp', 'phase','n','var', 'fom')

	def __init__(self, h, k, l, amp, phase, n = 0, var = 0.0,  fom = 99.0) :
            Reflection.__init__(self, h, k, amp, phase)
	    self.l = int(l)
	    self['l'] = self.l
	    self.fom = float(fom)
	    self['fom'] = self.fom
	    self.n = int(n)
	    self['n'] = self.n
	    self.var = float(var)
	    self['var'] = self.var



def parse_aph_file(aph_filepath):
	with open(aph_filepath,'r') as aph_file:
		reflection_list = [] 
		line_no = 0 
		# skip first line
		next(aph_file)
		for line in aph_file:
			el = line.split()
			if len(el) >= no_of_elements:
				r =  OrigtiltReflection(el[0],el[1],el[2],el[3],el[4],el[5],el[6],el[7],el[8],el[9]);
				reflection_list.append(r)
				line_no += 1
		return reflection_list

def get_reference_amplitude(reflection_list):
    """ since origitilt always takes the first image as a reference to scale
    the amplitudes of the following images, one can just look at the mean of
    the amplitudes of the first image in the list"""
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

def get_mean_amplitude(reflection_list):
    """returns the mean of the reflections amplitudes"""
    amps = array([ref.amp for ref in reflection_list])
    mean_amp = mean(amps)
    return mean_amp

def scale_amplitudes(r_list, scale):
    "scales the amplitudes of all reflections in the list by the scale factor"    
    for i in range(len(r_list)):
        r_list[i].amp = r_list[i].amp*scale
        r_list[i]['amp'] = r_list[i].amp
    return r_list


def scale_amplitudes2common_mean(reflection_list):
    "scales the amplitudes of the reflections to have a mean of default_amplitude_mean"
    mean_amp = get_mean_amplitude(reflection_list)
    amp_scale = default_amplitude_mean / mean_amp
    reflection_list = scale_amplitudes(reflection_list, amp_scale)
    mean_amp = get_mean_amplitude(reflection_list)
    print(" mean amplitude is:"+str(mean_amp))
    return reflection_list

def scale_all_amplitudes(reflections1, reflections2):
    """scales the amplitudes of reflections for both confirmations to a common
    mean"""
    reflections1 = scale_amplitudes2common_mean(reflections1)
    reflections2 = scale_amplitudes2common_mean(reflections2)
    return [reflections1, reflections2]

def group_reflection_by_index(reflection_list):
    """
    groups the reflections by miller index in a dictionary where the key is
    the miller index and the element is a list of reflections 
    """
    h_prev = 0
    k_prev = 0
    l_prev = 0
    same_reflection = []
    reflections_by_index = {}
    for ref in reflection_list:
		# same miller index
		if  (ref.h == h_prev and ref.k == k_prev and ref.l == l_prev):
			same_reflection.append(ref)
		else:
			if same_reflection:
				reflections_by_index[h_prev,k_prev,l_prev] = same_reflection
			h_prev = ref.h
			k_prev = ref.k
			l_prev = ref.l
			same_reflection = []
			same_reflection.append(ref)
    #print(sorted(reflections_by_index.keys()))
    return reflections_by_index


def filter_reflections(reflection_list, z_max = 0.0025, iq_max = 6):
    "filters reflections out that have a too high z (zmax) or iq (iq_max) value"
    filtered_list = [ ref for ref in reflection_list if (-z_max <= ref.z <= z_max and ref.iq <= iq_max)]
    return filtered_list


def merge_phase_calc_fom(reflections):
	combphase = 0.0
	iq_weights  = [49.00,27.56,8.51,4.17,2.48,1.65,1.17,0.25]
	if reflections:
		phase = radians(array([ref.phase for ref in reflections]))
		weights = array([iq_weights[ref.iq-1] for ref in reflections])
		sumsin = sum(weights*sin(phase)) 
		sumcos = sum(weights*cos(phase)) 
		combphase = degrees(arctan2(sumsin,sumcos))
                #TODO: this should only be done when symmetrized
                #combphase = combphase % 360.0
		xarg = sqrt(sumsin**2+sumcos**2)
		# bessel function 1 and 0
		bessel_0 = special.iv(0,xarg)
		bessel_1 = special.iv(1,xarg)
		#print('bessel functions for xarg = '+str(xarg)+ ', iv_0= '+str(bessel_0)+' iv_1= '+str(bessel_1))
		if isfinite(bessel_0) and isfinite(bessel_1):
			fom = 100.0*(bessel_1/bessel_0)
		else:
                        #print(":: xarg is "+str(xarg))
			sigma = sqrt(1.0/xarg)
			fom = 100.0*cos(sigma)
	return (combphase, fom)
 

def average_reflection_by_idx(reflections, index):
	"average all reflections belonging to the same index like in AVRAMPHS"
        if len(index)<3:
            h = index[0]
            k = index[1]
            l = 0 
        else:
            (h,k,l) = index
        n = len(reflections)
	amp = array([ref.amp for ref in reflections])
	ctf = array([ref.ctf for ref in reflections])
	back = array([ref.background for ref in reflections])
	sumamp = sum(amp*abs(ctf)/back**2)
	weight = ctf**2/back**2
	sumampw = sum(weight)
	weight = weight / sumampw
	combamp = sumamp/sumampw
	mu = combamp
	xi = amp/ctf
	if n>1 :
		var = sum(weight * ((xi - mu)**2))
	else:
		var = 0.0
	(combphase,fom) = merge_phase_calc_fom(reflections)
	avrg_reflection = AvramphsReflection(h, k, l, combamp, combphase, n, var, fom)
	return avrg_reflection

def merge_reflection(reflections):
	n = len(reflections)
	amp = array([ref.amp for ref in reflections])
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
	(combphase,fom) = merge_phase_calc_fom(reflections)
	ref = reflections[0];
	merged = AvramphsReflection(ref.h, ref.k, int(abs(ref.z)), combamp, combphase, n, var, fom)
	return merged

def merge_reflections(reflection_list, z_max = 0.0025, iq_max = 6, max_amp_correction = 0.2):
	"""merges relfections with the same miller index to a list, which then
        becomes a dictionary entry with the index as key"""
        h_prev = 0
	k_prev = 0
	same_reflection = []
	reflections_by_index = {}
        filtered_list = filter_reflections(reflection_list, z_max, iq_max)
        #filtered_list = reflection_list
	for ref in filtered_list:
	    # maximum CTF correction
	    if abs(ref.ctf) < max_amp_correction:
		    #print('correcting ctf '+str(ref.ctf)+' to '+str(max_amp_correction))
		    ref.ctf = copysign(max_amp_correction,ref.ctf)
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
	# don't forget the last reflection 
        if same_reflection and k_prev > 0:
		reflections_by_index[h_prev,k_prev] = same_reflection
		same_reflection.append(ref)
	return reflections_by_index

def average_reflections_by_idx(reflections):
        average_reflections = []
        for key in sorted(reflections):
            average_reflections.append(average_reflection_by_idx(reflections[key],key))
        return average_reflections

def average_significant_reflections(reflections, sig_indices):
    "averages only the signifcant reflections and returns them as a list"
    reflections = {k: reflections[k] for k in sig_indices if k in reflections}
    averaged_sig_refs = average_reflections_by_idx(reflections)
    return averaged_sig_refs


def determine_significance(reflections1, reflections2, confidence=99.0):
	#p_threshold = (100.0-confidence)/100.0
	p_threshold = 1.0 
        ref_no_compared = 0
        ref_no_significant = 0
	ref_unmatched = 0
	merged_reflections1 = []
	merged_reflections2 = []
	for key in sorted(reflections1):
		if key in sorted(reflections2):
			ref_no_compared += 1
			a =  array([ref.amp/abs(ref.ctf) for ref in reflections1[key]])
                        b =  array([ref.amp/abs(ref.ctf) for ref in reflections2[key]])
			#[k2_a, p_normal_a] = stats.mstats.normaltest(a,None)
			#[k2_b, p_normal_b] = stats.mstats.normaltest(b,None)
			[t, p] = stats.ttest_ind(a, b)
			if p <= p_threshold:
				#print('p = '+str(p))
				#print('t = '+str(t))
				merged_reflections1.append(average_reflection_by_idx(reflections1[key],key))
				merged_reflections2.append(average_reflection_by_idx(reflections2[key],key))
				ref_no_significant += 1
		else:
			ref_unmatched +=1
	# addinng unmatched reflections	
	refs_only_in1 = set(reflections1.keys()) - set(reflections2.keys())
        if len(refs_only_in1) > 0:
            print("reflections only in map1: "+str(len(refs_only_in1)))
	    print(str(sorted(refs_only_in1)))
	    for key in sorted(refs_only_in1):
		merged_reflections1.append((average_reflection_by_idx(reflections1[key],key)))
	refs_only_in2 = set(reflections2.keys()) - set(reflections1.keys())
        if len(refs_only_in2) > 0:
            print("reflections only in map2: "+str(len(refs_only_in2)))
	    print(str(sorted(refs_only_in2)))
	    for key in sorted(refs_only_in2):
		merged_reflections2.append((average_reflection_by_idx(reflections2[key],key)))
		
	merged_reflections1 = sorted(merged_reflections1,  key=attrgetter('h','k','l'))
	merged_reflections2 = sorted(merged_reflections2,  key=attrgetter('h','k','l'))
	
	return [merged_reflections1, merged_reflections2]

def get_significant_reflections(reflections, sig_indices):
    "returns the significant reflection list"
    sig_reflection = {}
    #TODO: check wether the key of the reflection is a tuple of length 2 or 3
    idx_length = len(reflections.keys()[0])
    if  idx_length == 3:
        sig_reflections = {k: reflections[k] for k in sig_indices if k in reflections}
    elif idx_length == 2:
        sig_reflections = {k: reflections[(k[0],k[1])] for k in sig_indices if (k[0],k[1]) in reflections}
    averaged_reflections = average_reflections_by_idx(sig_reflections)

    return averaged_reflections
 


def determine_significant_reflections(reflections1, reflections2, confidence=99.0):
    """determines the signifcance of each reflection by a t-welch test for the two
        reflection sets
        
        returns the significant reflection lists
    """
    [sig_idx1, sig_idx2] = determine_significance_indices(reflections1, reflections2, confidence)
    reflections1 = {k: reflections1[k] for k in sig_idx1 if k in reflections1}
    reflections2 = {k: reflections2[k] for k in sig_idx2 if k in reflections2}
    averaged_reflections1 = average_reflections_by_idx(reflections1)
    averaged_reflections2 = average_reflections_by_idx(reflections2)

    return [averaged_reflections1, averaged_reflections2]
    
def determine_significance_indices(reflections1, reflections2, confidence=99.0):
        """determines the signifcance of each reflection by a t-welch test for the two
        reflection sets 
        returns the indices for the significant reflections"""
	p_threshold = (100.0-confidence)/100.0
	#p_threshold = 1.0 
        ref_no_compared = 0
        ref_no_significant = 0
	ref_unmatched = 0
	sig_indices1 = []
	sig_indices2 = []
	for key in sorted(reflections1):
		if key in sorted(reflections2):
			ref_no_compared += 1
			a =  array([ref.amp/abs(ref.ctf) for ref in reflections1[key]])
                        b =  array([ref.amp/abs(ref.ctf) for ref in reflections2[key]])
			#[k2_a, p_normal_a] = stats.mstats.normaltest(a,None)
			#[k2_b, p_normal_b] = stats.mstats.normaltest(b,None)
			[t, p] = stats.ttest_ind(a, b)
			if p <= p_threshold:
				#print('p = '+str(p))
				#print('t = '+str(t))
				sig_indices1.append(key)
				sig_indices2.append(key)
				ref_no_significant += 1
		else:
			ref_unmatched +=1
	# addinng unmatched keys	
	refs_only_in1 = set(reflections1.keys()) - set(reflections2.keys())
        sig_indices1.extend(refs_only_in1)
	refs_only_in2 = set(reflections2.keys()) - set(reflections1.keys())
        sig_indices2.extend(refs_only_in2)
	sig_indices1= sorted(sig_indices1)
	sig_indices2 = sorted(sig_indices2)
        print(": significant reflections 1: "+str(len(sig_indices1)))
        print(": significant reflections 2: "+str(len(sig_indices2)))
	
	return [sig_indices1, sig_indices2]


	

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
				ref.ctf = copysign(max_amp_correction,ref.ctf)
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
			line= '  {0:4d}  {1:4d}  {2:4d}     {3:6.6}    {4:6.6}    {5:2.4F}'.format(ref.h, ref.k, ref.l ,ref.amp, ref.phase, ref.fom)
			print("%s" % line)
			hkl_file.write("%s\n" % line)

def write_aph_file(reflection_list, filename):
	with open(filename,'w') as aph_file:
		aph_file.write('      1001\n')
		for ref in reflection_list:
	                #param_labels =  ('h', 'k', 'z', 'amp', 'phase', 'number', 'iq', 'weight', 'background', 'ctf', 'fom')
                        line= '  {0:4d}  {1:4d}  {2:6.4}     {3:6.6}    {4:6.6}           {5:6d}  {6:2d}  {7:6.6}    {8:4.2}    {9:2.3}    '.format(ref.h, ref.k, ref.z ,ref.amp, ref.phase, ref.number, ref.iq, ref. weight, ref.background, ref.ctf)
			print("%s" % line)
			aph_file.write("%s\n" % line)


def write_sym_hkl_file(merged_reflections, filename):
	with open(filename,'w') as hkl_file:
		for ref in merged_reflections:
			line= '  {0:4d}  {1:4d}  {2:4d}     {3:8.8}    {4:8.8}       {5:2.6F}       {6:1.7F}'.format(ref.h, ref.k, ref.l,ref.amp, ref.phase, ref.fom, 1.0)
			print("%s" % line)
			hkl_file.write("%s\n" % line)



