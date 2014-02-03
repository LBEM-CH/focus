from EMAN2  import *
from sparx  import *

def getFloatList(input_list):
	output_list = []
	for item in input_list:
		output_list.append(float(item))
	return output_list
	
	
def getRealLattice(lattice, nsize):
	a1 = lattice[0]
	a2 = lattice[1]
	b1 = lattice[2]
	b2 = lattice[3]
	
	det = float(a1*b2 - b1*a2)
	
	real_lat = []
	
	real_lat.append(b2 * nsize / det)
	real_lat.append(-b1 * nsize / det)
	real_lat.append(-a2 * nsize / det)
	real_lat.append(a1 * nsize / det)
	
	return real_lat
	

def readConfigFile(filename):
	infile = open( filename, "r" )
	
	result_dict = dict()
	
	for line in infile:
		if line.startswith("set lattice"):
			result_dict['lattice'] = getFloatList(line.split('"')[1].split(','))
		if line.startswith("set phaori ="):
			result_dict['phaori'] = getFloatList(line.split('"')[1].split(','))
		if line.startswith("set TLTAXIS"):
			result_dict['TLTAXIS'] = float(line.split('"')[1])
		if line.startswith("set TLTANG"):
			result_dict['TLTANG'] = float(line.split('"')[1])
		if line.startswith("set TAXA"):
			result_dict['TAXA'] = float(line.split('"')[1])
		
	infile.close()
	
	return result_dict
	
	

def readFile(filename, offset=300):
	data = []
	infile = open( filename, "r" )

	infile.readline()
	infile.readline()
	infile.readline()
	infile.readline()
	infile.readline()
	
	line = infile.readline().split()
	size_x = float(line[0])
	size_y = float(line[1])
	
	infile.readline()

	for line in infile:
		split_line = line.split()
		x = float(split_line[0])
		y = float(split_line[1])
		p = float(split_line[2])
		if x>offset and y>offset and x<(size_x-offset) and y<(size_y-offset):
			data.append([x,y,p])
	
	infile.close()
	
	return sorted(data)
	
	


def pick(n):
	
	p_size = 200
	
	image_name = "DATA4SPARX/image_" + str(n) + ".mrc"
	cc_name = "DATA4SPARX/profile_" + str(n) + ".dat"
	cfg_name = "DATA4SPARX/config_" + str(n) + ".ctf"
	
	config_params = readConfigFile(cfg_name)
	print config_params
	
	data = readFile(cc_name)
	im = get_image(image_name)

	
	im.write_image("bdb:image%03d"%n)
	
	im = -1 * im
	
	lattice_real = getRealLattice(config_params['lattice'], im.get_xsize())
	print lattice_real
	
	x_center = (lattice_real[0] * config_params['phaori'][0]/360.0) + (lattice_real[2] * (config_params['phaori'][1])/360.0)
	y_center = (lattice_real[1] * config_params['phaori'][0]/360.0) + (lattice_real[3] * (config_params['phaori'][1])/360.0)
	offset_x = 0.5 * ( lattice_real[0] + lattice_real[2] );
	offset_y = 0.5 * ( lattice_real[1] + lattice_real[3] );

	
	print "\tnumber of peaks:", len(data)
	
	print x_center,y_center, offset_x, offset_y
	
	pick = model_blank(im.get_xsize(), im.get_xsize())
	
	counter = 0
	
	for i in range(len(data)):
		
		#print i
		
		x = data[i][0] - x_center + offset_x
		y = data[i][1] - y_center + offset_y
		#x = im.get_xsize() - (data[i][0] - offset_y)
		#y = im.get_xsize() - (data[i][1] - offset_x)
		
		pick.set_value_at(int(x), int(y), data[i][2])
		
		
		p = Util.window(im, p_size, p_size, 1, int(x) - im.get_xsize()/2, int(y) - im.get_ysize()/2, 0)
		p = ramp(p)
		st = Util.infomask(p, None, True)
		p -= st[0]
		
		if st[1] > 0.000001:
			p /= st[1]
			p.write_image("bdb:stack%03d"%n, counter)
			counter += 1
		
		
		
	pick.write_image("bdb:pick%03d"%n)
	
	ave_ali("bdb:stack%03d"%n, "ave.hdf")
	ave = get_image("ave.hdf")
	ave = ramp(ave)
	st = Util.infomask(ave, None, True)
	ave -= st[0]
	ave /= st[1]
	
	t = Transform({"type":"spider", 'phi':config_params['TLTAXIS'], 'theta':config_params['TLTANG'], 'psi':config_params['TAXA']})
	t.set_trans(Vec2f(0,0))
	ave.set_attr("xform.projection", t)
	
	ave.write_image("bdb:ave_stack", n)

if __name__ == "__main__":
	
	if len(sys.argv) != 2:
		sys.exit("Usage: python folder_name number")
		
	n = int(sys.argv[1])
	
	pick(n)

