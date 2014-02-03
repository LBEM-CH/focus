import sys
import os


if __name__ == '__main__':
	
	if len(sys.argv) != 2:
		sys.exit("Usage: image_out_2dx_absolut_path.py [script_name]")
	
	script_name = sys.argv[1]
	
	output_log_file = "LOGS/" + script_name + ".results"
	file_in = open(output_log_file, "r")
	content = file_in.read()
	file_in.close()
	
	content = content.split("\n")
	
	seen = set()
	seen_add = seen.add
	unique_content = [ x for x in content if x not in seen and not seen_add(x)]
	
	file_out = open(output_log_file, "w")
	for line in unique_content:	
		file_out.write(line + "\n")
		
	file_out.close()
	
	print "<<@evaluate>>"
	
	