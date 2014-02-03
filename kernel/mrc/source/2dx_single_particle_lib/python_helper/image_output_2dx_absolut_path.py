import sys
import os


if __name__ == '__main__':
	
	if len(sys.argv) != 5:
		sys.exit("Usage: image_out_2dx_absolut_path.py [file] [desc] [script_name] [important_flag]")

	absolute_path = sys.argv[1]
	desc = sys.argv[2]
	script_name = sys.argv[3]
	important_flag = sys.argv[4]
	
	words_cwd = os.getcwd().split("/")
	words_image = absolute_path.split("/")
	
	index = 0;
	
	for i in range(len(words_cwd)):
		if words_cwd[i] == words_image[i]:
			index += 1
			
	relative_file = str()
	
	for i in range(index, len(words_image)):
		relative_file += words_image[i] + "/"
	
	relative_file = relative_file[:-1]
	output_line = str()
	
	if important_flag == "1":
		output_line += " # IMAGE-IMPORTANT: "
	else:
		output_line += " # IMAGE: "
		
	output_line += relative_file + " <" + desc + ">\n";
	
	output_log_file = "LOGS/" + script_name + ".results"
	open(output_log_file, "a").write(output_line)
	
	print "<<@evaluate>>"
	
