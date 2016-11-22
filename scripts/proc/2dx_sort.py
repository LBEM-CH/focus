
import os,sys

infile = open(sys.argv[1])
outfile = open(sys.argv[2],'w')

lines = infile.readlines()

lines.sort()

for line in lines:
        outfile.write(line)

infile.close()
outfile.close()




