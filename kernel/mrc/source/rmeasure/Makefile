
rmeasure.exe: rmeasure.o mrc_ccp4.o 
	gfortran -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE \
	rmeasure.o mrc_ccp4.o ../../lib/libgen.a  -o ../../bin/rmeasure.exe
	\rm *.o

rmeasure.o: rmeasure.f
	gfortran -O3 -s -fomit-frame-pointer -Wall -falign-functions=4 \
	-D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE \
	-w -c rmeasure.f

mrc_ccp4.o: mrc_ccp4.f
	gfortran -O3 -s -fomit-frame-pointer -Wall -falign-functions=4 \
	-D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE \
	-w -c mrc_ccp4.f

library.o: library.c
        gcc -DPROTOTYPE -Dalliant -D_FILE_OFFSET_BITS=64 \
        -D_LARGEFILE_SOURCE -c library.c
#


