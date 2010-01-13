F77 = 
GCC = gcc
FFLAGS = -O1 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -dead_strip -static-libgcc -ipo -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -w 
# CFLAGS = -O3 -ftree-vectorize -w -ffast-math -ffast-math -dead_strip -static-libgcc
CFLAGS = -DPROTOTYPE -Dalliant -Dpowerpc -w -ffast-math -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE
# DOES THIS NEED FOR UNIX:
#  -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE

BIN = ../../bin

all: $(BIN)/2dx_ctffind3.exe $(BIN)/2dx_ctftilt.exe

$(BIN)/2dx_ctffind3.exe: 2dx_ctffind3.o mrc_ccp4.o library.o
	$(F77) $(FFLAGS) mrc_ccp4.o library.o -o $@ $< 

$(BIN)/2dx_ctftilt.exe: 2dx_ctftilt.o mrc_ccp4.o library.o
	$(F77) $(FFLAGS) mrc_ccp4.o library.o -o $@ $< 

2dx_ctffind3.o: 2dx_ctffind3.f
	$(F77) $(FFLAGS) -c $< 

2dx_ctftilt.o: 2dx_ctftilt.f
	$(F77) $(FFLAGS) -c $< 

mrc_ccp4.o: mrc_ccp4.f
	$(F77) $(FFLAGS) -c $< 

library.o: library.c
	$(GCC) $(CFLAGS) -c $< 



clean:
	\rm -f ../../bin/2dx_ctffind3.exe
	\rm -f ../../bin/2dx_ctftilt.exe
	\rm -f *.o

