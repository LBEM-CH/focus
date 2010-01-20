F77 = gfortran
GCC = gcc
GXX = g++
GFLAGS = -ffast-math -dead_strip -static-libgcc
FFLAGS = -O1 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -dead_strip -static-libgcc
BIN = ../bin
LDIR = -L../lib
LIBRARIES = ../lib/libgen.a ../lib/libmisc.a ../lib/libim2k.a ../lib/libplot2k.a ../lib/libfft.a ../lib/libconv.a ../lib/lib2dx.a
LFLAG = -l2dx -lim2k -lmisc -lgen -lplot2k -lfft -lconv -lfftw3f  -lstdc++-static
SOURCES = $(wildcard *.for) $(wildcard *.c) $(patsubst Cspotarrays.cpp,,$(patsubst Creflectionsarray.cc,,$(wildcard *.cc)))
CSOURCES = Creflectionsarray.cc Cspotarrays.cpp
OBJECTS = $(patsubst %.cpp,%.o,$(patsubst %.cc,%.o,$(CSOURCES)))
BINARIES = $(patsubst %.cc,$(BIN)/%.exe,$(patsubst %.c,$(BIN)/%.exe,$(patsubst %.for,$(BIN)/%.exe,$(SOURCES)))) 
PEAKMAKE = cd ./peaksearch; make all; cd ..
GETLATMAKE = cd ./2dx_getlat; make all; cd ..
ENDIANMAKE = cd ./endianness; make all; cd ..
CTFMAKE = cd ./ctf; make all; cd ..
PEAKMAKECLEAN = cd ./peaksearch; make clean; cd ..
GETLATMAKECLEAN = cd ./2dx_getlat; make clean; cd ..
ENDIANMAKECLEAN = cd ./endianness; make clean; cd ..
CTFMAKECLEAN = cd ./ctf; make clean; cd ..


all: $(LIBRARIES) $(BINARIES)

$(BIN)/%.exe: %.for 
	$(F77) $(FFLAGS) -o $@ $< $(LDIR) $(LFLAG)

$(BIN)/%.exe: %.cc
	$(GXX) $(GFLAGS) -o $@ $< $(CSOURCES) 

$(BIN)/%.exe: %.c
	$(GCC) $(GFLAGS) -o $@ $< 

$(PEAKSEARCH):  
	$(PEAKMAKE)	

$(GETLAT):  
	$(GETLATMAKE)	

$(ENDIAN):  
	$(ENDIANMAKE)	

$(CTF):  
	$(CTFMAKE)	

clean:
	\rm -f $(BINARIES) $(OBJECTS);
	$(PEAKMAKECLEAN)
	$(GETLATMAKECLEAN)
	$(ENDIANMAKECLEAN)
	$(CTFMAKECLEAN)
