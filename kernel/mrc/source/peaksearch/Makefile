F77 = 
GCC = gcc
GXX = g++
FFLAGS = -ffast-math -dead_strip -static-libgcc
GFLAGS = -ffast-math -dead_strip -static-libgcc
BIN = ../../bin
LDIR = -L../../lib
LFLAGS = -lfftw3f -l2dx  -lstdc++-static
HEADERS = 2dx_lowhighpass.cpp 2dx_peaksearch_fft2d.cpp 2dx_mask.cpp 2dx_peaksearcha.cpp 2dx_peaksearchb.cpp 2dx_shift.cpp
SOURCES = 2dx_peaksearch.c 
OBJECTS = 2dx_peaksearch.o 
CEXE = $(BIN)/2dx_peaksearch.exe 
INCLUDES = -I../../include/ -I./

all: $(CEXE) $(SOURCES) $(HEADERS) 

$(BIN)/2dx_peaksearch.exe: 2dx_peaksearch.c $(HEADERS)
	$(GXX) $(INCLUDES) $(GFLAGS) -o $(BIN)/2dx_peaksearch.exe 2dx_peaksearch.c $(HEADERS) $(LDIR) $(LFLAGS)

clean:
	\rm -f $(OBJECTS) $(CEXE) $(FEXE)
