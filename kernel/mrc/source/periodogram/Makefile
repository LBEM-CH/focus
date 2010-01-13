CXX=g++
CFLAGS= -w -ffast-math -DUSE_THREADS_2DX -ffast-math -dead_strip -static-libgcc
SOURCES=main.cpp
LDIR = -L../../lib/
LIB=-lfftw3f -l2dx
BIN=../../bin/
EXE = 2dx_periodogram.exe
INCLUDES = -I../../include/

all: $(BIN)/$(EXE)

$(BIN)/$(EXE): $(SOURCES)
	$(CXX) $(CFLAGS) $(INCLUDES) -o $(BIN)/$(EXE) $(SOURCES) $(INCLUDES) $(LDIR) $(LIB)
clean:
	\rm -f $(BIN)/$(EXE)
