CXX = g++
CXXFLAGS = -ffast-math
SOURCES = 2dx_endianTest.cpp 2dx_byteSwap.cpp endianOperations.cpp
OBJECTS = 2dx_endianTest.o 2dx_byteSwap.o endianOperations.o
BINDIR = ../../bin/
INCLUDE = -I../../include -I./
BINARIES = $(BINDIR)/2dx_endianTest.exe $(BINDIR)/2dx_byteSwap.exe

all: $(BINARIES) $(OBJECTS)

%.o: %.cpp
	$(CXX) -c $(CXXFLAGS) $(INCLUDE) $<

$(BINDIR)/%.exe: %.o endianOperations.o
	$(CXX) $(CXXFLAGS) -o $@ $< endianOperations.o  -lstdc++

clean:
	\rm -f $(BINARIES) $(OBJECTS)

