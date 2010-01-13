CFLAGS += -ffast-math
CXXFLAGS += -ffast-math
CONFIG += staticlib debug
TEMPLATE = lib
TARGET = mrcImage

include($$TOP_SRCDIR/qmake.pro)

# Input
HEADERS +=  $$INCLUDEPATH/threadedLoad.h \
  $$INCLUDEPATH/mrcImage.h \
	$$INCLUDEPATH/mrcHeader.h \
	$$INCLUDEPATH/largeMRC.h

SOURCES += threadedLoad.cpp \ 
					 mrcImage.cpp \
					 largeMRC.cpp \
	         mrcHeader.cpp
