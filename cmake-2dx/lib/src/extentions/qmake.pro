CONFIG += staticlib
TEMPLATE = lib
TARGET = extentions

include($$TOP_SRCDIR/qmake.pro)


# Input
HEADERS += $$INCLUDEPATH/scriptParser.h \
	   $$INCLUDEPATH/fileWatcher.h \
	   $$INCLUDEPATH/parse_csh.h
SOURCES += scriptParser.cpp \
	   fileWatcher.cpp \
	   parse_csh.cpp
