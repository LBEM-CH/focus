CONFIG += staticlib
TEMPLATE = lib
TARGET = conf

include($$TOP_SRCDIR/qmake.pro)


# Input
HEADERS += $$INCLUDEPATH/confData.h \
	$$INCLUDEPATH/confElement.h \
	$$INCLUDEPATH/confSection.h \
	$$INCLUDEPATH/confDelegate.h
SOURCES += confData.cpp \
	confElement.cpp \
	confSection.cpp \
	confDelegate.cpp
