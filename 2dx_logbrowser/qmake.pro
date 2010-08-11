include($$TOP_SRCDIR/qmake.pro)

TEMPLATE = app
TARGET = 2dx_logbrowser
DESTDIR = $$TOP_SRCDIR/2dx_logbrowser/
ICON = "./resource/icon_OSX.icns"

# Input
HEADERS += LBMainWindow.h \
           LogBrowserCommons.h \
           LogDirectory.h \
           LogFileReader.h \
           LogViewer.h \
           ViewerContainer.h
SOURCES += LBMainWindow.cpp \
           LogDirectory.cpp \
           LogFileReader.cpp \
           LogViewer.cpp \
           p2dxlogbrowser.cpp \
           ViewerContainer.cpp
