TEMPLATE = app
TARGET = 2dx_image
DESTDIR = $$TOP_SRCDIR/2dx_image/
 message("The top source dir:")
 message($$TOP_SRCDIR)

QMAKE_CXXFLAGS=-O3 -ffast-math
QMAKE_CFLAGS=-O3 -ffast-math
INCLUDEPATH+=. ../include 

macx {
  ICON = "./resource/icon_OSX.icns"
  INCLUDEPATH+=/opt/local/include 
}
prefix = $$prefix

include($$TOP_SRCDIR/qmake.pro)

macx {
  LIBS+=-L/opt/local/lib
}
LIBS += -L$$TOP_SRCDIR/lib/common -lconf -lwidgets -lextentions -lmrcImage -lfftw3f 

QT += network opengl 
QT += script webkit

#
# Install files
#
proc.path = /$(BARF)/$(QMAKE_TARGET)/proc
proc.files = proc/*
INSTALLS += proc

standard.path = /$(QMAKE_TARGET)/scripts-standard
standard.files = scripts-standard/*
INSTALLS += standard

custom.path = /$(QMAKE_TARGET)/scripts-custom
custom.files = scripts-custom
INSTALL += custom

resource.path = /$(QMAKE_TARGET)/resource
resource.files = resource/*.png
INSTALL += resource

# Input
# Input
HEADERS += centralWindow.h \
           mainWindow.h \
           navigator.h \
           controlActionsGroup.h \
           progressStamps.h \
           resultsFile.h \
           resultsParser.h \
           statusView.h \
           statusViewer.h \
           warningBox.h
SOURCES += centralWindow.cpp \
           main.cpp \
           mainWindow.cpp \
           navigator.cpp \
           controlActionsGroup.cpp \
           progressStamps.cpp \
           resultsFile.cpp \
           resultsParser.cpp \
           statusView.cpp \
           statusViewer.cpp \
           warningBox.cpp
