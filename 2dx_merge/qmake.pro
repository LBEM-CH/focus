TEMPLATE = app
TARGET = 2dx_merge
DESTDIR = $$TOP_SRCDIR/2dx_merge/
FORMS = importBox.ui importTool.ui
ICON = "./resource/icon.icns"

macx {
  INCLUDEPATH+=/opt/local/include 
}

include($$TOP_SRCDIR/qmake.pro)

macx {
  LIBS+=-L/opt/local/lib
}
LIBS += -L$$TOP_SRCDIR/lib/common -lconf -lwidgets -lmrcImage -lextentions -lfftw3f

# Input
QT += network opengl 
QT += script

# Input
HEADERS += mainWindow.h projectModel.h resultsData.h resultsModule.h translator.h projectDelegate.h \
          albumViewer.h \
          imageAlbum.h \
          albumModel.h \
          albumView.h \
          albumDelegate.h \
          albumControl.h \
          importBox.h \
					importTool.h \
					imageItem.h \
          imageNameParser.h
SOURCES +=  main.cpp \
            mainWindow.cpp \
            projectModel.cpp \
            resultsData.cpp \
            resultsModule.cpp \
            translator.cpp \
            projectDelegate.cpp \
            albumViewer.cpp \
            imageAlbum.cpp\
            albumModel.cpp \
            albumDelegate.cpp \
            albumControl.cpp \
						imageItem.cpp \
            importBox.cpp \
						importTool.cpp \
            imageNameParser.cpp
