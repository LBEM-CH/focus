CONFIG += staticlib debug

#CONFIG += dynamiclib debug
#LIBS += -L$$TOP_SRCDIR/lib/common -lconf -lextentions -lmrcImage -lfftw3f 

TEMPLATE = lib
TARGET = widgets
QT += network opengl 
QT += script

include($$TOP_SRCDIR/qmake.pro)

# Input
HEADERS += $$INCLUDEPATH/scriptProgress.h \
	$$INCLUDEPATH/aboutWindow.h \
	$$INCLUDEPATH/graphicalButton.h \
	$$INCLUDEPATH/viewContainer.h \
	$$INCLUDEPATH/LogViewer.h \
	$$INCLUDEPATH/controlBar.h \
	$$INCLUDEPATH/confInterface.h \
	$$INCLUDEPATH/confInput.h \
	$$INCLUDEPATH/confValidator.h \
	$$INCLUDEPATH/levelGroup.h \
	$$INCLUDEPATH/mrcHeaderDisplay.h \
	$$INCLUDEPATH/imagePreview.h \
	$$INCLUDEPATH/scriptModule.h \ 
	$$INCLUDEPATH/SpinBoxDelegate.h \
	$$INCLUDEPATH/phaseView.h \
	$$INCLUDEPATH/confSectionHeader.h \
	$$INCLUDEPATH/imageViewer.h \
	$$INCLUDEPATH/confManual.h \
	$$INCLUDEPATH/confModel.h \
	$$INCLUDEPATH/textBrowser.h \
	$$INCLUDEPATH/updateWindow.h \
	$$INCLUDEPATH/mrcGraphicsItem.h \
	$$INCLUDEPATH/resizeableStackedWidget.h \
	$$INCLUDEPATH/abstractTool.h \
	$$INCLUDEPATH/latticeTool.h \
	$$INCLUDEPATH/imageNavigator.h

HEADERS += $$INCLUDEPATH/ctfTool.h \
	$$INCLUDEPATH/fullScreenImage.h \
	$$INCLUDEPATH/selectionFFT.h \
	$$INCLUDEPATH/zoomWindow.h \
	$$INCLUDEPATH/latticeRefineTool.h \
	$$INCLUDEPATH/spotSelectTool.h \
	$$INCLUDEPATH/navigatorHelpTool.h \
	$$INCLUDEPATH/mouseAssignTool.h \
	$$INCLUDEPATH/displayParametersTool.h \
	$$INCLUDEPATH/glWidget.h \
  $$INCLUDEPATH/autoImportTool.h \
	$$INCLUDEPATH/importManager.h \
	$$INCLUDEPATH/confEditor.h \
	$$INCLUDEPATH/colorTool.h 

SOURCES += scriptProgress.cpp \
  aboutWindow.cpp \
	graphicalButton.cpp \
	viewContainer.cpp \
	LogViewer.cpp \
	confInterface.cpp \
	confInput.cpp \
	confValidator.cpp \
	controlBar.cpp \
	levelGroup.cpp \
	mrcHeaderDisplay.cpp \
	imagePreview.cpp \
	scriptModule.cpp \
  SpinBoxDelegate.cpp \
	confSectionHeader.cpp \
	imageViewer.cpp \
  confManual.cpp \
  confModel.cpp \
  textBrowser.cpp \
  updateWindow.cpp \
	resizeableStackedWidget.cpp \
	confEditor.cpp \
  abstractTool.cpp \
  latticeTool.cpp \
	imageNavigator.cpp 
SOURCES += ctfTool.cpp \
	   fullScreenImage.cpp \
		 selectionFFT.cpp \
		 colorTool.cpp \
		 glWidget.cpp \
 	   zoomWindow.cpp \
 	   latticeRefineTool.cpp \
		 spotSelectTool.cpp \
	   navigatorHelpTool.cpp \
	   mouseAssignTool.cpp \
     phaseView.cpp \
		 displayParametersTool.cpp \
     autoImportTool.cpp \
     importManager.cpp \
		 mrcGraphicsItem.cpp 
