############################################################################
# CMAKE FILE FOR 2dx/lib/extensions
# Contributed by: Marcel Arheit, Nikhil Biyani
############################################################################
#
cmake_minimum_required(VERSION 2.8.11)
#
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_MODULE_PATH})
################################################################################
#

############################################################################
# A 2dx library "widgets"
############################################################################
set(LIBTITLE widgets)
SET(LIBVERSION 1)
project(${LIBTITLE})

set(CMAKE_CXX_FLAGS "-O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra")

include(GetPropagateDeps)
include(SetPropagateDeps)

#------------------------------------------------------------------------------
# Find the dependency of FFTW
set(USE_FFTWF TRUE)
find_package(FFTW)
if(FFTWF_FOUND)
    message(STATUS "@LIBTITLE@: Found FFTW at ${FFTWF_LIB}")
    SET(@LIBTITLE@_LIBRARIES  ${FFTWF_LIB} ${@LIBTITLE@_LIBRARIES})
elseif(FFTWF_FOUND)
    message(FATAL_ERROR "FFTW not found!")
endif(FFTWF_FOUND)

#------------------------------------------------------------------------------
# Find the dependency of QT5
FIND_PACKAGE(Qt5 COMPONENTS Widgets Network Script OpenGL REQUIRED)

IF(Qt5_FOUND)
    INCLUDE_DIRECTORIES(${Qt5Widgets_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5Network_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5Script_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5OpenGL_INCLUDE_DIRS})
    ADD_DEFINITIONS(${Qt5Widgets_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5Network_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5Script_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5OepnGL_DEFINITIONS})
ELSE()
    MESSAGE(STATUS "@EXETITLE@: QT5 not Found!")
ENDIF()

# Header files -----------------------------------------------------------------
include_directories(${2DX_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_BINARY_DIR}/include)


# Source files -----------------------------------------------------------------
file(GLOB SRC *.cpp)

# Moc files 
# these are all the headers containing Q_OBJECT
set(MOC_HDRS
	${2DX_INCLUDE_DIR}/scriptProgress.h
	${2DX_INCLUDE_DIR}/aboutWindow.h
	${2DX_INCLUDE_DIR}/graphicalButton.h
	${2DX_INCLUDE_DIR}/viewContainer.h
	${2DX_INCLUDE_DIR}/LogViewer.h
	${2DX_INCLUDE_DIR}/controlBar.h
	${2DX_INCLUDE_DIR}/confInterface.h
	${2DX_INCLUDE_DIR}/confInput.h
	${2DX_INCLUDE_DIR}/confValidator.h
	${2DX_INCLUDE_DIR}/levelGroup.h
	${2DX_INCLUDE_DIR}/imagePreview.h
	${2DX_INCLUDE_DIR}/scriptModule.h
	${2DX_INCLUDE_DIR}/SpinBoxDelegate.h
	${2DX_INCLUDE_DIR}/phaseView.h
	${2DX_INCLUDE_DIR}/confSectionHeader.h
	${2DX_INCLUDE_DIR}/imageViewer.h
	${2DX_INCLUDE_DIR}/confManual.h
	${2DX_INCLUDE_DIR}/confModel.h
	${2DX_INCLUDE_DIR}/textBrowser.h
	${2DX_INCLUDE_DIR}/updateWindow.h
	${2DX_INCLUDE_DIR}/mrcGraphicsItem.h
	${2DX_INCLUDE_DIR}/resizeableStackedWidget.h
	${2DX_INCLUDE_DIR}/abstractTool.h
	${2DX_INCLUDE_DIR}/latticeTool.h
	${2DX_INCLUDE_DIR}/imageNavigator.h
	${2DX_INCLUDE_DIR}/ctfTool.h
	${2DX_INCLUDE_DIR}/fullScreenImage.h
	${2DX_INCLUDE_DIR}/selectionFFT.h
	${2DX_INCLUDE_DIR}/zoomWindow.h
	${2DX_INCLUDE_DIR}/latticeRefineTool.h
	${2DX_INCLUDE_DIR}/spotSelectTool.h
	${2DX_INCLUDE_DIR}/mouseAssignTool.h
	${2DX_INCLUDE_DIR}/displayParametersTool.h
	${2DX_INCLUDE_DIR}/confEditor.h
	${2DX_INCLUDE_DIR}/colorTool.h
	${2DX_INCLUDE_DIR}/noScrollComboBox.h
        ${2DX_INCLUDE_DIR}/blockContainer.h
)

QT5_WRAP_CPP(MOC_SRCS ${MOC_HDRS})

#------------------------------------------------------------------------------
# Debug Config
set(CMAKE_DEBUG_POSTFIX -dbg)


#------------------------------------------------------------------------------
# Add Library
add_library(${LIBTITLE} STATIC ${SRC} ${MOC_SRCS}) 

qt5_use_modules(${LIBTITLE} Widgets Network Script OpenGL)
TARGET_LINK_LIBRARIES(${LIBTITLE} Qt5::Widgets Qt5::Network Qt5::Script Qt5::OpenGL)

set_propagate_deps(${LIBTITLE})

