############################################################################
# CMAKE FILE FOR 2dx/lib/extensions
# Contributed by: Marcel Arheit, Nikhil Biyani
############################################################################
#
cmake_minimum_required(VERSION 2.8.11)
#
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
################################################################################
#

############################################################################
# A 2dx library "mrcImage"
############################################################################
set(LIBTITLE mrcImage)
SET(LIBVERSION 1)
project(${LIBTITLE})

set(CMAKE_CXX_FLAGS "-O3 -funroll-loops")

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
FIND_PACKAGE(Qt5 COMPONENTS Widgets REQUIRED)

IF(Qt5_FOUND)
    MESSAGE(STATUS "@LIBTITLE@: Found QT5 Widgets (Version: ${Qt5Widgets_VERSION_STRING})")
    INCLUDE_DIRECTORIES(${Qt5Widgets_INCLUDE_DIRS})
    ADD_DEFINITIONS(${Qt5Widgets_DEFINITIONS})
ELSE()
    MESSAGE(STATUS "@LIBTITLE@: QT5 not Found!")
ENDIF()

#------------------------------------------------------------------------------
# Header files 
INCLUDE_DIRECTORIES(${2DX_INCLUDE_DIR})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR}/include)

#------------------------------------------------------------------------------
# Source files

# Moc files 
# these are all the headers containing Q_OBJECT
set(MOC_HDRS
	${2DX_INCLUDE_DIR}/mrcImage.h
	${2DX_INCLUDE_DIR}/largeMRC.h
	${2DX_INCLUDE_DIR}/mrcImageFast.h 
	)

QT5_WRAP_CPP(MOC_SRCS ${MOC_HDRS})

file(GLOB SRC *.cpp)

#------------------------------------------------------------------------------
# Debug Config
set(CMAKE_DEBUG_POSTFIX -dbg)

#------------------------------------------------------------------------------
# Add Library
if(SRC)
    add_library(${LIBTITLE} ${SRC} ${MOC_SRCS})

    qt5_use_modules(${LIBTITLE} Widgets)
    TARGET_LINK_LIBRARIES(${LIBTITLE} Qt5::Widgets)

    set_propagate_deps(${LIBTITLE})
endif(SRC)

