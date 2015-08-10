############################################################################
# CMAKE FILE FOR 2dx/lib/conf
# Contributed by: Marcel Arheit, Nikhil Biyani
############################################################################
cmake_minimum_required(VERSION 2.8.11)
#
SET(CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR}/cmake ${CMAKE_MODULE_PATH})
#

############################################################################
# A 2dx library "conf"
############################################################################
SET(LIBTITLE conf)
SET(LIBVERSION 1)
PROJECT(${LIBTITLE})

SET(CMAKE_CXX_FLAGS "-std=c++11 -O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra")

INCLUDE(GetPropagateDeps)
INCLUDE(SetPropagateDeps)

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
set(MOC_HDRS
	${2DX_INCLUDE_DIR}/confData.h	
	${2DX_INCLUDE_DIR}/confDelegate.h
	${2DX_INCLUDE_DIR}/confEditor.h
	${2DX_INCLUDE_DIR}/confElement.h
	${2DX_INCLUDE_DIR}/confInput.h
	${2DX_INCLUDE_DIR}/confInterface.h
	${2DX_INCLUDE_DIR}/confManual.h
	${2DX_INCLUDE_DIR}/confModel.h
	${2DX_INCLUDE_DIR}/confSection.h
	${2DX_INCLUDE_DIR}/confSectionHeader.h
	${2DX_INCLUDE_DIR}/confValidator.h
    )

QT5_WRAP_CPP(MOC_SRCS ${MOC_HDRS})

FILE(GLOB SRC *.cpp)

#------------------------------------------------------------------------------
# Debug Config
set(CMAKE_DEBUG_POSTFIX -dbg)

#------------------------------------------------------------------------------
# Add Library
ADD_LIBRARY(${LIBTITLE} STATIC ${SRC} ${MOC_SRCS})

qt5_use_modules(${LIBTITLE} Widgets)

TARGET_LINK_LIBRARIES(${LIBTITLE} Qt5::Widgets)

set_propagate_deps(${LIBTITLE})

# Installation
INSTALL(TARGETS ${LIBTITLE} DESTINATION lib)

