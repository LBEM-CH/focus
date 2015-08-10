############################################################################
# CMAKE FILE FOR 2dx/lib/extensions
# Contributed by: Marcel Arheit, Nikhil Biyani
############################################################################
cmake_minimum_required(VERSION 2.8.11)
#
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
################################################################################
#

############################################################################
# A 2dx library "extensions"
############################################################################
set(LIBTITLE extentions)
SET(LIBVERSION 1)
project(${LIBTITLE})

set(CMAKE_CXX_FLAGS "-std=c++11 -O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra")

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
	${2DX_INCLUDE_DIR}/fileWatcher.h
	${2DX_INCLUDE_DIR}/scriptParser.h
)

QT5_WRAP_CPP(MOC_SRCS ${MOC_HDRS})

FILE(GLOB SRC *.cpp)

#------------------------------------------------------------------------------
# Debug Config
set(CMAKE_DEBUG_POSTFIX -dbg)

# Define compilation
if(SRC)
    add_library(${LIBTITLE} ${SRC} ${MOC_SRCS})

    qt5_use_modules(${LIBTITLE} Widgets)
    TARGET_LINK_LIBRARIES(${LIBTITLE} Qt5::Widgets)
    
    set_propagate_deps(${LIBTITLE})
endif(SRC)


