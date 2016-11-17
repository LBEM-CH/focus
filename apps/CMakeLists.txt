#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @author Marcel Arheit
# @location focus/apps
#------------------------------------------------------------------------------

#-- CMAKE Configurations ------------------------------------------------------
cmake_minimum_required(VERSION 2.8.11)

# CMAKE extra config files --------------
SET(CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR}/cmake ${CMAKE_MODULE_PATH})
INCLUDE(GetPropagateDeps)
INCLUDE(SetPropagateDeps)
#-------------------------------------------------------------------------------

#--- C++ Compiler --------------------------------------------------------------
set(CMAKE_DEBUG_POSTFIX -dbg)   # Debug options
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -g") #C++ options
if(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -openmp -O3 -xhost")
	add_definitions( -DUSE_CILK )
else(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp -O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra")
endif(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
#-------------------------------------------------------------------------------

#--- Libraries to use ----------------------------------------------------------

# FFTW ---------------------------------
set(USE_FFTWF TRUE)
find_package(FFTW)
if(FFTWF_FOUND)
	message(STATUS "@EXETITLE@: Found FFTW at ${FFTWF_LIB}")
	SET(FOCUS_LIBRARIES ${FOCUS_LIBRARIES} ${FFTWF_LIB})
elseif(FFTWF_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTWF_FOUND)
SET(FOCUS_LIBRARIES ${FOCUS_LIBRARIES} ${FFTW_LIB})

# QT5 ---------------------------------
FIND_PACKAGE(Qt5 COMPONENTS Widgets WebKitWidgets Network Script OpenGL Concurrent REQUIRED)
IF(APPLE)
    FIND_PACKAGE(Qt5 COMPONENTS MacExtras REQUIRED)
ENDIF()

IF(Qt5_FOUND)
    MESSAGE(STATUS "@EXETITLE@: Found QT5 Widgets (Version: ${Qt5Widgets_VERSION_STRING})")
    MESSAGE(STATUS "@EXETITLE@: Found QT5 WebKitWidgets (Version: ${Qt5WebKitWidgets_VERSION_STRING})")
    MESSAGE(STATUS "@EXETITLE@: Found QT5 Network (Version: ${Qt5Network_VERSION_STRING})")
    MESSAGE(STATUS "@EXETITLE@: Found QT5 Script (Version: ${Qt5Script_VERSION_STRING})")
    MESSAGE(STATUS "@EXETITLE@: Found QT5 OpenGL (Version: ${Qt5OpenGL_VERSION_STRING})")
    INCLUDE_DIRECTORIES(${Qt5Widgets_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5WebKitWidgets_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5Network_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5Script_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5OpenGL_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5Concurrent_INCLUDE_DIRS})
    IF(APPLE)
        INCLUDE_DIRECTORIES(${Qt5MacExtras_INCLUDE_DIRS})
    ENDIF(APPLE)
    ADD_DEFINITIONS(${Qt5Widgets_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5WebKitWidgets_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5Network_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5Script_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5OepnGL_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5Concurrent_DEFINITIONS})
    IF(APPLE)
        ADD_DEFINITIONS(${Qt5MacExtras_DEFINITIONS})
    ENDIF(APPLE)
ELSE()
    MESSAGE(STATUS "@EXETITLE@: QT5 not Found!")
ENDIF()

SET(FOCUS_LIBRARIES ${FOCUS_LIBRARIES} Qt5::Widgets Qt5::WebKitWidgets Qt5::Network Qt5::Script Qt5::OpenGL Qt5::Concurrent)
IF(APPLE)
    SET(FOCUS_LIBRARIES ${FOCUS_LIBRARIES} Qt5::MacExtras)
ENDIF()

# OPENGL ---------------------------------
find_package(OpenGL)
if(OPENGL_FOUND)
        message(STATUS "Found OpenGL at ${OPENGL_LIBRARIES}")
        INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR})
        SET(FOCUS_LIBRARIES ${FOCUS_LIBRARIES} ${OPENGL_LIBRARIES})
endif(OPENGL_FOUND)
#-------------------------------------------------------------------------------

#-- ADD SUBDIRECTORIES --------------------------------------------------------
add_subdirectory(src)
add_subdirectory(fLogBrowser)
add_subdirectory(focus)
add_subdirectory(fMrcConverter)
add_subdirectory(fViewer)
add_subdirectory(resources)
add_subdirectory(plugins)
add_subdirectory(bin)
#-------------------------------------------------------------------------------