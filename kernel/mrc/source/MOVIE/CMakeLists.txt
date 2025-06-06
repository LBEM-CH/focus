############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 3.10)
################################################################################
# Application Name
enable_language(Fortran)



# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

################################################################################
# Fortran Compiler -------------------------------------------------------------
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp")
set(CMAKE_Fortran_FLAGS_DEBUG "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp")
if(Fortran_COMPILER_NAME STREQUAL "gfortran")
	set(CMAKE_Fortran_FLAGS_RELEASE "-g -fbounds-check -fcheck=all -fbacktrace -O0 -Wall -Wsurprising -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -fopenmp -w")
	set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -fopenmp -g")
	if(APPLE)
		set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE ")
		set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -g -fbounds-check -fcheck=all -fbacktrace ")
	endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "f95")
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp")
	set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -g -fopenmp")
	if(APPLE)
		set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}") 
		set(FORTRAN_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE} )
		set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE ")
	endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "ifort")
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -w -132 -cxxlib")
elseif (Fortran_COMPILER_NAME STREQUAL "g77")
  	set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  	set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME STREQUAL "gfortran")
  	message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  	message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  	message ("No optimized Fortran compiler flags are known, we just try -O2...")
  	set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  	set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME STREQUAL "gfortran")

message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
message ("compiler options: " ${CMAKE_Fortran_FLAGS_RELEASE})

################################################################################
# C Compiler -------------------------------------------------------------------
set(CMAKE_C_FLAGS_RELEASE "-ffast-math")
if(APPLE)
	#set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -DPROTOTYPE -Dalliant -w -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE")
endif(APPLE)
set(CMAKE_C_FLAGS_DEBUG  "-ffast-math")

# Header files -----------------------------------------------------------------
if(NOT KERNEL_INCLUDE_DIR)
	set(KERNEL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
	message(STATUS  "MOVIE ${KERNEL_INCLUDE_DIR}")
endif(NOT KERNEL_INCLUDE_DIR)
include_directories(${KERNEL_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})


# Source files -----------------------------------------------------------------
file(GLOB FORTRAN_SOURCE *.for *.f)
file(GLOB C_SOURCE *.c *.h)
file(GLOB CPP_SOURCE *.cc *.cpp)
file(GLOB DAT_FILES *.dat)
file(GLOB BUILD_FILES Makefile.*)
file(GLOB README README*)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE} ${DAT_FILES} ${BUILD_FILES} ${README}) 

###############################################################################

set(CMAKE_DEBUG_POSTFIX -dbg)

# Libraries --------------------------------------------------------------------i
set(LOCAL_LIB_DIR ${CMAKE_CURRENT_BINARY_DIR}/../lib)
find_library(LIB2DX lib2dx.a PATH ${LOCAL_LIB_DIR})

list(APPEND LOCAL_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a ${LOCAL_LIB_DIR}/libfft.a ${LOCAL_LIB_DIR}/libconv.a)
list(APPEND LIBRARIES ${2DX_LIBRARIES})

if(FFTWF_FOUND)
        list(APPEND PER_LIBRARIES  ${FFTWF_LIB})
        include_directories(${FFTW_INCLUDE_PATH})
elseif(FFTWF_FOUND)
        message(FATAL_ERROR "FFTW not found!")
endif(FFTWF_FOUND)
if(LOCAL_LIB_DIR)
        list(APPEND PER_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a )
endif(LOCAL_LIB_DIR)


 
###############################################################################
# Executables ------------------------------------------------------------------
# create Fortran object files
set(CMAKE_EXECUTABLE_SUFFIX ".exe")
set(IOF iof.f)
set(IOC ioc.c)
set(FFT fftlib.cpp)

set(2DXMOVIEREFINE 2dx_movie_refine)
set(2DXMOVIEREFINE_SRC ${2DXMOVIEREFINE}.f)
set(2DXMOVIEREFINE_EXE ${2DXMOVIEREFINE})
add_executable(${2DXMOVIEREFINE_EXE} ${2DXMOVIEREFINE_SRC} ${IOF} ${IOC}) 

set(2DXEXERES 2dx_execute_merge_results)
set(2DXEXERES_SRC ${2DXEXERES}.f)
set(2DXEXERES_EXE ${2DXEXERES})
add_executable(${2DXEXERES_EXE} ${2DXEXERES_SRC}) 

set(2DXMARKSPOTS 2dx_mark_spots)
set(2DXMARKSPOTS_SRC ${2DXMARKSPOTS}.f)
set(2DXMARKSPOTS_EXE ${2DXMARKSPOTS})
add_executable(${2DXMARKSPOTS_EXE} ${2DXMARKSPOTS_SRC} ${IOF} ${IOC}) 

set(EXECUTABLES ${2DXEXERES_EXE} ${2DXMOVIEREFINE_EXE} ${2DXMARKSPOTS_EXE})
###############################################################################
# Installation  ---------------------------------------------------------------
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/source/${DIRECTORY})
endif(INSTALL_SRC_DIR)

if(NOT INSTALL_BIN_DIR)
	set(INSTALL_BIN_DIR kernel/mrc/bin)
endif(NOT INSTALL_BIN_DIR)


# sources
install(FILES ${SOURCES} DESTINATION ${INSTALL_SRC_DIR})
# executables
install(TARGETS ${EXECUTABLES} RUNTIME  DESTINATION ${INSTALL_BIN_DIR})

