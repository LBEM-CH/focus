############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 3.10)
################################################################################


# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

################################################################################
# Fortran Compiler -------------------------------------------------------------
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp -fallow-argument-mismatch")
set(CMAKE_Fortran_FLAGS_DEBUG "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp -fallow-argument-mismatch")
if(Fortran_COMPILER_NAME STREQUAL "gfortran")
	#gfortra
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -fopenmp -w")
	set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -fopenmp -g")
	if(APPLE)
		#set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -static-libgcc")
                #	set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -dead_strip -static-libgcc")
                #set(FORTRAN_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE}  -dead_strip -static-libgcc)
                #set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -static-libgcc")
	endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "f95")
	#gfortra
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp -fallow-argument-mismatch")
	set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -g -fopenmp")
	if(APPLE)
		#set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -static-libgcc")
		set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -dead_strip -static-libgcc")
		set(FORTRAN_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE}  -dead_strip -static-libgcc)
		set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -static-libgcc")
	endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "ifort")
	#ifort
	#LIST(APPEND CMAKE_Fortran_FLAGS_RELEASE "-O3 -axN -w -132 -cxxlib")
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -w -132 -cxxlib")
	#set(CMAKE_Fortran_FLAGS_DEBUG "-O3 -axN -w -132 -cxxlib")
elseif (Fortran_COMPILER_NAME STREQUAL "g77")
	# g77
  	set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  	set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME STREQUAL "gfortran")
  	message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  	message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  	message ("No optimized Fortran compiler flags are known, we just try -O2...")
  	set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  	set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME STREQUAL "gfortran")

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
	message(STATUS  "CTF ${KERNEL_INCLUDE_DIR}")
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


 
###############################################################################
# Executables ------------------------------------------------------------------
# create Fortran object files
set(CMAKE_EXECUTABLE_SUFFIX ".exe")
set(IOF iof.f)
set(IOC ioc.c)

set(CTFFIND3 ctffind3)
set(CTFFIND3_SRC ${CTFFIND3}.f)
set(CTFFIND3_EXE ${CTFFIND3})
add_executable(${CTFFIND3_EXE} ${CTFFIND3_SRC} ${IOF} ${IOC}) 

set(2DXCTFFIND3 2dx_ctffind3)
set(2DXCTFFIND3_SRC ${2DXCTFFIND3}.f)
set(2DXCTFFIND3_EXE ${2DXCTFFIND3})
add_executable(${2DXCTFFIND3_EXE} ${2DXCTFFIND3_SRC} ${IOF} ${IOC}) 

set(2DXCTFFIND3noOMP 2dx_ctffind3_noOMP)
set(2DXCTFFIND3noOMP_SRC ${2DXCTFFIND3noOMP}.f)
set(2DXCTFFIND3noOMP_EXE ${2DXCTFFIND3noOMP})
add_executable(${2DXCTFFIND3noOMP_EXE} ${2DXCTFFIND3noOMP_SRC} ${IOF} ${IOC}) 

set(2DXCTFTILT 2dx_ctftilt)
set(2DXCTFTILT_SRC ${2DXCTFTILT}.f)
set(2DXCTFTILT_EXE ${2DXCTFTILT})
add_executable(${2DXCTFTILT_EXE} ${2DXCTFTILT_SRC} ${IOF} ${IOC}) 

set(EXECUTABLES  ${CTFFIND3_EXE} ${2DXCTFFIND3_EXE} ${2DXCTFFIND3_ORI_EXE} ${2DXCTFTILT_EXE})
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
add_subdirectory(ORIGINALS)
# executables
install(TARGETS ${EXECUTABLES} RUNTIME  DESTINATION ${INSTALL_BIN_DIR})

