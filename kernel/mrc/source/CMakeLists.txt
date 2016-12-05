############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
################################################################################
# Application Name
enable_language(Fortran)


# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
else (NOT CMAKE_BUILD_TYPE)
	message(STATUS "THE BUILD TYPE IS SET TO: ${CMAKE_BUILD_TYPE}")
endif (NOT CMAKE_BUILD_TYPE)


################################################################################
# Fortran Compiler -------------------------------------------------------------
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message(STATUS "adding ${Fortran_COMPILER_NAME} specific flags")

if(Fortran_COMPILER_NAME STREQUAL "gfortran")
	#gfortran
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w ")
	set(CMAKE_Fortran_FLAGS_DEBUG "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w ")
	#set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -g")
	set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}  -static-libgcc")
	set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -static-libgcc")

	set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}")
	set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG}")

	if(APPLE)
		set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -dead_strip -D_LARGEFILE_SOURCE -arch x86_64")
		# set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -ffpe-trap=zero")
	endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "f95")
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w")
	set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -g")
	if(APPLE)
		#set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -static-libgcc")
		set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -dead_strip -static-libgcc")
		set(FORTRAN_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE}  -dead_strip -static-libgcc)
		set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -static-libgcc")
	endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "ifort")
	#ifort
	#SET(CMAKE_Fortran_FLAGS_RELEASE "-w -132 -cxxlib")
	#LIST(APPEND CMAKE_Fortran_FLAGS_RELEASE "-O3 -axN -w -132 -cxxlib")
	set(CMAKE_Fortran_FLAGS_DEBUG "-O3 -w -132 -cxxlib")
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -w -132 -cxxlib")
elseif (Fortran_COMPILER_NAME STREQUAL "g77")
	#g77
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
message ("compiler options in source: " ${CMAKE_Fortran_FLAGS_RELEASE})


################################################################################
# C Compiler -------------------------------------------------------------------
#add_definitions(-DPROTOTYPE -Dalliant -Dpowerpc -w -ffast-math -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -c)

get_filename_component(C_COMPILER_NAME ${CMAKE_C_COMPILER} NAME)
message(STATUS "adding ${C_COMPILER_NAME} specific flags")
if(C_COMPILER_NAME STREQUAL "gcc")
	set(CMAKE_C_FLAGS_RELEASE "-openMP -ffast-math  -static-libgcc")
	if(APPLE)
		set(CMAKE_C_FLAGS_RELEASE "-dead_strip ${CMAKE_C_FLAGS_RELEASE}")
	endif(APPLE)
	set(CMAKE_C_FLAGS_DEBUG  "-g -ffast-math  -static-libgcc")
elseif(C_COMPILER_NAME STREQUAL "cc")
	set(CMAKE_C_FLAGS_RELEASE "-ffast-math")
else()
  	message (STATUS "c compiler is: " ${C_COMPILER_NAME})
endif()

################################################################################
# C++ Compiler -----------------------------------------------------------------
if(CMAKE_CXX_COMPILER STREQUAL "icpc")
	set(CMAKE_CXX_FLAGS "-axN -w -132 -cxxlib")
else(CMAKE_CXX_COMPILER STREQUAL "icpc")
	#set(CMAKE_CXX_FLAGS_RELEASE "") 
	set(CMAKE_CXX_FLAGS_DEBUG  "-g -ffast-math")
endif(CMAKE_CXX_COMPILER STREQUAL "icpc")


# Header files -----------------------------------------------------------------
#include_directories(${2DX_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
file(GLOB FORTRAN_SOURCE *.for)
file(GLOB C_SOURCE *.c)
file(GLOB CPP_SOURCE *.cc *.cpp)
list(REMOVE_ITEM CPP_SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/Creflectionsarray.cc)
list(REMOVE_ITEM CPP_SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/Cspotarrays.cpp)
file(GLOB DAT_FILES *.dat)
file(GLOB BUILD_FILES Makefile.*)
file(GLOB README README*)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE} ${DAT_FILES} ${BUILD_FILES} ${README}) 
###############################################################################

set(CMAKE_DEBUG_POSTFIX -dbg)

################################################################################
# Libraries --------------------------------------------------------------------i
set(LOCAL_LIB_DIR ${CMAKE_CURRENT_BINARY_DIR}/../lib)
find_library(LIB2DX lib2dx.a PATH ${LOCAL_LIB_DIR})

if(APPLE)
find_library(LIBSTDCXX NAMES libstdc++.a libstdc++-static.a HINTS /opt/local/lib/gcc48 )  
else(APPLE)
find_library(LIBSTDCXX NAMES libstdc++.a libstdc++-static.a) 
endif(APPLE)

if(LIBSTDCXX)
	message(STATUS "KERNEL: Found stdc++ at ${LIBSTDCXX}")
	list(APPEND LIBRARIES  ${LIBSTDCXX})
else(LIBSTDCXX)
	find_library(LIBSTDCXXD stdc++)
	if(LIBSTDCXXD)
		message(STATUS "KERNEL: Could not find libstdc++.a, therefore using ${LIBSTDCXXD}")
		list(APPEND LIBRARIES  ${LIBSTDCXXD})
	else(LIBSTDCXXD)
		list(APPEND LIBRARIES stdc++)
		message(STATUS "KERNEL: Could not find libstdc++")
	endif(LIBSTDCXXD)
endif(LIBSTDCXX)

list(APPEND LOCAL_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a ${LOCAL_LIB_DIR}/libim2k.a ${LOCAL_LIB_DIR}/libmisc.a ${LOCAL_LIB_DIR}/libgen.a ${LOCAL_LIB_DIR}/libplot2k.a ${LOCAL_LIB_DIR}/libfft.a ${LOCAL_LIB_DIR}/libconv.a)
list(APPEND LIBRARIES ${2DX_LIBRARIES})

set(USE_FFTWF TRUE)
find_package(FFTW)
if(FFTWF_FOUND)
	message(STATUS "KERNEL: Found FFTW at ${FFTWF_LIBS}")
	list(APPEND LIBRARIES  ${FFTWF_LIBS} ${2DX_LIBRARIES})
	include_directories(${FFTW_INCLUDE_PATH})
elseif(FFTWF_FOUND)
	message(FATAL_ERROR "FFTWF not found!")
endif(FFTWF_FOUND)
 



################################################################################
# Executables ------------------------------------------------------------------

# create Fortran object files
set(CMAKE_FORTRAN_COMPILE_OBJECT ON)
set(CMAKE_EXECUTABLE_SUFFIX ".exe")

# fortran 
foreach(i ${FORTRAN_SOURCE})
	get_filename_component(EXECUTABLE ${i}  NAME_WE)
	add_executable(${EXECUTABLE} ${i})
	target_link_libraries(${EXECUTABLE} ${LOCAL_LIBRARIES} ${LIBRARIES})
	list(APPEND EXECUTABLES ${EXECUTABLE})
endforeach(i ${FORTRAN_SOURCE})

# c++ 
set(CPP_ADD_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/Creflectionsarray.cc ${CMAKE_CURRENT_SOURCE_DIR}/Cspotarrays.cpp)
foreach(i ${CPP_SOURCE})
	get_filename_component(EXECUTABLE ${i}  NAME_WE)
	add_executable(${EXECUTABLE} ${i} ${CPP_ADD_SOURCES})
	list(APPEND EXECUTABLES ${EXECUTABLE})
endforeach(i ${CPP_SOURCE})

# c 
foreach(i ${C_SOURCE})
	get_filename_component(EXECUTABLE ${i}  NAME_WE)
	add_executable(${EXECUTABLE} ${i})
	list(APPEND EXECUTABLES ${EXECUTABLE})
endforeach(i ${C_SOURCE})


###############################################################################
# Installation  ---------------------------------------------------------------
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/${DIRECTORY})
endif(INSTALL_SRC_DIR)

set(INSTALL_BIN_DIR kernel/mrc/bin)

# sources
install(FILES ${SOURCES} DESTINATION ${INSTALL_SRC_DIR})
# executables
install(TARGETS ${EXECUTABLES} DESTINATION ${INSTALL_BIN_DIR})
###############################################################################
# sub directories -------------------------------------------------------------
#add_subdirectory(2dx_single_particle_lib)
add_subdirectory(periodogram)
add_subdirectory(peaksearch)
add_subdirectory(2dx_getlat)
add_subdirectory(endianness)
add_subdirectory(fouriercrop)
add_subdirectory(ctf)
add_subdirectory(ctfcor)
add_subdirectory(MOVIE)
# add_subdirectory(MRCOMP)
add_subdirectory(maximum_likelihood)
add_subdirectory(diffmap)
add_subdirectory(py_util)
add_subdirectory(movie_mode_py)
add_subdirectory(RUtils)


###############################################################################
