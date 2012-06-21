############################################################################
# A 2dx CMakeLists.txt                                                     #
############################################################################
# The author was:                                                          #
# 2012 Sebastian Scherer                                                   #
############################################################################
#
cmake_minimum_required(VERSION 2.6)

# Header files -----------------------------------------------------------------
if(NOT KERNEL_INCLUDE_DIR)
	set(KERNEL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
endif(NOT KERNEL_INCLUDE_DIR)
include_directories(${KERNEL_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
file(GLOB FORTRAN_SOURCE *.for *.f)
file(GLOB C_SOURCE *.c *.h)
file(GLOB CPP_SOURCE *.cc *.cpp)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE}) 
set(PER_TARGETS 2dx_periodogram)

# Libraries --------------------------------------------------------------------i
set(USE_FFTWF TRUE)
find_package(FFTW)
if(FFTW_FOUND)
	message(STATUS "PER: Found FFTW at ${FFTW_LIB}")
	list(APPEND PER_LIBRARIES  ${FFTW_LIB})
	include_directories(${FFTW_INCLUDE})
elseif(FFTW_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTW_FOUND)
if(LOCAL_LIB_DIR)
	message(STATUS "LOCAL_LIB_DIR=${LOCAL_LIB_DIR}")
	list(APPEND PER_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a )
endif(LOCAL_LIB_DIR)
# Executables ------------------------------------------------------------------
set(LOCAL_EXECUTABLES "")
foreach(i ${PER_TARGETS})
	set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/${i}.c PROPERTIES LANGUAGE CXX )
	add_executable(${i}  ${CMAKE_CURRENT_SOURCE_DIR}/${i}.cpp)
	target_link_libraries(${i} ${PER_LIBRARIES})
	list(APPEND LOCAL_EXECUTABLES ${i})
endforeach(i ${PER_TARGETS})
# Installation  ---------------------------------------------------------------
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_DIR)
	set(INSTALL_DIR ${INSTALL_DIR}/${DIRECTORY})
else(INSTALL_DIR)
	set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/source/${DIRECTORY})
endif(INSTALL_DIR)
if(NOT INSTALL_BIN_DIR)
	set(INSTALL_BIN_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin)
endif(NOT INSTALL_BIN_DIR)
install(FILES ${SOURCES} DESTINATION ${INSTALL_DIR})
install(TARGETS ${LOCAL_EXECUTABLES} DESTINATION ${INSTALL_BIN_DIR})
