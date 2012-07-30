############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)

# Header files -----------------------------------------------------------------
if(NOT KERNEL_INCLUDE_DIR)
	set(KERNEL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
	message(STATUS  "PEAKSEARCH ${KERNEL_INCLUDE_DIR}")
endif(NOT KERNEL_INCLUDE_DIR)
include_directories(${KERNEL_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
file(GLOB FORTRAN_SOURCE *.for *.f)
file(GLOB C_SOURCE *.c *.h)
file(GLOB CPP_SOURCE *.cc *.cpp)
list(REMOVE_ITEM CPP_SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/2dx_peaksearcha_cg.cpp)
list(REMOVE_ITEM CPP_SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/2dx_peaksearchb_cg.cpp)
list(REMOVE_ITEM CPP_SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/test.cpp)
file(GLOB DAT_FILES *.dat)
file(GLOB BUILD_FILES Makefile.* *.in)
file(GLOB README README* readme*)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE} ${DAT_FILES} ${BUILD_FILES} ${README}) 
set(EXECUTABLE 2dx_peaksearch)
list(APPEND PEAKSEARCH_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a)
if(FFTW_FOUND)
	list(APPEND PEAKSEARCH_LIBRARIES ${FFTW_LIB})
	include_directories(${FFTW_INCLUDE_PATH})
elseif(FFTW_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTW_FOUND)
set(LOCAL_LIB_DIR ${CMAKE_CURRENT_BINARY_DIR}/../lib)

# Executables ------------------------------------------------------------------
add_executable(${EXECUTABLE} ${CPP_SOURCE})
set(LOCAL_EXECUTABLES ${EXECUTABLE})
target_link_libraries(${EXECUTABLE} ${PEAKSEARCH_LIBRARIES})

# Installation  ---------------------------------------------------------------
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_DIR)
	set(INSTALL_DIR ${INSTALL_DIR}/${DIRECTORY})
else(INSTALL_DIR)
	set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/source/${DIRECTORY})
endif(INSTALL_DIR)
#if(NOT INSTALL_BIN_DIR)
set(INSTALL_BIN_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin)
#endif(NOT INSTALL_BIN_DIR)
install(FILES ${SOURCES} DESTINATION ${INSTALL_DIR})
install(TARGETS ${LOCAL_EXECUTABLES} DESTINATION ${INSTALL_BIN_DIR})
