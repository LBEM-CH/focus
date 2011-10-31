############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_DIR)
	set(INSTALL_DIR ${INSTALL_DIR}/${DIRECTORY})
else(INSTALL_DIR)
	set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/source/${DIRECTORY})
endif(INSTALL_DIR)
file(GLOB FORTRAN_SOURCE *.for *.f)
file(GLOB C_SOURCE *.c *.h)
file(GLOB CPP_SOURCE *.cc *.cpp)
file(GLOB DAT_FILES *.dat)
file(GLOB BUILD_FILES Makefile.*)
file(GLOB README README*)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE} ${DAT_FILES} ${BUILD_FILES} ${README}) 
#message(STATUS SOURCES=${SOURCES})
install(FILES ${SOURCES} DESTINATION ${INSTALL_DIR})
