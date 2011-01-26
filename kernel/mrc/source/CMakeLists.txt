############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.8)
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_DIR)
	set(INSTALL_DIR ${INSTALL_DIR}/${DIRECTORY})
else(INSTALL_DIR)
	set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/${DIRECTORY})
endif(INSTALL_DIR)
file(GLOB FORTRAN_SOURCE *.for)
file(GLOB C_SOURCE *.c *.h)
file(GLOB CPP_SOURCE *.cc *.cpp)
set(BUILD_FILES Makefile.in)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE} ${BUILD_FILES} README) 
install(FILES ${SOURCES} DESTINATION ${INSTALL_DIR})
add_subdirectory(2dx_getlat)
add_subdirectory(ctf)
add_subdirectory(endianness)
add_subdirectory(image_conversion)
add_subdirectory(incompatible)
add_subdirectory(maximum_likelihood)
add_subdirectory(peaksearch)
add_subdirectory(periodogram)
add_subdirectory(rmeasure)
add_subdirectory(spotIntensity)

