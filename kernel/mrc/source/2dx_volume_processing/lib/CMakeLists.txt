############################################################################
# CMakeLists.txt for lib folder of 2dx_volume_processing                   #
############################################################################
# Author:                                                                  #
# Nikhil Biyani  01/2015                                                   #
############################################################################
#
cmake_minimum_required(VERSION 2.6)

#==============================#
# Set Environment              #
#==============================#
file(GLOB_RECURSE CPP_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
file(GLOB_RECURSE HPP_HEADERS ${CMAKE_CURRENT_SOURCE_DIR}/*.hpp)

set(PROJECT_SOURCES ${CPP_SOURCES})
set(PROJECT_HEADERS ${HPP_HEADERS})

#==============================#
# Add library		           #
#==============================#
add_library(2dx_volume_processing ${PROJECT_SOURCES})


#==============================#
# Installation                 #
#==============================#
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/source/2dx_volume_processing_lib/${DIRECTORY})
endif(INSTALL_SRC_DIR)

install(FILES ${PROJECT_SOURCES} DESTINATION ${INSTALL_SRC_DIR})
install(FILES ${PROJECT_HEADERS} DESTINATION ${INSTALL_SRC_DIR})
