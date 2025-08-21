############################################################################
# CMakeLists.txt for kernel/toolkit                                        #
############################################################################
# Author:                                                                  #
# Nikhil Biyani  03/2016                                                   #
############################################################################
#
cmake_minimum_required(VERSION 3.10)

set(CMAKE_CXX_FLAGS_RELEASE "-std=c++17 -g")

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
add_library(2dx_toolkit ${PROJECT_SOURCES})


#==============================#
# Installation                 #
#==============================#
#get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
#if(INSTALL_SRC_DIR)
#	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
#else(INSTALL_SRC_DIR)
#	set(INSTALL_SRC_DIR kernel/toolkit/${DIRECTORY})
#endif(INSTALL_SRC_DIR)
#
#install(FILES ${PROJECT_SOURCES} DESTINATION ${INSTALL_SRC_DIR})
#install(FILES ${PROJECT_HEADERS} DESTINATION ${INSTALL_SRC_DIR})
