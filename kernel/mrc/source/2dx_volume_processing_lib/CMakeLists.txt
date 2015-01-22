############################################################################
# CMakeLists.txt for 2dx_volume_processing                                 #
############################################################################
# Author:                                                                  #
# Nikhil Biyani  01/2015                                                   #
############################################################################
#
cmake_minimum_required(VERSION 2.6)

#==============================#
# Set Environment              #
#==============================#
file(GLOB CPP_SOURCES *.cpp)
file(GLOB HPP_HEADERS *.hpp)
SET(PROJ_NAME      "2dx_process_hkz" )
#SET(PROJ_SOURCES   "VolumeHKL.cpp" "SymmetryOperations.cpp" "Symmetry2dx.cpp" "Reflection.cpp" "NumericalUtils.cpp" "MillerIndex.cpp" "2dx_process_hkz.cpp")
SET(PROJ_SOURCES ${CPP_SOURCES})
#SET(PROJ_HEADERS   "CommonDefinitions.hpp" "VolumeHKL.hpp" "SymmetryOperations.hpp" "Symmetry2dx.hpp" "Reflection.hpp" "NumericalUtils.hpp" "MillerIndex.hpp")
SET(PROJ_HEADERS ${HPP_HEADERS})
set(CMAKE_CXX_FLAGS_RELEASE "-std=c++11")

#=============================#
# Create Executables          # 
#=============================#
#set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/${i}.c PROPERTIES LANGUAGE CXX )
#include_directories(${PROJ_INCLUDES})
add_executable(${PROJ_NAME} ${PROJ_SOURCES})
#target_link_libraries(${PROJ_LIBRARIES})


#==============================#
# Installation                 #
#==============================#
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/source/${DIRECTORY})
endif(INSTALL_SRC_DIR)

if(NOT INSTALL_BIN_DIR)
	set(INSTALL_BIN_DIR kernel/mrc/bin)
endif(NOT INSTALL_BIN_DIR)

install(FILES ${PROJ_SOURCES} DESTINATION ${INSTALL_SRC_DIR})
install(TARGETS ${PROJ_NAME} DESTINATION ${INSTALL_BIN_DIR})
