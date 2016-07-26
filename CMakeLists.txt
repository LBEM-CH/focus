############################################################################
# @file CMakeLists.txt
# @author Nikhil Biyani
# @author Marcel Arheit
# @location 2dx
############################################################################

#-- CMAKE Configurations ------------------------------------------------------
#
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Set CMAKE Policies
#~~ TODO
#cmake_policy(SET CMP0012 NEW)

#CMAKE extra config files
add_subdirectory(cmake)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})
include(GetPropagateDeps)
include(SetPropagateDeps)
#------------------------------------------------------------------------------

#-- PROJECT Configurations ----------------------------------------------------
# 
PROJECT(2dx)
set (2dx_VERSION_MAJOR 3)
set (2dx_VERSION_MINOR 6)
set (2dx_VERSION_PATCH 0)
#------------------------------------------------------------------------------


find_program(HOSTNAME_CMD NAMES hostname)
exec_program(${HOSTNAME_CMD} ARGS OUTPUT_VARIABLE HOSTNAME)

#-- FIND PROGRAMS -------------------------------------------------------------
# 
# INTELCC
if (INTEL_CC)
  find_program(CMAKE_C_COMPILER NAMES icc)
  find_program(CMAKE_CXX_COMPILER NAMES icpc)
  find_program(CMAKE_AR NAMES xiar)
  find_program(CMAKE_LINKER NAMES xild)

  if (CMAKE_C_COMPILER MATCHES CMAKE_C_COMPILER-NOTFOUND OR
      CMAKE_CXX_COMPILER MATCHES CMAKE_CXX_COMPILER-NOTFOUND OR
      CMAKE_AR MATCHES CMAKE_AR-NOTFOUND OR
      CMAKE_LINKER MATCHES CMAKE_LINKER-NOTFOUND)
    message(FATAL_ERROR "Cannot find Intel compiler.  You may need to run `. /opt/intel/bin/compilervars.sh intel64'")
  endif ()
endif (INTEL_CC) 

# CUDA
if ("$ENV{USE_CUDA}" STREQUAL "TRUE")
	message("Try to use the GPU")
        FIND_PACKAGE(CUDA)
else()
	message("No GPU-support")
endif()

if(CUDA_FOUND)
	OPTION(ENABLE_CUDA "enable 2dx CUDA usage" ON)
	message("USING CUDA")
	add_definitions( -DUSE_CUDA )
	include_directories(/usr/local/cuda-5.5/include)
else()
	message("NOOOOOT USING CUDA")
	OPTION(ENABLE_CUDA "enable 2dx CUDA usage" OFF)
endif()
#------------------------------------------------------------------------------

#-- BASE DIRECTORIES ----------------------------------------------------------
#
# Source directory
set(2DX_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE PATH "The path of the 2dx root source directory.")
message(STATUS "The variable 2DX_ROOT_DIR is set to: ${2DX_ROOT_DIR}")

# Include directory
set(2DX_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include CACHE PATH "The path of the 2dx include directory.")
message(STATUS "The variable 2DX_INCLUDE_DIR is set to: ${2DX_INCLUDE_DIR}")

# Binary Directory
set(2DX_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE PATH "The path of the 2dx binary directory.")

#add_subdirectory(include)
#-------------------------------------------------------------------------------

#-- EXTRA CONFIGURATION -------------------------------------------------------
#
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# Configure files to replace version numbers
configure_file("changes.htm.in" "changes.htm")
configure_file("about.htm.in" "about.htm")
configure_file("current.version.in" "current.version")

file(GLOB HTM ${CMAKE_BINARY_DIR}/*.htm)
install(FILES ${HTM} DESTINATION .)
#------------------------------------------------------------------------------

#-- ADD SUBDIRECTORIES --------------------------------------------------------
#
# the libraries have to be built before by the build_all script 
add_subdirectory(external)
add_subdirectory(kernel)
#add_subdirectory(lib)
add_subdirectory(gui)
#add_subdirectory(2dx_image)
#add_subdirectory(2dx_merge)
add_subdirectory(2dx_logbrowser)
add_subdirectory(2dx_automator)
add_subdirectory(plugins)
add_subdirectory(bin)
add_subdirectory(packaging)
#------------------------------------------------------------------------------
