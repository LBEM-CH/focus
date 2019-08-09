#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @author Marcel Arheit
# @location focus
#------------------------------------------------------------------------------

#-- CMAKE Configurations ------------------------------------------------------
#
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Set CMAKE Policies
#~~ TODO
#cmake_policy(SET CMP0012 NEW)
if(COMMAND cmake_policy)
    cmake_policy(SET CMP0011 NEW)
endif(COMMAND cmake_policy)
add_compile_options(-std=c++11)

#CMAKE extra config files
add_subdirectory(cmake)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})
include(GetPropagateDeps)
include(SetPropagateDeps)
#------------------------------------------------------------------------------

#-- PROJECT Configurations ----------------------------------------------------
# 
PROJECT(Focus)
set (FOCUS_VERSION_MAJOR 1)
set (FOCUS_VERSION_MINOR 1)
set (FOCUS_VERSION_PATCH 0)
#
# Get the current working branch
execute_process(
  COMMAND git rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
#
# Get the latest abbreviated commit hash of the working branch
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
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
	OPTION(ENABLE_CUDA "enable Focus CUDA usage" ON)
	message("USING CUDA")
	add_definitions( -DUSE_CUDA )
	include_directories(/usr/local/cuda-5.5/include)
else()
	message("NOT USING CUDA")
	OPTION(ENABLE_CUDA "enable Focus CUDA usage" OFF)
endif()
#------------------------------------------------------------------------------

#-- BASE DIRECTORIES ----------------------------------------------------------
#
# Source directory
set(FOCUS_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE PATH "The path of the FOCUS root source directory.")
message(STATUS "The variable FOCUS_ROOT_DIR is set to: ${FOCUS_ROOT_DIR}")

# Include directory
set(FOCUS_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include CACHE PATH "The path of the FOCUS include directory.")
message(STATUS "The variable FOCUS_INCLUDE_DIR is set to: ${FOCUS_INCLUDE_DIR}")

# Binary Directory
set(FOCUS_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE PATH "The path of the FOCUS binary directory.")

#add_subdirectory(include)
#-------------------------------------------------------------------------------

#-- EXTRA CONFIGURATION -------------------------------------------------------
#
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# Configure files to replace version numbers
configure_file("changes.htm.in" "changes.dat")
configure_file("current.version.in" "current.version")

file(GLOB HTM ${CMAKE_BINARY_DIR}/*.htm)
install(FILES ${HTM} DESTINATION .)
file(GLOB DAT ${CMAKE_BINARY_DIR}/*.dat)
install(FILES ${DAT} DESTINATION .)
#------------------------------------------------------------------------------

#-- ADD SUBDIRECTORIES --------------------------------------------------------
#
# the libraries have to be built before by the build_all script 
add_subdirectory(external)
add_subdirectory(kernel)
add_subdirectory(apps)
add_subdirectory(packaging)
#------------------------------------------------------------------------------

#-- COPY SCRIPTS AND WEB FOLDER AS THEY ARE -----------------------------------
#
set(EXECUTE_ALL OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
INSTALL(DIRECTORY scripts DESTINATION ./ FILES_MATCHING PATTERN "*" PERMISSIONS ${EXECUTE_ALL})
INSTALL(DIRECTORY web/status DESTINATION web FILES_MATCHING PATTERN "*")
INSTALL(DIRECTORY web/scripts DESTINATION web FILES_MATCHING PATTERN "*" PERMISSIONS ${EXECUTE_ALL})
#------------------------------------------------------------------------------
