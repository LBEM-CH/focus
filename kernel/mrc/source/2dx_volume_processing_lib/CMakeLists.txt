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

# Executables
file(GLOB EXAMPLES_SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/examples/*.cpp)
file(GLOB EXEC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/executables/*.cpp)

file(GLOB_RECURSE CPP_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp)
file(GLOB_RECURSE HPP_HEADERS ${CMAKE_CURRENT_SOURCE_DIR}/src/*.hpp)

set(RUNNER_SOURCES ${EXAMPLES_SOURCE};${EXEC_SOURCES})
set(PROJECT_SOURCES ${CPP_SOURCES})
set(PROJECT_HEADERS ${HPP_HEADERS})

set(CMAKE_CXX_FLAGS_RELEASE "-std=c++11 -g")

set(USE_FFTWF FALSE)
set(USE_FFTWD TRUE)
find_package(FFTW REQUIRED)

#==============================#
# Include directories          #
#==============================#
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

if(APPLE)
	include_directories(/opt/local/include)
endif(APPLE)

#=============================#
# Create Executables          # 
#=============================#
# For Executables
foreach(i ${RUNNER_SOURCES})
    get_filename_component(EXECUTABLE ${i}  NAME_WE)
    add_executable(${EXECUTABLE} ${i};${PROJECT_SOURCES})
    target_link_libraries(${EXECUTABLE} ${FFTWD_LIB})

    list(APPEND EXECUTABLES_SP ${EXECUTABLE})
endforeach(i ${RUNNER_SOURCES})



#==============================#
# Add subdirectories           #
#==============================#
add_subdirectory(gunnar)

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

# executables
install(TARGETS ${EXECUTABLES_SP} DESTINATION ${INSTALL_BIN_DIR})