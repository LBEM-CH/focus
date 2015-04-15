############################################################################
# CMakeLists.txt for examples in 2dx_volume_processing                     #
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
file(GLOB EXAMPLE_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
set(RUNNER_SOURCES ${EXAMPLE_SOURCES})

set(CMAKE_CXX_FLAGS_RELEASE "-std=c++11 -g")

set(USE_FFTWF FALSE)
set(USE_FFTWD TRUE)
find_package(FFTW REQUIRED)

#==============================#
# Include Directories          #
#==============================#
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)
if(APPLE)
	include_directories(/opt/local/include)
endif(APPLE)

#=============================#
# Create Executables          # 
#=============================#
# For Executables
foreach(i ${RUNNER_SOURCES})
    get_filename_component(EXECUTABLE ${i}  NAME_WE)
    add_executable(${EXECUTABLE} ${i})
    
    target_link_libraries(${EXECUTABLE} 2dx_volume_processing)
    target_link_libraries(${EXECUTABLE} ${FFTWD_LIB})

    list(APPEND EXAMPLES_VP ${EXECUTABLE})
endforeach(i ${RUNNER_SOURCES})

#==============================#
# Installation                 #
#==============================#
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/source/2dx_volume_processing_lib/${DIRECTORY})
endif(INSTALL_SRC_DIR)

if(INSTALL_BIN_DIR)
	set(INSTALL_VOLPROCESS_DIR ${INSTALL_BIN_DIR}/2dx_volume_processing)
else(INSTALL_BIN_DIR)
	set(INSTALL_VOLPROCESS_DIR kernel/mrc/bin/2dx_volume_processing)
endif(INSTALL_BIN_DIR)

install(FILES ${RUNNER_SOURCES} DESTINATION ${INSTALL_SRC_DIR})
install(TARGETS ${EXAMPLES_VP} DESTINATION ${INSTALL_VOLPROCESS_DIR})