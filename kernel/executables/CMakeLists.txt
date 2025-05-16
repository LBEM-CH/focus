############################################################################
# CMakeLists.txt for kernel/executables                                    #
############################################################################
# Author:                                                                  #
# Nikhil Biyani  03/2016                                                   #
############################################################################
#
cmake_minimum_required(VERSION 3.10)

#==============================#
# Set Environment              #
#==============================#

# Executables
file(GLOB EXEC_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
set(RUNNER_SOURCES ${EXEC_SOURCES})

set(CMAKE_CXX_FLAGS_RELEASE "-std=c++11 -g")
set(CMAKE_EXECUTABLE_SUFFIX ".exe")

#==============================#
# External Libraries           #
#==============================#
set(USE_FFTWF FALSE)
set(USE_FFTWD TRUE)
find_package(FFTW REQUIRED)

#==============================#
# Include Directories          #
#==============================#
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../toolkit/include)
SET(TCLAP_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/external/tclap)
include_directories(${TCLAP_INCLUDE_DIR})

#=============================#
# Create Executables          # 
#=============================#
# For Executables
foreach(i ${RUNNER_SOURCES})
    get_filename_component(EXECUTABLE ${i}  NAME_WE)
    
    add_executable(${EXECUTABLE} ${i})
    
    #Link to the library created in step above
    target_link_libraries(${EXECUTABLE} 
	LINK_PUBLIC 2dx_toolkit
    )
    target_link_libraries(${EXECUTABLE} 
        LINK_PUBLIC ${FFTWD_LIB}
    )

    list(APPEND EXECUTABLES_VP ${EXECUTABLE})
endforeach(i ${RUNNER_SOURCES})

#==============================#
# Installation                 #
#==============================#
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_BIN_DIR)
	set(INSTALL_EXEC_DIR ${INSTALL_BIN_DIR})
else(INSTALL_BIN_DIR)
	set(INSTALL_EXEC_DIR kernel/mrc/bin)
endif(INSTALL_BIN_DIR)

install(TARGETS ${EXECUTABLES_VP} DESTINATION ${INSTALL_EXEC_DIR})
