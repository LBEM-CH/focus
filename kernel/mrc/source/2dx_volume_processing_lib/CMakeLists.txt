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
file(GLOB CPP_SOURCES ./src/Main/*.cpp ./src/DataStructure/*.cpp ./src/NumericalRecipes/*.cpp)
file(GLOB HPP_HEADERS ./include/*.hpp)
SET(PROJ_NAME      "2dx_process_hkz" )
SET(BEADGEN_EXE "beadgen_comp")
SET(MAPHIST_EXE "maphist")
SET(PROJ_SOURCES ${CPP_SOURCES})
SET(PROJ_HEADERS ${HPP_HEADERS})
set(CMAKE_CXX_FLAGS_RELEASE "-std=c++11")

#=============================#
# Create Executables          # 
#=============================#
add_executable(${PROJ_NAME} ${PROJ_SOURCES})
add_executable(${BEADGEN_EXE} ./gunnar/${BEADGEN_EXE}.c)
add_executable(${MAPHIST_EXE} ./gunnar/${MAPHIST_EXE}.c)

#=============================#
# Link Libraries          # 
#=============================#

set(USE_FFTWF FALSE)
set(USE_FFTWD TRUE)
find_package(FFTW REQUIRED)

target_link_libraries(${PROJ_NAME} ${FFTWD_LIB})

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
install(TARGETS ${BEADGEN_EXE} DESTINATION ${INSTALL_BIN_DIR})
install(TARGETS ${MAPHIST_EXE} DESTINATION ${INSTALL_BIN_DIR})
