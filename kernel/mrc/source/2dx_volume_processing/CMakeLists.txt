############################################################################
# CMakeLists.txt for 2dx_volume_processing                                 #
############################################################################
# Author:                                                                  #
# Nikhil Biyani  01/2015                                                   #
############################################################################
#
cmake_minimum_required(VERSION 2.6)

set(CMAKE_CXX_FLAGS_RELEASE "-std=c++11 -g")

set(USE_FFTWF FALSE)
set(USE_FFTWD TRUE)
find_package(FFTW REQUIRED)

#==============================#
# Add subdirectories           #
#==============================#
add_subdirectory(lib)
add_subdirectory(executables)
