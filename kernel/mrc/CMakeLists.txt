############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 3.10)
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

set(INSTALL_SRC_DIR kernel/${DIRECTORY})
set(INSTALL_BIN_DIR kernel/${DIRECTORY}/bin)


set(KERNEL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)
add_subdirectory(include)
include_directories(include)
add_subdirectory(lib)
add_subdirectory(source)
add_subdirectory(bin)
