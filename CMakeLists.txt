############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
project(2dx)
cmake_minimum_required(VERSION 2.8)
set(2DX_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE PATH "The path of the 2dx root source directory.")
message(STATUS "The variable 2DX_ROOT_DIR is set to: ${2DX_ROOT_DIR}")
set(2DX_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include CACHE PATH "The path of the 2dx include directory.")
message(STATUS "The variable 2DX_INCLUDE_DIR is set to: ${2DX_INCLUDE_DIR}")
set(2DX_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE PATH "The path of the 2dx binary directory.")
add_subdirectory(include)
# the libraries have to be built before by the build_all script 
#add_subdirectory(lib)
add_subdirectory(kernel)
add_subdirectory(2dx_image)
add_subdirectory(2dx_merge)
add_subdirectory(2dx_logbrowser)
add_subdirectory(bin)
add_subdirectory(plugins)
install(FILES LICENSE INSTALL.SOURCE DESTINATION ${CMAKE_INSTALL_PREFIX})
#packaging
 set(CPACK_PACKAGE_NAME "2dx")
 set(CPACK_PACKAGE_VENDOR "2dx.org")
 set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "2dx -Allows interactive processing of single EM-Micrographs of 2D membrane crystals.")
 set(CPACK_PACKAGE_VERSION "3.2.0")
 set(CPACK_PACKAGE_VERSION_MAJOR "3")
 set(CPACK_PACKAGE_VERSION_MINOR "2")
 set(CPACK_PACKAGE_VERSION_PATCH "0")
 set(CPACK_PACKAGE_INSTALL_DIRECTORY "2dx")

include(CPack)
