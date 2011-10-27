cmake_minimum_required(VERSION 2.6)

message(STATUS "Install prefix is set to ${CMAKE_INSTALL_PREFIX}")
 
project(CMakeTools)

install(FILES FindFFTW.cmake       DESTINATION ${CMAKE_BINARY_DIR}/share)

#install(FILES build_all              DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
