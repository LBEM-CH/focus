cmake_minimum_required(VERSION 3.5)

message(STATUS "Install prefix is set to ${CMAKE_INSTALL_PREFIX}")
 
project(CMakeTools)

install(FILES FindFFTW.cmake         DESTINATION share)
install(FILES FindCppUnit.cmake      DESTINATION share)
install(FILES FindEigen2.cmake       DESTINATION share)