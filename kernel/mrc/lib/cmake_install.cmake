# Install script for directory: /Users/marheit/code/2dx/trunk/kernel/mrc/lib

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/Users/marheit/code/build/install")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/mrc/lib" TYPE FILE FILES
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/2dx_conv.cpp"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/2dx_conv.o"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/2dx_fftlib-2.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/2dx_fftlib.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/2dx_func.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/ccplib.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/configure.in"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/diskio.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/fftlib.cpp"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/fftlib.o"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/font84.dat"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/fsplit"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/fsplit.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/ifftsub.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/imsubs2000.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/imsubs2.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/imsubs_common.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/lcflib.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/lib2dx.a"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/libconv.a"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/libfft.a"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/libgen.a"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/libim2k.a"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/libmisc.a"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/libplot2k.a"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/library.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/library.h"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/Makefile.in"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/miscsubs.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/mrcImage.cpp"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/mrcImage.o"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/mtzlib.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/parser.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/plot2k.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/plot2k.o"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/README"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/subs.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/symlib.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/symop.lib"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/unix.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/lib/unix.m4"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/lib/old/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

