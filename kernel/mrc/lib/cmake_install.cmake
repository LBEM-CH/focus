# Install script for directory: /Users/sthennin/Projects/2dx/kernel/mrc/lib

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/Users/sthennin/2dx")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "RELEASE")
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
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/unix.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/diskio.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/ccplib.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/subs.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/parser.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/symlib.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/library.c"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/mtzlib.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/lcflib.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/miscsubs.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/imsubs2000.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/imsubs2.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/plot2k.c"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/fftlib.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/2dx_conv.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/2dx_func.for"
    "/Users/sthennin/Projects/2dx/kernel/mrc/lib/mrcImage.cpp"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/Users/sthennin/2dx/kernel/mrc/lib/old/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

