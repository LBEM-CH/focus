# Install script for directory: /Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/mrc/source/peaksearch" TYPE FILE FILES
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/normalize.c"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/common.h"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/2dx_lowhighpass.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/2dx_mask.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/2dx_peaksearch.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/2dx_peaksearch_fft2d.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/2dx_peaksearcha.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/2dx_peaksearchb.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/2dx_shift.cpp"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/Makefile.in"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/config.h.in"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/configure.in"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/peaksearch/Makefile.in"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin" TYPE EXECUTABLE FILES "/Users/sthennin/2dx/kernel/mrc/source/peaksearch/2dx_peaksearch.exe")
  IF(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin/2dx_peaksearch.exe" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin/2dx_peaksearch.exe")
    IF(CMAKE_INSTALL_DO_STRIP)
      EXECUTE_PROCESS(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin/2dx_peaksearch.exe")
    ENDIF(CMAKE_INSTALL_DO_STRIP)
  ENDIF()
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

