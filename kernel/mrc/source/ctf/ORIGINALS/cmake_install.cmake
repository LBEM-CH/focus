# Install script for directory: /Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/mrc/source/ctf/ORIGINALS" TYPE FILE FILES
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/ctffind3.f"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/ctftilt.f"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/iof.f"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/ioc.c"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/ctffind3.com"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/ctffind3_mp.com"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/ctftilt.com"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/ctftilt_mp.com"
    "/Users/sthennin/Projects/2dx/kernel/mrc/source/ctf/ORIGINALS/README.txt"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

