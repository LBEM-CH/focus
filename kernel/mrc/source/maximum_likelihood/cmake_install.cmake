# Install script for directory: /Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/mrc/source/maximum_likelihood" TYPE FILE FILES
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/2dx_lowpass.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/2dx_ML.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/2dx_ML_particle.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/2dx_ML_stack.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/ainterpo3d.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/ainterpo3dbig.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/align.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/align_refer.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/amp_ph.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/apply_envelop.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/box_ft.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/cgetline.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/cross_corr.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/ctf.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/ctf_particle.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/fgetline.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/flip.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/get_resolution.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/get_units.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/igetline.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/interp.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/interpolate.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/main_ML.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/mask.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/maximum_likelihood.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/maximum_likelihood_pthread.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/normalize_image.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/quadintp.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/realspace_lattice.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/reciprocal.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/rot_refer.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/rotate.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/spec_snr.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/string_to_integer.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/string_to_real.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/Symmetrize.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/trans.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/whitening.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/whitening_particle.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/common.h"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/Makefile.in"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/readme.TXT"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/readme.TXT"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

