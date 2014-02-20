# Install script for directory: /Users/sthennin/Projects/2dx/kernel/2dx_image

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/2dx_image/scripts-standard" TYPE FILE PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ FILES
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_applyCTF.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_fftrans.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_generateMAP.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_getDefTilt.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_getLattice.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_getSampleTilt.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_getspots.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_getspots1.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_initialize.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_initialize_files.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_max_likelihood.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_refineLattice.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_unbend1.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_unbend2.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-standard/2dx_unbendSyn.script"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/2dx_image/scripts-custom" TYPE FILE PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ FILES
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_all_parameters.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_allspace.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_cleanup.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_clone.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_evaluateLattice.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_exportEMX.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_fourierSpots.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_generateSymMap.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_inventory.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_maskCrystal.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_maskLattice.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_ModifAmplitude.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_openDir.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_phaseorigin.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refine_unbend1.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refine_unbend12.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refine_unbend2.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refine_unbend22.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refine_unbendSyn.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refinedefocus.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refineParams.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refinespots.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_image/scripts-custom/2dx_refinetilt.script"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

