# Install script for directory: /Users/marheit/code/2dx/trunk/kernel/2dx_merge

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/2dx_merge/scripts-standard" TYPE FILE PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ FILES
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-standard/2dx_finalmerge.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-standard/2dx_generateImageMaps.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-standard/2dx_generateMergeMap.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-standard/2dx_initialize.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-standard/2dx_merge.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-standard/2dx_mergeRefine.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-standard/2dx_refine.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-standard/2dx_reunbend.script"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/2dx_merge/scripts-custom" TYPE FILE PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ FILES
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_all_parameters.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_backup.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_cleanup.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_copyMerge.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_deleteDir.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_DetermineSwitch.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_genMergeMapRegister.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_ideal.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_import_large.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_inventory.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_mapdisplay.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_MaximumLikelihood.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_mergeCustom.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_modifyImageParameter.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_openDir.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_refreshData.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/2dx_repair_links.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/Custom-1.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/Custom-2.script"
    "/Users/marheit/code/2dx/trunk/kernel/2dx_merge/scripts-custom/Custom-3.script"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

