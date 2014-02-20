# Install script for directory: /Users/sthennin/Projects/2dx/kernel/2dx_merge

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/2dx_merge/scripts-standard" TYPE FILE PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ FILES
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-standard/2dx_finalmerge.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-standard/2dx_generateImageMaps.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-standard/2dx_generateMergeMap.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-standard/2dx_initialize.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-standard/2dx_merge.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-standard/2dx_mergeRefine.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-standard/2dx_QualityEvaluation.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-standard/2dx_refine.script"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/2dx_merge/scripts-custom" TYPE FILE PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ FILES
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_all_parameters.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_automatic_image.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_backup.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_backup_databases.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_cleanup.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_copyMerge.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_deleteDir.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_DetermineSwitch.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_diffmap.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_diffmapII.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_genMergeMapRegister.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_ideal.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_import_large.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_inventory.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_mapdisplay.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_mergeCustom.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_ML_3Drun.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_ML_MakeStack.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_modifyImageParameter.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_openDir.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_refreshData.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_repair_links.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/2dx_reunbend.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/Custom-1.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/Custom-2.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/Custom-3.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/prepare_2nd_project.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-custom/TTF_Correction.script"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/2dx_merge/scripts-singleparticle" TYPE FILE PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ FILES
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/AnalyzeContainer.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/CreateContainer.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/DensityClean.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/GenerateInitModels.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/LoadOrientation.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/MeasureResolution.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/NNRefinement.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/RefineInitialTiltGeometry.script"
    "/Users/sthennin/Projects/2dx/kernel/2dx_merge/scripts-singleparticle/SingleParticle_Picking.script"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

