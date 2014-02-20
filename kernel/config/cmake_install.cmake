# Install script for directory: /Users/sthennin/Projects/2dx/kernel/config

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/config" TYPE FILE FILES
    "/Users/sthennin/Projects/2dx/kernel/config/VARIABLE.txt"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_image-user.cfg"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_merge-user.cfg"
    "/Users/sthennin/Projects/2dx/kernel/config/status-window"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx.cfg"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx.cshrc"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_image-algo-1.jpg"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_image-algo-2.jpg"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_master.cfg"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  INCLUDE("/Users/sthennin/Projects/2dx/kernel/config/install_symlink.cmake")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/config/2dx_image" TYPE FILE FILES
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_image/about.htm"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_image/navigator_help.htm"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_image/2dx_status.html"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/config/2dx_merge" TYPE FILE FILES
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_merge/importPatterns.inf"
    "/Users/sthennin/Projects/2dx/kernel/config/2dx_merge/projectMenu.inf"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/config/resource" TYPE FILE FILES
    "/Users/sthennin/Projects/2dx/kernel/config/resource/2dx_fftrans.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/2dx_unbend1.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/2dx_unbend2.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/Bug-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/Bug-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/ctf-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/ctf-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/customScriptIcon-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/defocus-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/defocus-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/dryRun-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/dryRun-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/dryRun-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/dryRun-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/expand-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/expand-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/expand-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/expand-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/fft-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/fft-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbAqua-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbAqua-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbBlue-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbBlue-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbGreen-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbGreen-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbOrange-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbOrange-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbPurple-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbPurple-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbRed-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbRed-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/help-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/help-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/help-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/help-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/info-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/info-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/info-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/info-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/is-wrong-bg.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/lattice-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/lattice-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/lock-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/lock-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/manual-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/manual-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/manual-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/manual-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/mask-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/mask-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/merge-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/merge-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/merge.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/nullPreview.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/phori-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/phori-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/play-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/play-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/play-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/play-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/previewControl-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/previewControl-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/previewControl-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/previewControl-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/psFile.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/refresh-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/refresh-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/refresh-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/refresh-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/refresh-U.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/reprocess-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/reprocess-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/save-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/save-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/save-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/save-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/saveDark-AD.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/saveDark-AU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/saveDark-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/saveDark-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/scriptIcon-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/spots-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/spots-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/spScriptIcon.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/subScript-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/textFile.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/unbend-ID.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/unbend-IU.png"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/ControlButton.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbBlue-IN.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/gbGreen-IN.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/GlassButton.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/icon-2dx_image.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/icon-2dx_merge.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/icon.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/Lock.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/MetalButton.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/PostScript.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/saveButton-ID.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/scriptIcon.psd"
    "/Users/sthennin/Projects/2dx/kernel/config/resource/icon_OSX.icns"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

