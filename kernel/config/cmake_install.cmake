# Install script for directory: /Users/marheit/code/2dx/trunk/kernel/config

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/config" TYPE FILE FILES
    "/Users/marheit/code/2dx/trunk/kernel/config/VARIABLE.txt"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_image-user.cfg"
    "/Users/marheit/code/2dx/trunk/kernel/config/status-window"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx.cshrc"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx.cfg"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_image-algo-1.jpg"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_image-algo-2.jpg"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_master.cfg"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/config/install_symlink.cmake")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/config/2dx_image" TYPE FILE FILES
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_image/about.htm"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_image/navigator_help.htm"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_image/2dx_status.html"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/config/2dx_merge" TYPE FILE FILES
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_merge/importPatterns.inf"
    "/Users/marheit/code/2dx/trunk/kernel/config/2dx_merge/projectMenu.inf"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/config/resource" TYPE FILE FILES
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/2dx_fftrans.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/2dx_unbend1.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/2dx_unbend2.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/Bug-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/Bug-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/ctf-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/ctf-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/customScriptIcon-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/defocus-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/defocus-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/dryRun-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/dryRun-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/dryRun-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/dryRun-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/expand-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/expand-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/expand-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/expand-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/fft-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/fft-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbAqua-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbAqua-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbBlue-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbBlue-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbGreen-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbGreen-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbOrange-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbOrange-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbPurple-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbPurple-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbRed-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbRed-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/help-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/help-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/help-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/help-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/info-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/info-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/info-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/info-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/is-wrong-bg.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/lattice-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/lattice-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/lock-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/lock-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/manual-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/manual-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/manual-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/manual-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/mask-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/mask-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/merge-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/merge-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/merge.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/nullPreview.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/phori-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/phori-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/play-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/play-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/play-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/play-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/previewControl-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/previewControl-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/previewControl-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/previewControl-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/psFile.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/refresh-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/refresh-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/refresh-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/refresh-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/refresh-U.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/reprocess-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/reprocess-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/save-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/save-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/save-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/save-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/saveDark-AD.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/saveDark-AU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/saveDark-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/saveDark-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/scriptIcon-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/spots-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/spots-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/subScript-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/textFile.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/unbend-ID.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/unbend-IU.png"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/ControlButton.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbBlue-IN.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/gbGreen-IN.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/GlassButton.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/icon-2dx_image.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/icon-2dx_merge.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/icon.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/Lock.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/MetalButton.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/PostScript.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/saveButton-ID.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/scriptIcon.psd"
    "/Users/marheit/code/2dx/trunk/kernel/config/resource/icon_OSX.icns"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

