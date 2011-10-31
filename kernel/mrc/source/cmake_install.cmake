# Install script for directory: /Users/marheit/code/2dx/trunk/kernel/mrc/source

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/mrc/source" TYPE FILE FILES
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_allspacea.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_avrgamphs.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_boximage.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_calangle.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_calcmag.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_calcpositions.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_callat.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_ccunbendh.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_ccunbendk.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_centric.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_centric2.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_centric3.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_checklat3.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_CompleteFriedel.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_ctfapplyk.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_DATAtranslate.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_emtilt.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_fftrans.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_findlat.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_getdirectories.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_getnumber.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_getspot.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_hand.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_hklclean.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_hklsym.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_hklsym2.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_ideal2.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_latconvert.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_latlinek.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_latlinprescal.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_lencalc.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_maintain_defocus_table.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_maketrana.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_masktrana.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_merge_compileA.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_merge_compileB.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_merge_compileM.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_merge_compilePLT.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_merge_eval.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_merge_inventory.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_merge_modifyImageParameter.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_mergeeval.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_mmboxa.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_mmlatref.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_modifypolygon.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_origtiltk.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_permutate.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_permutate_file.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_permutateref.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_phasezero.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_pickautok.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_plotres.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_plotreska.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_pltiltk.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_powerhisto.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_prep.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_primes.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_quadserchk-2.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_reciproc.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_spotfilter.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_spotgenerator.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_spotrefine.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_taperedgek.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_ttboxk.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_ttboxref.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_ttmask.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_ttrefine.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_twofile.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/angback.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/autocorrl.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/autoindexk.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/avrgamphs.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/backautok.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/boxim.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/centricp2.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/centricp4.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/ctfcalck.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/ctffinda.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/ctfsearch2.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/curvy2k.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/divideq.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/getmax.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/hcut.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/header.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/histok.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/image_convert.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/labelh.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/laserplot.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/latlinprescal.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/mmtomklcf.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/mrc2tif.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/oddremove.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/padbox.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/prepmklcf.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/refineorigin.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/scalimamp3d.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/tif2mrc.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/trmask.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/twofile.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/twolattk.for"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/byte_swap_map.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/floatcompare.c"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/Creflectionsarray.h"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/Cspotarrays.h"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/mygeometry.h"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/myvector.h"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/vGeometry.h"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_plotGraph.cc"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_tiltgeom.cc"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_tiltgeom2.cc"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/Creflectionsarray.cc"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/deftilt.cc"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/Cspotarrays.cpp"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/Makefile.in"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/source/README"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/2dx_getlat/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/ctf/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/endianness/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/image_conversion/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/incompatible/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/maximum_likelihood/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/peaksearch/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/periodogram/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/rmeasure/cmake_install.cmake")
  INCLUDE("/Users/marheit/code/2dx/trunk/kernel/mrc/source/spotIntensity/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

