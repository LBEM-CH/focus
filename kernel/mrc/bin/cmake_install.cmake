# Install script for directory: /Users/marheit/code/2dx/trunk/kernel/mrc/bin

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
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin" TYPE FILE PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ FILES
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_allspacea.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_avrgamphs.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_boximage.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_byteSwap.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_calangle.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_calcmag.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_calcpositions.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_callat.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ccunbendh.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ccunbendk.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_centric.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_centric2.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_centric3.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_checklat3.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_CompleteFriedel.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ctfapplyk.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ctffind3.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ctftilt.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_DATAtranslate.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_emtilt.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_endianTest.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_fftrans.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_findlat.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_getdirectories.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_getlat.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_getnumber.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_getspot.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_hand.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_hklclean.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_hklsym.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_hklsym2.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ideal2.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_latconvert.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_laterror.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_latlinek.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_latlinprescal.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_lencalc.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_maintain_defocus_table.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_maketrana.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_masktrana.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_merge_compileA.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_merge_compileB.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_merge_compileM.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_merge_compilePLT.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_merge_eval.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_merge_inventory.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_merge_modifyImageParameter.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_mergeeval.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ML.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_mmboxa.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_mmlatref.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_modifypolygon.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_origtiltk.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_peaksearch.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_periodogram.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_permutate.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_permutate_file.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_permutateref.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_phasezero.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_pickautok.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_plotGraph.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_plotres.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_plotreska.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_pltiltk.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_powerhisto.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_prep.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_primes.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_quadserchk-2.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_reciproc.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_spotfilter.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_spotgenerator.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_spotrefine.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_taperedgek.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_tiltgeom.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_tiltgeom2.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ttboxk.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ttboxref.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ttmask.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_ttrefine.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/2dx_twofile.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/angback.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/autocorrl.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/autoindexk.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/avrgamphs.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/backautok.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/boxim.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/byte_swap_map.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/centricp2.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/centricp4.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/ctfcalck.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/ctffind3.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/ctffinda.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/ctfsearch2.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/curvy2k.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/deftilt.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/divideq.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/floatcompare.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/getmax.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/hcut.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/header.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/histok.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/image_convert.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/labelh.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/laserplot.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/latlinprescal.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/masktrana.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/mmtomklcf.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/mrc2tif.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/oddremove.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/padbox.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/prepmklcf.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/refineorigin.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/scalimamp3d.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/splittiff.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/tif2mrc.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/trmask.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/twofile.exe"
    "/Users/marheit/code/2dx/trunk/kernel/mrc/bin/twolattk.exe"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

