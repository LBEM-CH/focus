if(@LIBTITLE@_FOUND)

  message(STATUS "@LIBTITLE@ is already included")

else(@LIBTITLE@_FOUND)

  message("-- Included @LIBTITLE@-@LIBVERSION@ Library")
  set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})

  INCLUDE_DIRECTORIES(@CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/ )

  #------------------------------------------------------------------------------
  set(USE_FFTWF TRUE)
  find_package(FFTW)
  if(FFTWF_FOUND)
	message(STATUS "@LIBTITLE@: Found FFTW at ${FFTWF_LIB}")
	SET(@LIBTITLE@_LIBRARIES  ${FFTWF_LIB} ${@LIBTITLE@_LIBRARIES})
  elseif(FFTWF_FOUND)
	message(FATAL_ERROR "FFTW not found!")
  endif(FFTWF_FOUND)

find_package(Qt4 COMPONENTS QtCore QTGui QtOpenGL QtScript QtNetwork REQUIRED)
if(QT4_FOUND)
	message(STATUS "@LIBTITLE@: Found QT4 at ${QT_LIBRARY_DIR}")
	include(${QT_USE_FILE})
        SET(@LIBTITLE@_LIBRARIES ${QT_LIBRARIES} ${@LIBTITLE@_LIBRARIES})
endif(QT4_FOUND)
  

  
  #------------------------------------------------------------------------------
  # Include myself
  if("${LIBTITLE}" STREQUAL "@LIBTITLE@")
    # We are building the library at the moment
    set(LIB_@LIBTITLE@ "")
    message(STATUS "@LIBTITLE@: building at the moment")
    message(STATUS "@LIBTITLE@: BINARY_DIR is ${CMAKE_CURRENT_BINARY_DIR}")
    set(@LIBTITLE@_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})
  else("${LIBTITLE}" STREQUAL "@LIBTITLE@")
    message(STATUS "@LIBTITLE@: @LIBTITLE@_BINARY_DIR is ${@LIBTITLE@_BINARY_DIR}")
    find_library(LIB_@LIBTITLE@ @LIBTITLE@ PATHS ${@LIBTITLE@_BINARY_DIR})
    message(STATUS "@LIBTITLE@: Link with ${LIB_@LIBTITLE@}")
    SET(@LIBTITLE@_LIBRARIES  optimized ${LIB_@LIBTITLE@} ${@LIBTITLE@_LIBRARIES}) 
    # Include Files of this library
    INCLUDE_DIRECTORIES(@CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/ )
    message(STATUS "@LIBTITLE@: Include: @CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/")
  endif("${LIBTITLE}" STREQUAL "@LIBTITLE@")
  
  add_definitions(-D__WIDGETS__)
  set(@LIBTITLE@_FOUND true)

endif(@LIBTITLE@_FOUND)
