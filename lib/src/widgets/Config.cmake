if(@LIBTITLE@_FOUND)

  message(STATUS "@LIBTITLE@ is already included")

else(@LIBTITLE@_FOUND)

  message("-- Included @LIBTITLE@-@LIBVERSION@ Library")
  set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})

  INCLUDE_DIRECTORIES(@CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/ )

  STRING(REPLACE ":" ";" LD_PATHS "$ENV{LD_LIBRARY_PATH}")
  
  #------------------------------------------------------------------------------
  set(USE_FFTWF TRUE)
  find_package(FFTW)
  if(FFTW_FOUND)
	message(STATUS "@LIBTITLE@: Found FFTW at ${FFTW_LIB}")
 	SET(2DX_LIBRARIES  ${FFTW_LIB} ${2DX_LIBRARIES})
  elseif(FFTW_FOUND)
	message(FATAL_ERROR "FFTW not found!")
  endif(FFTW_FOUND)

#  set(QT_USE_QTGUI  TRUE)
#  set(QT_USE_OPENGL TRUE)
#  set(QT_USE_QTSCRIPT TRUE)
#  set(QT_USE_NETWORK TRUE)
#  find_package(Qt4 REQUIRED)
#  if(QT4_FOUND)
#    message(STATUS "@LIBTITLE@: Found QT4 at ${QT_LIBRARY_DIR}")
#    SET(QT_INCLUDES  ${QT_QTCORE_INCLUDE_DIR}  ${QT_INCLUDES})
#    SET(QT_INCLUDES ${QT_INCLUDE_DIR}/QtGui ${QT_INCLUDES})
#    SET(QT_INCLUDES  ${QT_QTSCRIPT_INCLUDE_DIR} ${QT_INCLUDES})
#    SET(QT_INCLUDES ${QT_QTOPENGL_INCLUDE_DIR} ${QT_INCLUDES})
#    SET(QT_INCLUDES ${QT_QTNETWORK_INCLUDE_DIR} ${QT_INCLUDES})
#    set(QT_USE_QTGUI  TRUE)
#    include(${QT_USE_FILE})
#    INCLUDE_DIRECTORIES(${QT_INCLUDES})
    #SET(2DX_LIBRARIES ${QT_QTCORE_LIBRARY} ${QT_QTGUI_LIBRARY} ${QT_QTOPENGL_LIBRARY} ${QT_QTNETWORK_LIBRARY} ${QT_QTWEBKIT_LIBRARY} ${2DX_LIBRARIES})
find_package(Qt4 COMPONENTS QtCore QTGui QtOpenGL QtScript QtNetwork REQUIRED)
if(QT4_FOUND)
	message(STATUS "@LIBTITLE@: Found QT4 at ${QT_LIBRARY_DIR}")
	include(${QT_USE_FILE})
        SET(2DX_LIBRARIES ${QT_LIBRARIES} ${2DX_LIBRARIES})
        #message(STATUS "2DX_LIBRARIES ${2DX_LIBRARIES}")
endif(QT4_FOUND)
#   SET(2DX_LIBRARIES   ${QT_LIBRARIES} ${2DX_LIBRARIES})
#    #add_definitions(-D__2DX__GL__)
#    add_definitions(-D__2DX__QT4__)
#  else(QT4_FOUND)
#    message(STATUS "@LIBTITLE@: Qt4 not Found!")
#  endif(QT4_FOUND)
  

  
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
    SET(2DX_LIBRARIES  optimized ${LIB_@LIBTITLE@} ${2DX_LIBRARIES}) 
    # Include Files of this library
    INCLUDE_DIRECTORIES(@CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/ )
    message(STATUS "@LIBTITLE@: Include: @CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/")
  endif("${LIBTITLE}" STREQUAL "@LIBTITLE@")
  
  add_definitions(-D__WIDGETS__)
  set(@LIBTITLE@_FOUND true)

endif(@LIBTITLE@_FOUND)
