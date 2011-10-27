if(@LIBTITLE@_FOUND)

  message(STATUS "@LIBTITLE@ is already included")

else(@LIBTITLE@_FOUND)

  message("-- Included @LIBTITLE@-@LIBVERSION@ Library")
  #set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})

  INCLUDE_DIRECTORIES(@CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/ )

  #------------------------------------------------------------------------------
  find_package(Qt4 REQUIRED)
  if(QT4_FOUND)
    message(STATUS "@LIBTITLE@: Found QT4 at ${QT_LIBRARY_DIR}")
    SET(QT_INCLUDES  ${QT_QTCORE_INCLUDE_DIR}  ${QT_INCLUDES})
    include(${QT_USE_FILE})
    INCLUDE_DIRECTORIES(${QT_INCLUDES})
    #SET(2DX_LIBRARIES   ${QT_LIBRARY_DIR}/libQtOpenGL.so ${QT_LIBRARY_DIR}/libQtNetwork.so  ${QT_LIBRARIES} ${2DX_LIBRARIES})
    SET(2DX_LIBRARIES   ${QT_LIBRARIES} ${2DX_LIBRARIES})
    add_definitions(-D__2DX__QT4__)
  else(QT4_FOUND)
    message(STATUS "@LIBTITLE@: Qt4 not Found!")
  endif(QT4_FOUND)
  

  
  #------------------------------------------------------------------------------
  # Include myself
  if("${LIBTITLE}" STREQUAL "@LIBTITLE@")
    message(STATUS "We are building the library at the moment")
    # We are building the library at the moment
    set(LIB_@LIBTITLE@ "")
    set(@LIBTITLE@_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})
    #set(@LIBTITLE@_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    #message(STATUS "@LIBTITLE@_DIR: ${@LIBTITLE@_DIR}")
  else("${LIBTITLE}" STREQUAL "@LIBTITLE@")
    message(STATUS "@LIBTITLE@: @LIBTITLE@_BINARY_DIR is ${@LIBTITLE@_BINARY_DIR}")
    find_library(LIB_@LIBTITLE@ @LIBTITLE@ PATHS ${@LIBTITLE@_BINARY_DIR})
    message(STATUS "@LIBTITLE@: Link with ${LIB_@LIBTITLE@}")
    SET(2DX_LIBRARIES  optimized ${LIB_@LIBTITLE@} ${2DX_LIBRARIES}) 
    # Include Files of this library
    #INCLUDE_DIRECTORIES(@CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/ )
    #message(STATUS "@LIBTITLE@: Include: @CMAKE_INSTALL_PREFIX@/include/@LIBTITLE@-@LIBVERSION@/")
  endif("${LIBTITLE}" STREQUAL "@LIBTITLE@")
  
  add_definitions(-D__CONF__)
  set(@LIBTITLE@_FOUND true)

endif(@LIBTITLE@_FOUND)
