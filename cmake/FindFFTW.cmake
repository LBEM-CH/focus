## Last modified by Alexis Rohou on 11.02.2018
## FFTW can be compiled and subsequently linked against
## various data types.
## There is a single set of include files, and then muttiple libraries,
## One for each type.  I.e. libfftw.a-->double, libfftwf.a-->float

## The following logic belongs in the individual package
## MARK_AS_ADVANCED(USE_FFTWD)
## OPTION(USE_FFTWD "Use double precision FFTW if found" ON)
## MARK_AS_ADVANCED(USE_FFTWF)
## OPTION(USE_FFTWF "Use single precision FFTW if found" ON)

SET(FFTW_FOUND 0)

## to include FFTW even if none of the of the variables are set
#IF(NOT(USE_FFTWD OR USE_FFTWF))
#	#message(WARNING "both USE_FFTWD and USE_FFTWF ar not set, so we use single precision")
#	set(USE_FFTWD ON)
#ENDIF(NOT(USE_FFTWD OR USE_FFTWF))

#
# when we find threaded versions of the fftw library,
# we will need to also know how to link against system
# threading libraries, e.g. pthread
#
find_package( Threads )



#
# Look for double-precision first
#
IF(USE_FFTWD)

  SET(FFTWD_INC_SEARCHPATH
    /usr/include
    /sw/include
    /usr/local/include
    /usr/include/fftw
    /usr/local/include/fftw
    /opt/local/include
    /programs/i386-mac/fftw/3.0.1/include
    /programs/i386-linux/fftw/3.3/include
    $ENV{FFTW3_ROOT_DIR}/include
    $ENV{FFTW3D_ROOT_DIR}/include
  )

  FIND_PATH(FFTWD_INCLUDE_PATH fftw3.h ${FFTWD_INC_SEARCHPATH})
  MESSAGE("found fftw double: ${FFTWD_INCLUDE_PATH}")

  IF(FFTWD_INCLUDE_PATH)
    SET(FFTWD_INCLUDE ${FFTWD_INCLUDE_PATH})
  ENDIF (FFTWD_INCLUDE_PATH)

  IF(FFTWD_INCLUDE)
    INCLUDE_DIRECTORIES( ${FFTWD_INCLUDE})
  ENDIF(FFTWD_INCLUDE)

  GET_FILENAME_COMPONENT(FFTWD_INSTALL_BASE_PATH ${FFTWD_INCLUDE_PATH} PATH)

  SET(FFTWD_LIB_SEARCHPATH
    $ENV{FFTW3_ROOT_DIR}/lib64
    $ENV{FFTW3D_ROOT_DIR}/lib64
    ${FFTWD_INSTALL_BASE_PATH}/lib
    ${FFTWD_INSTALL_BASE_PATH}/lib64
    /usr/lib/fftw
    /usr/local/lib/fftw
    /usr/local/lib
    /programs/i386-linux/fftw/3.3/lib
  )


  MARK_AS_ADVANCED(FFTWD_LIB)
# OPTION(FFTWD_LIB "The full path to the fftw3 library (including the library)" )
  FIND_LIBRARY(FFTWD_LIB fftw3 ${FFTWD_LIB_SEARCHPATH}) #Double Precision Lib
  FIND_LIBRARY(FFTWD_THREADS_LIB fftw3_threads ${FFTWD_LIB_SEARCHPATH}) #Double Precision Lib only if compiled with threads support

  MESSAGE("Double-precision FFTW libs found: ${FFTWD_LIB} ${FFTWD_THREADS_LIB}")

  IF(FFTWD_LIB)
    SET(FFTWD_FOUND 1)
    IF(FFTWD_THREADS_LIB)
      SET(FFTWD_LIB ${FFTWD_LIB} ${FFTWD_THREADS_LIB} ${CMAKE_THREAD_LIBS_INIT})
      #list(APPEND CMAKE_REQUIRED_LIBRARIES "${CMAKE_THREAD_LIBS_INIT}")
    ENDIF(FFTWD_THREADS_LIB)
  ENDIF(FFTWD_LIB)
  
  IF(FFTWD_FOUND)
    SET(FFTW_FOUND 1)
    SET(FFTW_LIB ${FFTWD_LIB}) 
  ENDIF(FFTWD_FOUND)

ENDIF(USE_FFTWD)

#
# Single precision now
#

IF(USE_FFTWF)

  SET(FFTWF_INC_SEARCHPATH
    /usr/include
    /sw/include
    /usr/local/include
    /usr/include/fftw
    /usr/local/include/fftw
    /opt/local/include
    /programs/i386-mac/fftw/3.0.1/include
    /programs/i386-linux/fftw/3.3/include
    $ENV{FFTW3_ROOT_DIR}/include
    $ENV{FFTW3F_ROOT_DIR}/include
  )

  FIND_PATH(FFTWF_INCLUDE_PATH fftw3.h PATHS ${FFTWF_INC_SEARCHPATH} NO_DEFAULT_PATH)
  MESSAGE("found FFTW single: ${FFTWF_INCLUDE_PATH}")
  MESSAGE("This was the search path: ${FFTWF_INC_SEARCHPATH}")

  IF(FFTWF_INCLUDE_PATH)
    SET(FFTWF_INCLUDE ${FFTWF_INCLUDE_PATH})
  ENDIF (FFTWF_INCLUDE_PATH)

  IF(FFTWF_INCLUDE)
    INCLUDE_DIRECTORIES( ${FFTWF_INCLUDE})
  ENDIF(FFTWF_INCLUDE)

  GET_FILENAME_COMPONENT(FFTWF_INSTALL_BASE_PATH ${FFTWF_INCLUDE_PATH} PATH)

  SET(FFTWF_LIB_SEARCHPATH
    $ENV{FFTW3_ROOT_DIR}/lib64
    $ENV{FFTW3F_ROOT_DIR}/lib64
    ${FFTWF_INSTALL_BASE_PATH}/lib
    ${FFTWF_INSTALL_BASE_PATH}/lib64
    /usr/lib/fftw
    /usr/local/lib/fftw
    /usr/local/lib
    /programs/i386-linux/fftw/3.3/lib
  )



  MARK_AS_ADVANCED(FFTWF_LIB)
# OPTION(FFTWF_LIB "The full path to the fftw3f library (including the library)" )
  FIND_LIBRARY(FFTWF_LIB fftw3f ${FFTWF_LIB_SEARCHPATH}) #Single Precision Lib
  FIND_LIBRARY(FFTWF_THREADS_LIB fftw3f_threads ${FFTWF_LIB_SEARCHPATH}) #Single Precision Lib only if compiled with threads support


  MESSAGE("Single-precision FFTW libs found: ${FFTWF_LIB} ${FFTWF_THREADS_LIB}")

  IF(FFTWF_LIB)
    SET(FFTWF_FOUND 1)
    SET(FFTWF_LIBS ${FFTWF_LIBS} ${FFTWF_LIB} )
    IF(FFTWF_THREADS_LIB)
      SET(FFTWF_LIBS ${FFTWF_LIBS} ${FFTWF_THREADS_LIB} ${CMAKE_THREAD_LIBS_INIT} )
      #SET(CMAKE_REQUIRED_LIBRARIES "${CMAKE_THREAD_LIBS_INIT}")
    ENDIF(FFTWF_THREADS_LIB)
  ENDIF(FFTWF_LIB)
  MESSAGE("FFTWF_LIBS is ${FFTWF_LIBS}")

  
  
  IF(FFTWF_FOUND)
    SET(FFTW_FOUND 1)
    SET(FFTW_LIB ${FFTW_LIB} ${FFTWF_LIB}) 
  ENDIF(FFTWF_FOUND)


ENDIF(USE_FFTWF)

message ("END OF FFTW. CMAKE_REQUIRED_LIBRARIES: ${CMAKE_REQUIRED_LIBRARIES}")
