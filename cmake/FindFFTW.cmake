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


IF(USE_FFTWD OR USE_FFTWF)

  SET(FFTW_INC_SEARCHPATH
    /usr/include
    /sw/include
    /usr/local/include
    /usr/include/fftw
    /usr/local/include/fftw
    /opt/local/include
    /programs/i386-mac/fftw/3.0.1/include
    /programs/i386-linux/fftw/3.3/include
    $ENV{FFTW3_ROOT_DIR}/include
  )

  FIND_PATH(FFTW_INCLUDE_PATH fftw3.h ${FFTW_INC_SEARCHPATH})
  MESSAGE(found: ${FFTW_INCLUDE_PATH})

  IF(FFTW_INCLUDE_PATH)
    SET(FFTW_INCLUDE ${FFTW_INCLUDE_PATH})
  ENDIF (FFTW_INCLUDE_PATH)

  IF(FFTW_INCLUDE)
    INCLUDE_DIRECTORIES( ${FFTW_INCLUDE})
  ENDIF(FFTW_INCLUDE)

  GET_FILENAME_COMPONENT(FFTW_INSTALL_BASE_PATH ${FFTW_INCLUDE_PATH} PATH)

  SET(FFTW_LIB_SEARCHPATH
    $ENV{FFTW3_ROOT_DIR}/lib64
    ${FFTW_INSTALL_BASE_PATH}/lib
    /usr/lib/fftw
    /usr/local/lib/fftw
    /usr/local/lib
    /programs/i386-linux/fftw/3.3/lib
  )

  IF(USE_FFTWD)
    MARK_AS_ADVANCED(FFTWD_LIB)
#   OPTION(FFTWD_LIB "The full path to the fftw3 library (including the library)" )
    FIND_LIBRARY(FFTWD_LIB fftw3 ${FFTW_LIB_SEARCHPATH}) #Double Precision Lib
    FIND_LIBRARY(FFTWD_THREADS_LIB fftw3_threads ${FFTW_LIB_SEARCHPATH}) #Double Precision Lib only if compiled with threads support

    IF(FFTWD_LIB)
      SET(FFTWD_FOUND 1)
      IF(FFTWD_THREADS_LIB)
        SET(FFTWD_LIB ${FFTWD_LIB} ${FFTWD_THREADS_LIB} )
      ENDIF(FFTWD_THREADS_LIB)
    ENDIF(FFTWD_LIB)
  ENDIF(USE_FFTWD)

  IF(USE_FFTWF)
    MARK_AS_ADVANCED(FFTWF_LIB)
#   OPTION(FFTWF_LIB "The full path to the fftw3f library (including the library)" )
    FIND_LIBRARY(FFTWF_LIB fftw3f ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib
    FIND_LIBRARY(FFTWF_THREADS_LIB fftw3f_threads ${FFTW_LIB_SEARCHPATH}) #Single Precision Lib only if compiled with threads support


    MESSAGE("Libs found: ${FFTWF_LIB} ${FFTWF_THREADS_LIB}")

    IF(FFTWF_LIB)
      SET(FFTWF_FOUND 1)
      SET(FFTWF_LIBS ${FFTWF_LIBS} ${FFTWF_LIB} )
      IF(FFTWF_THREADS_LIB)
	SET(FFTWF_LIBS ${FFTWF_LIBS} ${FFTWF_THREADS_LIB} )
      ENDIF(FFTWF_THREADS_LIB)
    ENDIF(FFTWF_LIB)
    MESSAGE("FFTWF_LIBS is ${FFTWF_LIBS}")
  ENDIF(USE_FFTWF)
  
  IF(FFTWD_FOUND)
    SET(FFTW_FOUND 1)
    SET(FFTW_LIB ${FFTWD_LIB}) 
  ENDIF(FFTWD_FOUND)
  
  IF(FFTWF_FOUND)
    SET(FFTW_FOUND 1)
    SET(FFTW_LIB ${FFTWF_LIB}) 
  ENDIF(FFTWF_FOUND)


ENDIF(USE_FFTWD OR USE_FFTWF)
