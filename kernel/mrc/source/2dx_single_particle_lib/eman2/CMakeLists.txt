PROJECT(EMAN)
CMAKE_MINIMUM_REQUIRED(VERSION 3.10)
SET(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS true)

cmake_policy(SET CMP0012 NEW)

IF(COMMAND cmake_policy)
       cmake_policy(SET CMP0003 NEW)
       cmake_policy(SET CMP0002 OLD)
ENDIF(COMMAND cmake_policy)

MARK_AS_ADVANCED(FORCE CMAKE_BACKWARDS_COMPATIBILITY)
MARK_AS_ADVANCED(CLEAR CMAKE_VERBOSE_MAKEFILE)
OPTION(CMAKE_VERBOSE_MAKEFILE "if all commands will be echoed to the console during the make" ON)

SET(EMAN_INSTALL_PREFIX $ENV{HOME}/EMAN2 CACHE PATH "installation prefix")
#SET(CMAKE_INSTALL_PREFIX ${EMAN_INSTALL_PREFIX} CACHE INTERNAL "installation prefix")
MARK_AS_ADVANCED(CLEAR EMAN_INSTALL_PREFIX)

#SET(EXECUTABLE_OUTPUT_PATH ${EMAN_BINARY_DIR}/bin CACHE PATH "output directory for building all executables.")
#SET(LIBRARY_OUTPUT_PATH ${EMAN_BINARY_DIR}/lib CACHE PATH "output directory for building all libraries.")
SET(EMAN_EXECUTABLE_PATH "${EXECUTABLE_OUTPUT_PATH}")
SET(EMAN_LIBRARY_PATH "${LIBRARY_OUTPUT_PATH}")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

OPTION(ENABLE_AUTODOC "enable latex/doxygen document generation and etc. " OFF)

OPTION(ENABLE_FFTW2 "enable fftw 2 support" OFF)
OPTION(ENABLE_FFTW3 "enable fftw 3 support" ON)
OPTION(ENABLE_FFTW_PLAN_CACHING "enable fftw caching" ON)
OPTION(ENABLE_NATIVE_FFT "enable native fft support" OFF)
OPTION(ENABLE_ACML_FFT "enable AMD Core Math Library fft support" OFF)
MARK_AS_ADVANCED(ENABLE_ACML_FFT)
OPTION(ENABLE_DJBFFT "enable djbfft support" OFF)
MARK_AS_ADVANCED(ENABLE_DJBFFT)

OPTION(ENABLE_OPTPP "enable Opt++ support" OFF)
OPTION(ENABLE_TAO "enable TAO support" OFF)
MARK_AS_ADVANCED(ENABLE_TAO)

OPTION(ENABLE_TIFF "enable TIFF support" OFF)
OPTION(ENABLE_HDF5 "enable HDF5 support" OFF)
OPTION(ENABLE_V4L2 "enable V4L2 framegrabbing support" OFF)
OPTION(ENABLE_PNG "enable PNG support" OFF)
OPTION(ENABLE_JPEG "enable JPEG support" OFF)

OPTION(ENABLE_OPENGL "enable EMAN2 OpenGL usage" ON)
OPTION(ENABLE_SPARX_CUDA "enable SPARX CUDA usage" OFF)
OPTION(ENABLE_EMAN_CUDA "enable EMAN2 CUDA usage" OFF)
OPTION(ENABLE_FTGL "enable EMAN2 FTGL (3D fonts) usage" OFF)
OPTION(ENABLE_STATIC_FTGL "If build EMAN with libftgl.a" OFF)
MARK_AS_ADVANCED(ENABLE_STATIC_FTGL)

#flags used for debugging and testing
OPTION(ENABLE_MEMDEBUG "enable memory debug support of objects" OFF)
OPTION(ENABLE_MEMDEBUG2 "enable memory debug support of EMData" OFF)
OPTION(ENABLE_BOUNDS_CHECKING "enable bounds checking support" OFF)
OPTION(ENABLE_RT "enable RT support" OFF)

OPTION(ENABLE_IOCACHE "enable ImageIO caching" OFF)
OPTION(ENABLE_HDFIOCACHE "enable HDF5 file caching" OFF)

#flags for optimization level. You can only turn one of following option to ON, or leave all of them to OFF.
OPTION(ENABLE_DEBUG "enable debug support" OFF)
OPTION(ENABLE_PROFILE "enable symbol generation without debug support" OFF)
OPTION(ENABLE_ATHLON "enable ATHLON specific support" OFF)
OPTION(ENABLE_X86_64 "enable x86_64 specific optimizations" OFF)
OPTION(ENABLE_WINDOWS_VC "enable visual c++ specific optimization" OFF)

OPTION(ENABLE_LEOPARD "enable OSX 10.5 support" OFF)

IF (ENABLE_LEOPARD)
	SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,-dylib_file,/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib")
	SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,-dylib_file,/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib")
ENDIF (ENABLE_LEOPARD)

IF (ENABLE_MEMDEBUG)
	ADD_DEFINITIONS(-DMEMDEBUG -W -Wall)
ENDIF (ENABLE_MEMDEBUG)

IF (ENABLE_MEMDEBUG2)
	ADD_DEFINITIONS(-DMEMDEBUG2 -W -Wall)
ENDIF (ENABLE_MEMDEBUG2)

IF (ENABLE_V4L2)
	ADD_DEFINITIONS(-DBOUNDS_CHECKING -DENABLE_V4L2)
ENDIF (ENABLE_V4L2)

IF (ENABLE_BOUNDS_CHECKING)
	ADD_DEFINITIONS(-DBOUNDS_CHECKING)
ENDIF (ENABLE_BOUNDS_CHECKING)

IF (ENABLE_DEBUG)
    SET (OPT_FLAGS "-DDEBUG -O0 -g -ansi -Wall -W -fno-inline")
ELSEIF(ENABLE_PROFILE)
    SET (OPT_FLAGS "-O2 -g -ansi")
    OPTION(ENABLE_DEBUG "enable debug support" OFF)
ELSEIF (ENABLE_ATHLON)
    SET (OPT_FLAGS "-O3 -ansi -march=athlon-xp -mfpmath=sse -msse -funroll-loops -pipe")
ELSEIF (ENABLE_X86_64)
    SET (OPT_FLAGS "-O3 -ansi -march=k8 -mfpmath=sse -m64 -pipe")
ELSEIF (ENABLE_WINDOWS_VC)
	SET (OPT_FLAGS "/Ox")
ELSE()
    SET (OPT_FLAGS "-O3 -ansi -asdf")		# This is the default
ENDIF()
#MESSAGE("OPT_FLAGS = " ${OPT_FLAGS})

################################################################################
# C++ Compiler -----------------------------------------------------------------
if(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(OPT_FLAGS "-openmp -O3 -xhost -no-prec-div -opt-prefetch -unroll-aggressive -m64 -ansi-alias")
	#set(OPT_FLAGS "-openmp -O3 -xhost")
	#set(CMAKE_CXX_FLAGS "-openmp -O2 -xhost -vec-report2")
else(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(OPT_FLAGS "-O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra")
endif(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")



IF (ENABLE_OPENGL)
# 	FIND_PATH(GL_INCLUDE_DIR gl.h /usr/include/GL)
# 	FIND_PATH(GLU_INCLUDE_DIR glu.h /usr/include/GL)
ENDIF (ENABLE_OPENGL)


IF(CMAKE_SYSTEM MATCHES "IRIX.*")
	SET(PLATFORMLIB "/usr/lib32" CACHE INTERNAL "IRIX lib32")  
    #SET(EMAN_CXX_FLAGS "-n32 -LANG:std -w2 -fullwarn -LANG:ansi-for-init-scope=ON" CACHE INTERNAL "C++ compiler flags")
    SET(EMAN_CXX_FLAGS "-n32 -woffall -LANG:std -LANG:ansi-for-init-scope=ON" CACHE INTERNAL "C++ compiler flags")
ENDIF(CMAKE_SYSTEM MATCHES "IRIX.*")

IF(CMAKE_SYSTEM MATCHES "AIX*")
    SET(EMAN_CXX_FLAGS "" CACHE INTERNAL "C++ compiler flags")
ENDIF(CMAKE_SYSTEM MATCHES "AIX*")

IF(CMAKE_SYSTEM MATCHES "OSF1-V*")
    SET(EMAN_CXX_FLAGS "-w0" CACHE INTERNAL "C++ compiler flags")
ENDIF(CMAKE_SYSTEM MATCHES "OSF1-V*")

IF(CMAKE_COMPILER_IS_GNUCXX)
    #SET(EMAN_CXX_FLAGS "-Wall -Werror" CACHE INTERNAL "EMAN CXX FLAGS")
    IF (ENABLE_DEBUG)
    	SET(EMAN_CXX_FLAGS "-g -Wall -W -Wcast-qual -Wwrite-strings" CACHE INTERNAL "EMAN CXX FLAGS")
	SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -g")
	SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -g")
    ELSE (ENABLE_DEBUG)
    	SET(EMAN_CXX_FLAGS "-Wall" CACHE INTERNAL "EMAN CXX FLAGS")
    ENDIF (ENABLE_DEBUG)
    SET(PLATFORMLIB "/usr/lib64" CACHE INTERNAL "lib64")
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

SET(CMAKE_C_COMPILER ${CMAKE_CXX_COMPILER})
SET(CMAKE_ORIG_FLAGS ${CMAKE_CXX_FLAGS})
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${EMAN_CXX_FLAGS} ${OPT_FLAGS}")

MARK_AS_ADVANCED(CMAKE_C_FLAGS)
MARK_AS_ADVANCED(CMAKE_CXX_FLAGS)

# for 64-bit large file support
ADD_DEFINITIONS(-D_LARGEFILE_SOURCE)
ADD_DEFINITIONS(-D_FILE_OFFSET_BITS=64)
ADD_DEFINITIONS(-D_LARGE_FILES)

INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFiles.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckFunctionExists.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckTypeSize.cmake)

MACRO(EMAN_CHECK_FUNCTION FUNCTION VARIABLE)
    CHECK_FUNCTION_EXISTS(${FUNCTION} ${VARIABLE})
    IF(${VARIABLE})
		ADD_DEFINITIONS(-D${VARIABLE})
    ENDIF(${VARIABLE})
ENDMACRO(EMAN_CHECK_FUNCTION)

EMAN_CHECK_FUNCTION(fseek64 HAVE_FSEEK64)
EMAN_CHECK_FUNCTION(fseeko HAVE_FSEEKO)
EMAN_CHECK_FUNCTION(ftell64 HAVE_FTELL64)
EMAN_CHECK_FUNCTION(ftello HAVE_FTELLO)
# end for 64-bit large file support

IF(ENABLE_IOCACHE)
	ADD_DEFINITIONS(-DIMAGEIO_CACHE)
ENDIF(ENABLE_IOCACHE)

IF(ENABLE_HDFIOCACHE)
    ADD_DEFINITIONS(-DHDFIO_CACHE)
ENDIF(ENABLE_HDFIOCACHE)

SET(INCLUDE_DIRS
	${EMAN_SOURCE_DIR}/libEM 
	$ENV{HOME}/include 
	$ENV{FFTWDIR}/include 
	$ENV{GSLDIR}/include
	$ENV{GSL_IND}
        $ENV{FFTW3_ROOT_DIR}/include 
	/usr/local/include 
	/usr/local/cuda/include
	/Developer/SDKs/MacOSX10.5.sdk/usr/X11/include
	/usr/include
	/usr/include/gsl
	/sw/include
	/usr/include/freetype2
	/opt/local/include
	/opt/local/include/gsl
	/opt/local/include/boost
	/opt/local/include/freetype2
	)


SET(LIB_DIRS
	$ENV{EMANDIR}/lib 
	$ENV{HOME}/lib 
	${PLATFORMLIB} 
	$ENV{FFTWDIR}/lib
	$ENV{FFTW3_ROOT_DIR}/lib64
	$ENV{GSLDIR}/lib
	$ENV{GSL_DIR}
	/usr/local/lib
	/usr/local/cuda/lib
	/usr/lib64 
	/usr/lib 
	/sw/lib
	/opt/local/lib
	)


MACRO(CHECK_REQUIRED_LIB2 upper lower header lower2 header2)
    FIND_PATH(${upper}_INCLUDE_PATH ${header} $ENV{${upper}DIR}/include ${INCLUDE_DIRS})
    IF(${upper}_INCLUDE_PATH)
    ELSE (${upper}_INCLUDE_PATH)
		FIND_PATH(${upper}_INCLUDE_PATH ${header2} $ENV{${upper}DIR}/include ${INCLUDE_DIRS})
    ENDIF(${upper}_INCLUDE_PATH)
    
    IF(${upper}_INCLUDE_PATH)
		FIND_LIBRARY(${upper}_LIBRARY NAMES ${lower} ${lower2} PATHS $ENV{${upper}DIR}/lib ${LIB_DIRS})		
		IF(${upper}_LIBRARY)
		ELSE(${upper}_LIBRARY)
			MESSAGE(SEND_ERROR "ERROR: ${upper} not found. please install ${upper} first!")
		ENDIF(${upper}_LIBRARY)
    ELSE(${upper}_INCLUDE_PATH)
		MESSAGE(SEND_ERROR "ERROR: ${upper} not found. please install ${upper} first!")
    ENDIF(${upper}_INCLUDE_PATH)
ENDMACRO(CHECK_REQUIRED_LIB2)

MACRO(CHECK_REQUIRED_LIB upper lower header)
    CHECK_REQUIRED_LIB2(${upper} ${lower} ${header} "" "")
ENDMACRO(CHECK_REQUIRED_LIB)

MACRO(CHECK_OPTIONAL_LIB upper lower header)
    FIND_PATH(${upper}_INCLUDE_PATH ${header} $ENV{${upper}DIR}/include ${INCLUDE_DIRS}) 
    
    IF(${upper}_INCLUDE_PATH)
        FIND_LIBRARY(${upper}_LIBRARY NAMES ${lower} PATHS $ENV{${upper}DIR}/lib ${LIB_DIRS})
		
        IF(${upper}_LIBRARY)
			ADD_DEFINITIONS(-DEM_${upper})
			INCLUDE_DIRECTORIES(${${upper}_INCLUDE_PATH})
        ELSE(${upper}_LIBRARY)    
			#MESSAGE("WARNING: ${upper} not found, ENABLE_${upper} is set back to OFF")
			OPTION(ENABLE_${upper} "if enable ${upper} support" OFF)
        ENDIF(${upper}_LIBRARY)
		
    ELSE(${upper}_INCLUDE_PATH)
		#MESSAGE("WARNING: ${upper} not found, ENABLE_${upper} is set back to OFF")
        OPTION(ENABLE_${upper} "if enable ${upper} support" OFF)
    ENDIF(${upper}_INCLUDE_PATH)
ENDMACRO(CHECK_OPTIONAL_LIB)

MACRO(CHECK_LIB_ONLY upper lower)
    FIND_LIBRARY(${upper}_LIBRARY NAMES ${lower} PATHS $ENV{${upper}DIR}/lib ${LIB_DIRS})
ENDMACRO(CHECK_LIB_ONLY)

MACRO(CHECK_PYTHON)
	SET(PYTHON_FOUND FALSE)
	
	FOREACH(PYVER ${ARGV})
		IF(NOT PYTHON_FOUND)
			IF (WIN32) 
				FIND_PATH(PYTHON_INCLUDE_PATH Python.h
					$ENV{PYTHON_ROOT}/include C:/Python${PYVER}/include)

				FIND_LIBRARY(PYTHON_LIBRARY NAMES python${PYVER} PATHS 
					$ENV{PYTHON_ROOT}/libs C:/Python${PYVER}/libs)
				
			ELSE (WIN32)
				FIND_PATH(PYTHON_INCLUDE_PATH Python.h
					$ENV{PYTHON_ROOT}/include/python${PYVER}
					/usr/local/include/python${PYVER}
					/usr/include/python${PYVER})
				
				FIND_LIBRARY(PYTHON_LIBRARY NAMES python${PYVER} PATHS
					$ENV{PYTHON_ROOT}/lib/python${PYVER}/config
					/usr/local/lib/python${PYVER}/config
					/usr/lib/python${PYVER}/config
					/usr/lib64/python${PYVER}/config
					$ENV{PYTHON_ROOT}/lib /usr/local/lib /usr/lib)

			ENDIF (WIN32)
			
			IF(PYTHON_INCLUDE_PATH)
				IF(PYTHON_LIBRARY)
					SET(PYTHON_FOUND TRUE)
				ENDIF(PYTHON_LIBRARY)
			ENDIF(PYTHON_INCLUDE_PATH)
			
		ENDIF(NOT PYTHON_FOUND)
    ENDFOREACH(PYVER)
ENDMACRO(CHECK_PYTHON)

IF(ENABLE_FFTW2)
    OPTION(ENABLE_NFFT "enable nfft support" OFF)
    OPTION(ENABLE_FFTW3 "enable fftw 3 support" OFF) 
    OPTION(ENABLE_NATIVE_FFT "enable native fft support" OFF)
    OPTION(ENABLE_ACML_FFT "enable AMD Core Math Library fft support" OFF)

	IF (WIN32)
		CHECK_REQUIRED_LIB2(FFTW2 FFTW2dll sfftw.h fftw fftw.h)
		CHECK_REQUIRED_LIB2(RFFTW2 RFFTW2dll srfftw.h rfftw rfftw.hx)
	ELSE (WIN32)
		CHECK_REQUIRED_LIB2(FFTW2 sfftw sfftw.h fftw fftw.h)
		CHECK_REQUIRED_LIB2(RFFTW2 srfftw srfftw.h rfftw rfftw.hx)
	ENDIF (WIN32)

	ADD_DEFINITIONS(-DFFTW2)
    SET(FFTW_LIBRARIES ${RFFTW2_LIBRARY} ${FFTW2_LIBRARY})
    SET(FFTW_INCLUDE_PATH ${FFTW2_INCLUDE_PATH})
    
    IF(ENABLE_NFFT)
    	CHECK_REQUIRED_LIB(FFTW2D fftw fftw.h)
    	CHECK_REQUIRED_LIB(NFFT nfft nfft.h)
    	ADD_DEFINITIONS(-DNFFT)
		INCLUDE_DIRECTORIES(${NFFT_INCLUDE_PATH}) 
    ENDIF(ENABLE_NFFT)
ENDIF(ENABLE_FFTW2)

IF(ENABLE_FFTW3)
    OPTION(ENABLE_NFFT2 "enable nfft2 support" OFF)
    OPTION(ENABLE_FFTW2 "enable fftw 2 support" OFF) 
    OPTION(ENABLE_NATIVE_FFT "enable native fft support" OFF)
    OPTION(ENABLE_ACML_FFT "enable AMD Core Math Library fft support" OFF)
  
	if($ENV{BRUTUS_2DX})
		MESSAGE("On brutus")	        
        else()
                CHECK_REQUIRED_LIB(FFTW3 fftw3f fftw3.h)
        endif()

  
   # CHECK_REQUIRED_LIB(FFTW3 fftw3f fftw3.h)
    ADD_DEFINITIONS(-DFFTW3)
    SET(FFTW_LIBRARIES ${FFTW3_LIBRARY})
    SET(FFTW_INCLUDE_PATH ${FFTW3_INCLUDE_PATH})
    
    IF(ENABLE_NFFT2)
    	CHECK_REQUIRED_LIB(FFTW3D fftw3 fftw3.h)
    	CHECK_REQUIRED_LIB(NFFT2 nfft nfft.h)
    	ADD_DEFINITIONS(-DNFFT2)
		INCLUDE_DIRECTORIES(${NFFT2_INCLUDE_PATH}) 
    ENDIF(ENABLE_NFFT2)
ENDIF(ENABLE_FFTW3)

IF(ENABLE_FFTW_PLAN_CACHING)
	ADD_DEFINITIONS(-DFFTW_PLAN_CACHING)
ENDIF(ENABLE_FFTW_PLAN_CACHING)

IF(ENABLE_ACML_FFT)
	OPTION(ENABLE_FFTW2 "enable fftw 2 support" OFF) 
	OPTION(ENABLE_FFTW3 "enable fftw 3 support" OFF)
	OPTION(ENABLE_NATIVE_FFT "enable native fft support" OFF)
	
	CHECK_REQUIRED_LIB(ACML acml acml.h)
	FIND_LIBRARY(G2C_LIBRARY NAMES g2c PATHS
		/usr/lib64
		/usr/lib
		/usr/local/lib
		$ENV{HOME}/lib )
	ADD_DEFINITIONS(-DACML)
	SET(ACML_LIBRARIES ${ACML_LIBRARY})
    SET(ACML_INCLUDE_PATH ${ACML_INCLUDE_PATH})
ENDIF(ENABLE_ACML_FFT)
	
IF(ENABLE_NATIVE_FFT)
	ADD_DEFINITIONS(-DNATIVE_FFT)
	OPTION(ENABLE_FFTW2 "enable fftw 2 support" OFF)
	OPTION(ENABLE_FFTW3 "enable fftw 3 support" OFF)
	OPTION(ENABLE_ACML_FFT "enable AMD Core Math Library fft support" OFF)
ENDIF(ENABLE_NATIVE_FFT)

IF(ENABLE_DJBFFT)
	OPTION(ENABLE_NFFT2 "enable nfft2 support" OFF)
	CHECK_REQUIRED_LIB(DJBFFT fftc4.h fftr4.h)
	ADD_DEFINITIONS(-DDJBFFT)
	SET(DJBFFT_LIBRARIES ${DJBFFT_LIBRARY})
	SET(DJBFFT_INCLUDE_PATH ${DJBFFT_INCLUDE_PATH})
ENDIF(ENABLE_DJBFFT)

IF(ENABLE_OPTPP)
    FIND_PATH(OPTPP_INCLUDE_PATH Opt.h /usr/include /usr/include/opt++
              /usr/local/include /usr/local/include/opt++ $ENV{HOME}/include
             $ENV{HOME}/include/opt++)
    FIND_PATH(NEWMAT_INCLUDE_PATH newmat.h /usr/include /usr/include/opt++
              /usr/include/newmat /usr/local/include/newmat
              /usr/local/include /usr/local/include/opt++ $ENV{HOME}/include
             $ENV{HOME}/include/opt++)
    FIND_LIBRARY(OPTPP_LIBRARY NAMES opt-linux PATHS /usr/lib /usr/local/lib)
    FIND_LIBRARY(NEWMAT_LIBRARY NAMES newmat-linux PATHS /usr/lib /usr/local/lib)

    ADD_DEFINITIONS(-DOPTPP)
    INCLUDE_DIRECTORIES(${OPTPP_INCLUDE_PATH} ${NEWMAT_INCLUDE_PATH}) 
ENDIF(ENABLE_OPTPP)

CHECK_REQUIRED_LIB(GSL gsl gsl/gsl_linalg.h)
CHECK_REQUIRED_LIB(GSL_CBLAS gslcblas gsl/gsl_linalg.h)
IF (ENABLE_OPENGL)
	CHECK_REQUIRED_LIB(GL GL GL/gl.h)
	CHECK_REQUIRED_LIB(GLU GLU GL/glu.h)
	ADD_DEFINITIONS(-DEMAN2_USING_OPENGL)
ENDIF (ENABLE_OPENGL)

IF (ENABLE_FTGL)
	CHECK_REQUIRED_LIB2(FTGL ftgl FTGL/FTGL.h ftgl FTGL/ftgl.h)
	ADD_DEFINITIONS(-DEMAN2_USING_FTGL)
	IF(EXISTS ${FTGL_INCLUDE_PATH}/FTGL/FTGL.h AND COMMAND IF)
		ADD_DEFINITIONS(-DOLD_FTGL)
	ENDIF(EXISTS ${FTGL_INCLUDE_PATH}/FTGL/FTGL.h AND COMMAND IF)
	ADD_SUBDIRECTORY(fonts)
	CHECK_REQUIRED_LIB(FREETYPE freetype freetype/config/ftheader.h)
	IF (ENABLE_STATIC_FTGL)
		IF(WIN32)
			CHECK_REQUIRED_LIB(FREETYPE freetype ft2build.h)
		ENDIF(WIN32)
	ENDIF (ENABLE_STATIC_FTGL)
ENDIF (ENABLE_FTGL)


IF(ENABLE_TIFF)
    CHECK_OPTIONAL_LIB(TIFF tiff tiffio.h)
    CHECK_LIB_ONLY(JPEG jpeg)
ENDIF(ENABLE_TIFF)

IF(ENABLE_JPEG)
    CHECK_OPTIONAL_LIB(JPEG jpeg jpeglib.h)
ENDIF(ENABLE_JPEG)

IF(ENABLE_HDF5)
    IF (WIN32)
		CHECK_OPTIONAL_LIB(HDF5 hdf5dll hdf5.h)
		CHECK_LIB_ONLY(SZLIB szlibdll)
		CHECK_LIB_ONLY(ZLIB zlib1d)
		ADD_DEFINITIONS(-D_HDF5USEDLL_)
    ELSE (WIN32)
		CHECK_OPTIONAL_LIB(HDF5 hdf5 hdf5.h)
		CHECK_LIB_ONLY(ZLIB z)
    ENDIF (WIN32)
ENDIF(ENABLE_HDF5)

IF(ENABLE_PNG)
    IF (WIN32)
        CHECK_OPTIONAL_LIB(PNG libpng13d png.h)
        CHECK_LIB_ONLY(ZLIB zlib1d)
    ELSE (WIN32)
        CHECK_OPTIONAL_LIB(PNG png png.h)
    ENDIF (WIN32)    
ENDIF(ENABLE_PNG)

#FIND_PATH(NUMPY_INCLUDE_PATH numpy/arrayobject.h
#    $ENV{PYTHON_ROOT}/lib/python2.7/site-packages/numpy/core/include
#    $ENV{PYTHON_ROOT}/lib/python2.6/site-packages/numpy/core/include
#	$ENV{PYTHON_ROOT}/lib/python2.5/site-packages/numpy/core/include
#	$ENV{PYTHON_ROOT}/lib/python2.4/site-packages/numpy/core/include
#	/usr/local/lib/python2.7/site-packages/numpy/core/include
#	/usr/local/lib/python2.6/site-packages/numpy/core/include
#	/usr/lib64/python2.7/site-packages/numpy/core/include
#	/usr/lib64/python2.6/site-packages/numpy/core/include
#	/usr/lib64/python2.5/site-packages/numpy/core/include
#	/usr/lib64/python2.4/site-packages/numpy/core/include
#	/usr/lib/python2.7/site-packages/numpy/core/include
#	/usr/lib/python2.6/site-packages/numpy/core/include
#	/usr/lib/python2.5/site-packages/numpy/core/include
#    /Library/Python/2.5/site-packages/numpy/core/include
#    /usr/lib/python2.4/site-packages/numpy/core/include
#    /System/Library/Frameworks/Python.framework/Versions/2.7/Extras/lib/python/numpy/core/include
#    /System/Library/Frameworks/Python.framework/Versions/2.6/Extras/lib/python/numpy/core/include
#    /usr/include)
#INCLUDE_DIRECTORIES(${NUMPY_INCLUDE_PATH}) 

#IF(WIN32)
#	CHECK_PYTHON($ENV{PYTHON_VERSION} 27 26 25 24 23 22)
#ELSE(WIN32)
#	CHECK_PYTHON($ENV{PYTHON_VERSION} 2.7 2.6 2.5 2.4 2.3 2.2)
#ENDIF(WIN32)

#IF(PYTHON_INCLUDE_PATH)
    # Check the most generic name first (used by cctbx).
    # CHECK_REQUIRED_LIB(BOOST boost_python boost/python.hpp)
#ELSE(PYTHON_INCLUDE_PATH)
#    MESSAGE("ERROR: Python.h not found")
#ENDIF(PYTHON_INCLUDE_PATH)

IF(ENABLE_HDFIOCACHE)
    IF(APPLE)
        CHECK_OPTIONAL_LIB(BOOST_THREAD boost_thread boost/thread.hpp)
        CHECK_OPTIONAL_LIB(BOOST_FILESYSTEM boost_filesystem boost_system boost/filesystem.hpp)
        CHECK_OPTIONAL_LIB(BOOST_SYSTEM boost_system boost/filesystem.hpp)
    ELSE(APPLE)
        CHECK_OPTIONAL_LIB(BOOST_THREAD boost_thread-mt boost/thread.hpp)
        CHECK_OPTIONAL_LIB(BOOST_FILESYSTEM boost_filesystem-mt boost/filesystem.hpp)
    ENDIF(APPLE)
ENDIF(ENABLE_HDFIOCACHE)

MARK_AS_ADVANCED(FORCE CMAKE_BACKWARDS_COMPATIBILITY)

INCLUDE_DIRECTORIES(. 
	    ${EMAN_SOURCE_DIR} 
	    ${EMAN_SOURCE_DIR}/libEM
	    ${GL_INCLUDE_PATH}
	    $ENV{GSL_INC}
	    ${GSL_INCLUDE_PATH}
	    ${GSL_INCLUDE_PATH}/gsl
	    ${FFTW_INCLUDE_PATH}
	    ${DJBFFT_INCLUDE_PATH}
	    ${ACML_INCLUDE_PATH}
	    ${BOOST_INCLUDE_PATH}
	    ${PYTHON_INCLUDE_PATH}
	    ${FREETYPE_INCLUDE_PATH}
		/opt/local/include
            )
#            /usr/include/freetype2
#            /usr/local/include/freetype2

IF(ENABLE_RT)
    ADD_SUBDIRECTORY(rt)
ENDIF(ENABLE_RT)

IF (ENABLE_SPARX_CUDA)
	INCLUDE(${CMAKE_SOURCE_DIR}/libEM/cuda/FindCUDA.cmake)
	CUDA_INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/libEM)
	CUDA_INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/libEM/sparx/cuda)
	ADD_DEFINITIONS(-DSPARX_USING_CUDA)
	INCLUDE_DIRECTORIES(${CUDA_TOOLKIT_INCLUDE})
	ADD_SUBDIRECTORY(libEM/sparx/cuda)
ENDIF (ENABLE_SPARX_CUDA)

IF (ENABLE_EMAN_CUDA)
	INCLUDE(${CMAKE_SOURCE_DIR}/kernel/mrc/source/2dx_single_particle_lib/eman2/libEM/cuda/FindCUDA.cmake)
	CUDA_INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/libEM)
	ADD_DEFINITIONS(-DEMAN2_USING_CUDA)
	INCLUDE_DIRECTORIES(${CUDA_TOOLKIT_INCLUDE})
	ADD_SUBDIRECTORY(libEM/cuda)
ENDIF (ENABLE_EMAN_CUDA)

IF(ENABLE_TAO)
    ADD_SUBDIRECTORY(tao)
ENDIF(ENABLE_TAO)

ADD_SUBDIRECTORY(libEM)
#ADD_SUBDIRECTORY(libpyEM)
#ADD_SUBDIRECTORY(pyemtbx)
#ADD_SUBDIRECTORY(sparx)
#ADD_SUBDIRECTORY(utils)
#ADD_SUBDIRECTORY(mpi_eman)
#ADD_SUBDIRECTORY(examples)
#ADD_SUBDIRECTORY(programs)
#ADD_SUBDIRECTORY(doc)
#ADD_SUBDIRECTORY(images)

ADD_CUSTOM_COMMAND(
	TARGET unittest
	POST_BUILD
	COMMAND python 
	ARGS ${EMAN_INSTALL_PREFIX}/test/rt/rt.py
)

#set the list of unit test to run
#
SET (TESTS 
	test_vec 
	test_utils 
	test_typeconverter 
	test_transform 
	test_misc 
	test_imageio
	test_emdata
	test_processor
	test_cmp
	test_aligner
	test_averager
	)

#add the custom commands for each unit test program
#
FOREACH (TEST ${TESTS})

	ADD_CUSTOM_COMMAND(
        OUTPUT ${TEST}.PRINT_SPACE
        COMMAND ${CMAKE_COMMAND} ARGS -E echo ""
	)
	
	#this is the trick to make the target run every time, DoUnitTest suppose is a file never been built
	#
	ADD_CUSTOM_COMMAND(
        OUTPUT ${TEST}.DoUnitTest
		COMMAND ${CMAKE_COMMAND} ARGS -E time ${EMAN_INSTALL_PREFIX}/test/rt/${TEST}.py
		DEPENDS	${TEST}.PRINT_SPACE
	)
	
	#build a list of all the results
	SET( TEST_RESULTS ${TEST_RESULTS}
		${TEST}.DoUnitTest
	)

ENDFOREACH (TEST)

ADD_CUSTOM_TARGET( test
	DEPENDS	${TEST_RESULTS}
)
