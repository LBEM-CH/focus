############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 4.0)
################################################################################
# Application Name
enable_language(Fortran)


# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
      set (CMAKE_BUILD_TYPE RELEASE CACHE STRING "Choose the type of build, options are: None Debug Release." FORCE)
endif (NOT CMAKE_BUILD_TYPE)

################################################################################
# Fortran Compiler -------------------------------------------------------------
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
message(STATUS "adding ${Fortran_COMPILER_NAME} specific flags")

if(Fortran_COMPILER_NAME STREQUAL "gfortran")
      set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp ")
      set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -g -fopenmp ")
      if(APPLE)
            set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -dead_strip -static-libgcc")
            set(FORTRAN_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE}  -dead_strip -static-libgcc)
            set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -static-libgcc")
      endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "f95")
      set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w")
      set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -g")
      if(APPLE)
            set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ")
            set(FORTRAN_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE} )
            set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE ")
      endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "ifort")
      set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -w -132 -cxxlib")
elseif (Fortran_COMPILER_NAME STREQUAL "g77")
      set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
      set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME STREQUAL "gfortran")
      message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
      message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
      message ("No optimized Fortran compiler flags are known, we just try -O2...")
      set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
      set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME STREQUAL "gfortran")

################################################################################
# C Compiler -------------------------------------------------------------------
#add_definitions(-DPROTOTYPE -Dalliant -Dpowerpc -w -ffast-math -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -c)
set(CMAKE_C_FLAGS_RELEASE "-DUSE_THREADS_2DX -DPROTOTYPE -Dalliant -w -ffast-math")
set(CMAKE_C_FLAGS_DEBUG  "-ffast-math")

################################################################################
# C++ Compiler -----------------------------------------------------------------
if(CMAKE_CXX_COMPILER STREQUAL "icpc")
      set(CMAKE_CXX_FLAGS "-DUSE_THREADS_2DX -ffast-math -axN -w -132 -cxxlib")
else(CMAKE_CXX_COMPILER STREQUAL "icpc")
      set(CMAKE_CXX_FLAGS "-DUSE_THREADS_2DX -DPROTOTYPE -Dalliant -w -ffast-math") 
      if(APPLE)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
      endif(APPLE)
endif(CMAKE_CXX_COMPILER STREQUAL "icpc")


# Header files -----------------------------------------------------------------
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)
if(APPLE)
      include_directories(/opt/local/include)
endif(APPLE)
      


# Source files -----------------------------------------------------------------
set(GENFOR unix.for diskio.for ccplib.for subs.for parser.for symlib.for)
set(GENC library.c)
set(MISC misc)
set(MISCFOR mtzlib.for lcflib.for miscsubs.for)
set(IM2K im2k)
set(IMLIBFOR imsubs2000.for imsubs2.for)
set(IMLIBFORABS ${CMAKE_CURRENT_SOURCE_DIR}/imsubs2000.for ${CMAKE_CURRENT_SOURCE_DIR}/imsubs2.for)
set(PLOT2KC plot2k.c)
set(FFTCPP fftlib.cpp)
set(CONVCPP 2dx_conv.cpp)
set(FSPLIT_SRC fsplit.c)
set(LIB2DX 2dx_func.for mrcImage.cpp)
list(APPEND SOURCES ${GENFOR} ${GENC} ${MISCFOR} ${IMLIBFOR} ${PLOT2KC} ${FFTCPP} ${CONVCPP} ${FSPLIT} ${LIB2DX}) 
set(SOURCES_FILE ${CMAKE_CURRENT_BINARY_DIR}/sources.cmake)
###############################################################################


################################################################################
# Libraries --------------------------------------------------------------------i
#set(LOCAL_LIB_DIR ../lib)
list(APPEND LOCAL_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a ${LOCAL_LIB_DIR}/libim2k.a ${LOCAL_LIB_DIR}/libmisc.a ${LOCAL_LIB_DIR}/libgen.a ${LOCAL_LIB_DIR}/libplot2k.a ${LOCAL_LIB_DIR}/libfft.a  ${LOCAL_LIB_DIR}/libconv.a)

if(APPLE)
   find_library(CPP_STD_LIB c++)
   if (CPP_STD_LIB)
       message(STATUS "Found libc++: ${CPP_STD_LIB}")
   else()
       message(WARNING "Could not find libc++ explicitly, using -lc++")
       set(CPP_STD_LIB c++)
   endif()
endif()

################################################################################
# Executables ------------------------------------------------------------------

# create Fortran object files
#set(CMAKE_FORTRAN_COMPILE_OBJECT ON)
set(C_COMPILE_OBJECT ON)
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -I${CMAKE_CURRENT_SOURCE_DIR}")

# fsplit -----------------------------------------------------------------------
set(FSPLIT_EXE fsplit)
set(FSPLIT_COMMAND ${FSPLIT_EXE})
add_executable(${FSPLIT_EXE} ${FSPLIT_SRC})

function(did_lib_sources_change RETURN_VALUE LIB SOURCES)
      set(${RETURN_VALUE} 0 PARENT_SCOPE) 
      foreach(FORFILE ${SOURCES})
            if(${CMAKE_CURRENT_SOURCE_DIR}/${FORFILE} IS_NEWER_THAN ${CMAKE_CURRENT_BINARY_DIR}/${LIB})
                  set(${RETURN_VALUE} 1  PARENT_SCOPE)
                  message(STATUS "MRC/LIB: ${FORFILE} is newer than ${LIB}")
            endif()
      endforeach()
endfunction()



# libgen -----------------------------------------------------------------------
set(TARGET gen)
set(LIB "lib${TARGET}.a")
did_lib_sources_change(ISNEWER lib${TARGET}.a "${GENFOR};${GENC}")
if(${ISNEWER})
      message(STATUS "${LIB} is outdated.")
      file(WRITE ${SOURCES_FILE} "")
      foreach(GENFILE ${GENFOR})
            add_custom_command(OUTPUT ${GENFILE}  DEPENDS ${FSPLIT_EXE} COMMAND ${FSPLIT_COMMAND} ${CMAKE_CURRENT_SOURCE_DIR}/${GENFILE})
            list(APPEND GENF_ORIG ${GENFILE})
            file(APPEND ${SOURCES_FILE} "${CMAKE_CURRENT_SOURCE_DIR}/${FORFILE};")
      endforeach(GENFILE)
      
      add_custom_target(${TARGET} ALL
            DEPENDS ${GENF_ORIG} ${GENC}
            COMMAND ${CMAKE_COMMAND} -DFSPLIT_OUTFILE=gen_fslpit_files.cmake -DSOURCES_FILE=${SOURCES_FILE} -DLIB=lib${TARGET}.a -P ${CMAKE_CURRENT_SOURCE_DIR}/collectFiles.cmake
            COMMAND ${CMAKE_COMMAND} -DFSPLIT_INFILE=gen_fslpit_files.cmake -DFORTRAN_COMPILE_COMMAND=${Fortran_COMPILER_NAME}  -DFORTRAN_FLAGS=${CMAKE_Fortran_FLAGS_RELEASE} -P ${CMAKE_CURRENT_SOURCE_DIR}/doPostFortran.cmake
            COMMAND ${CMAKE_C_COMPILER} -DPROTOTYPE -Dalliant -w -ffast-math -c ${CMAKE_CURRENT_SOURCE_DIR}/${GENC}
            COMMAND ${CMAKE_COMMAND} -DLIB_NAME=${TARGET} -P ${CMAKE_CURRENT_SOURCE_DIR}/createLib.cmake
            WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            COMMENT "compiling ${LIB}"
            VERBATIM)
else()
    # Define the gen target anyway, so dependencies work
    add_custom_target(${TARGET})
endif()
# libmisc ----------------------------------------------------------------------
set(TARGET misc)
set(LIB "lib${TARGET}.a")
message(STATUS ${CMAKE_CURRENT_BINARY_DIR}/${LIB})
did_lib_sources_change(ISNEWER lib${TARGET}.a "${MISCFOR}")
if(${ISNEWER})    
      message(STATUS "${LIB} is outdated.")
      file(WRITE ${SOURCES_FILE} "")
      foreach(FORFILE ${MISCFOR})
            add_custom_command(OUTPUT ${FORFILE} DEPENDS ${FSPLIT_EXE} COMMAND ${FSPLIT_COMMAND} ${CMAKE_CURRENT_SOURCE_DIR}/${FORFILE})
            list(APPEND MISC_FSPLIT ${FORFILE})
            file(APPEND ${SOURCES_FILE} "${CMAKE_CURRENT_SOURCE_DIR}/${FORFILE};")
      endforeach(FORFILE)

      add_custom_target(${TARGET} ALL
            DEPENDS ${MISC_FSPLIT}
            COMMAND ${CMAKE_COMMAND} -DFSPLIT_OUTFILE=gen_fslpit_files.cmake -DSOURCES_FILE=${SOURCES_FILE} -DLIB=lib${TARGET}.a -P ${CMAKE_CURRENT_SOURCE_DIR}/collectFiles.cmake
            COMMAND ${CMAKE_COMMAND} -DFSPLIT_INFILE=gen_fslpit_files.cmake -DFORTRAN_COMPILE_COMMAND=${Fortran_COMPILER_NAME}  -DFORTRAN_FLAGS=${CMAKE_Fortran_FLAGS_RELEASE} -P ${CMAKE_CURRENT_SOURCE_DIR}/doPostFortran.cmake
            COMMAND ${CMAKE_COMMAND} -DLIB_NAME=${TARGET} -P ${CMAKE_CURRENT_SOURCE_DIR}/createLib.cmake
            WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            COMMENT "compiling ${LIB}"
            VERBATIM)
else()
    # Define the gen target anyway, so dependencies work
    add_custom_target(${TARGET})
endif()
# libim2k ----------------------------------------------------------------------
set(TARGET im2k)
set(LIB "lib${TARGET}.a")
did_lib_sources_change(ISNEWER lib${TARGET}.a "${IMLIBFOR}")
if(${ISNEWER})
      message(STATUS "${LIB} is outdated.")
      file(WRITE ${SOURCES_FILE} "")
      foreach(FORFILE ${IMLIBFOR})
            add_custom_command(OUTPUT ${FORFILE} DEPENDS ${FSPLIT_EXE} COMMAND ${FSPLIT_COMMAND} ${CMAKE_CURRENT_SOURCE_DIR}/${FORFILE})
            list(APPEND IM2K_FSPLIT ${FORFILE})
            file(APPEND ${SOURCES_FILE} "${CMAKE_CURRENT_SOURCE_DIR}/${FORFILE};")
      endforeach(FORFILE)
      add_custom_target(${TARGET} ALL
            DEPENDS ${IM2K_FSPLIT}
            COMMAND ${CMAKE_COMMAND} -DFSPLIT_OUTFILE=gen_fslpit_files.cmake -DSOURCES_FILE=${SOURCES_FILE} -DLIB=lib${TARGET}.a -P ${CMAKE_CURRENT_SOURCE_DIR}/collectFiles.cmake
            COMMAND ${CMAKE_COMMAND} -DFSPLIT_INFILE=gen_fslpit_files.cmake -DFORTRAN_COMPILE_COMMAND=${Fortran_COMPILER_NAME}  -DFORTRAN_FLAGS=${CMAKE_Fortran_FLAGS_RELEASE} -P ${CMAKE_CURRENT_SOURCE_DIR}/doPostFortran.cmake
            COMMAND ${CMAKE_COMMAND} -DLIB_NAME=${TARGET} -P ${CMAKE_CURRENT_SOURCE_DIR}/createLib.cmake
            WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            COMMENT "compiling ${LIB}"
            VERBATIM)
endif()
# libplot2k --------------------------------------------------------------------
set(PLOT2K plot2k)
add_library(${PLOT2K} STATIC ${PLOT2KC})
add_dependencies(${PLOT2K} ${IM2k} misc gen)

# lib2dx -----------------------------------------------------------------------
FIND_PACKAGE(Qt5 COMPONENTS Widgets REQUIRED)

IF(Qt5_FOUND)
    MESSAGE(STATUS "KERNEL/LIB: Found QT5 Widgets (Version: ${Qt5Widgets_VERSION_STRING})")
    INCLUDE_DIRECTORIES(${Qt5Widgets_INCLUDE_DIRS})
    ADD_DEFINITIONS(${Qt5Widgets_DEFINITIONS})
    set(2DX_LIBRARIES Qt5::Widgets ${2DX_LIBRARIES})
ELSE()
    MESSAGE(STATUS "KERNEL/LIB: QT5 not Found!")
ENDIF()
 
set(2DX 2dx) 
add_library(${2DX} STATIC 2dx_func.for mrcImage.cpp)
qt5_use_modules(${2DX} Widgets)
target_link_libraries(${2DX} ${2DX_LIBRARIES})
add_dependencies(${2DX} ${PLOT2K})

# libfft -----------------------------------------------------------------------
set(FFT fft) 
add_library(${FFT} STATIC ${FFTCPP})
add_dependencies(${FFT} ${2DX})

# libconvi ---------------------------------------------------------------------
set(CONV conv) 
add_library(${CONV} STATIC ${CONVCPP})
add_dependencies(${CONV} ${FFT})

###############################################################################
# Installation  ---------------------------------------------------------------
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(INSTALL_SRC_DIR)
      set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
      set(INSTALL_SRC_DIR kernel/mrc/${DIRECTORY})
endif(INSTALL_SRC_DIR)

# sources
install(FILES ${SOURCES} DESTINATION ${INSTALL_SRC_DIR})

add_subdirectory(old)

if(NOT INSTALL_BIN_DIR)
      set(INSTALL_BIN_DIR kernel/mrc/bin)
endif(NOT INSTALL_BIN_DIR)
# executables

install(TARGETS ${LIBRARIES} DESTINATION ${INSTALL_BIN_DIR})
###############################################################################
