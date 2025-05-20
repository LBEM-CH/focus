############################################################################
# A 2dx CMakeLists.txt                                                     #
############################################################################
# The author was:                                                          #
# 2012 Sebastian Scherer                                                   #
############################################################################
#
cmake_minimum_required(VERSION 3.10)

################################################################################
# Fortran Compiler -------------------------------------------------------------
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp")
set(CMAKE_Fortran_FLAGS_DEBUG "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp")
if(Fortran_COMPILER_NAME STREQUAL "gfortran")
        #gfortra
        set(CMAKE_Fortran_FLAGS_RELEASE "-g -fbounds-check -fcheck=all -fbacktrace -O0 -Wall -Wsurprising -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -fopenmp -w")
        set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -fopenmp -g")
        if(APPLE)
                set(FORTRAN_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE})
                set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE ")
        endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "f95")
        #gfortra
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -fopenmp")
        set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -g -fopenmp")
        if(APPLE)
                set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}")
                set(FORTRAN_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE} )
                set(CMAKE_Fortran_FLAGS_DEBUG  "${CMAKE_Fortran_FLAGS_DEBUG} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE ") 
        endif(APPLE)
elseif(Fortran_COMPILER_NAME STREQUAL "ifort")
        #ifort
        #LIST(APPEND CMAKE_Fortran_FLAGS_RELEASE "-O3 -axN -w -132 -cxxlib")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -w -132 -cxxlib")
        #set(CMAKE_Fortran_FLAGS_DEBUG "-O3 -axN -w -132 -cxxlib")
elseif (Fortran_COMPILER_NAME STREQUAL "g77")
        # g77
        set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
        set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME STREQUAL "gfortran")
        message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
        message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
        message ("No optimized Fortran compiler flags are known, we just try -O2...")
        set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
        set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME STREQUAL "gfortran")

message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
message ("compiler options in ctfcor: " ${CMAKE_Fortran_FLAGS_RELEASE})


# Header files -----------------------------------------------------------------
if(NOT KERNEL_INCLUDE_DIR)
	set(KERNEL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
endif(NOT KERNEL_INCLUDE_DIR)
include_directories(${KERNEL_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
file(GLOB FORTRAN_SOURCE *.for *.f *.for)
file(GLOB C_SOURCE *.c *.h)
file(GLOB CPP_SOURCE *.cc *.cpp)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE}) 
set(PER_F_TARGETS 2dx_ctfcor_stripes 2dx_ctfcor_tiles)


# Libraries --------------------------------------------------------------------i
if(FFTWF_FOUND)
	list(APPEND PER_LIBRARIES  ${FFTWF_LIB})
	include_directories(${FFTW_INCLUDE_PATH})
elseif(FFTWF_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTWF_FOUND)
if(LOCAL_LIB_DIR)
       list(APPEND LOCAL_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a ${LOCAL_LIB_DIR}/libim2k.a ${LOCAL_LIB_DIR}/libmisc.a ${LOCAL_LIB_DIR}/libgen.a ${LOCAL_LIB_DIR}/libplot2k.a ${LOCAL_LIB_DIR}/libfft.a ${LOCAL_LIB_DIR}/libconv.a)
endif(LOCAL_LIB_DIR)

list(APPEND LIBRARIES ${2DX_LIBRARIES})



# Executables ------------------------------------------------------------------
set(LOCAL_EXECUTABLES "")

foreach(i ${PER_F_TARGETS})
# 	set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/${i}.f PROPERTIES LANGUAGE fortran )
	add_executable(${i}  ${CMAKE_CURRENT_SOURCE_DIR}/${i}.f ${CMAKE_CURRENT_SOURCE_DIR}/iof.f ${CMAKE_CURRENT_SOURCE_DIR}/ioc.c )
	target_link_libraries(${i} ${PER_LIBRARIES} ${LOCAL_LIBRARIES} ${LIBRARIES})
	list(APPEND LOCAL_EXECUTABLES ${i})
endforeach(i ${PER_F_TARGETS})

# Installation  ---------------------------------------------------------------
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/source/${DIRECTORY})
endif(INSTALL_SRC_DIR)

if(NOT INSTALL_BIN_DIR)
	set(INSTALL_BIN_DIR kernel/mrc/bin)
endif(NOT INSTALL_BIN_DIR)

install(FILES ${SOURCES} DESTINATION ${INSTALL_SRC_DIR})
install(TARGETS ${LOCAL_EXECUTABLES} DESTINATION ${INSTALL_BIN_DIR})
