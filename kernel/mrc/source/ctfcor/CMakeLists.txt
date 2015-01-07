############################################################################
# A 2dx CMakeLists.txt                                                     #
############################################################################
# The author was:                                                          #
# 2012 Sebastian Scherer                                                   #
############################################################################
#
cmake_minimum_required(VERSION 2.6)

# Header files -----------------------------------------------------------------
if(NOT KERNEL_INCLUDE_DIR)
	set(KERNEL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
endif(NOT KERNEL_INCLUDE_DIR)
include_directories(${KERNEL_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
file(GLOB FORTRAN_SOURCE *.for *.f)
file(GLOB C_SOURCE *.c *.h)
file(GLOB CPP_SOURCE *.cc *.cpp)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE}) 
set(PER_F_TARGETS 2dx_ctfcor_fftw)


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
