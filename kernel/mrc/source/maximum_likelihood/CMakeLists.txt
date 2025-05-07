############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 3.10)

# Header files -----------------------------------------------------------------
if(NOT KERNEL_INCLUDE_DIR)
	set(KERNEL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
endif(NOT KERNEL_INCLUDE_DIR)
include_directories(${KERNEL_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
#set(ML_FILES 2dx_ML.c main_ML.c get_stack.c 2dx_lowpass.c ctf.c whitening.c mask.c trans.c quadintp.c rotate.c cross_corr.c get_resolution.c apply_envelop.c maximum_likelihood.c)
set(ML_SOURCES 2dx_ML.c)
#set(3D_STACK_SOURCES  2dx_ML_stack.c write_stack_merge.c fgetline.c cgetline.c 2dx_lowpass.c ctf.c whitening.c mask.c trans.c quadintp.c rotate.c cross_corr.c get_resolution.c apply_envelop.c maximum_likelihood.c) 
#set(3D_MERGE_SOURCES  2dx_ML_merge.c  fgetline.c cgetline.c 2dx_lowpass.c ctf.c whitening.c mask.c trans.c quadintp.c rotate.c cross_corr.c get_resolution.c apply_envelop.c maximum_likelihood.c) 
file(GLOB FORTRAN_SOURCE *.for *.f)
file(GLOB C_SOURCE *.c *.h)
file(GLOB CPP_SOURCE *.cc *.cpp)
set(SOURCES ${FORTRAN_SOURCE} ${C_SOURCE} ${CPP_SOURCE}) 
set(ML_TARGETS 2dx_ML)
#set(ML_TARGETS 2dx_ML 2dx_ML_stack 2dx_ML_merge)
# Libraries --------------------------------------------------------------------i
if(FFTWF_FOUND)
	list(APPEND ML_LIBRARIES  ${FFTWF_LIB})
	include_directories(${FFTW_INCLUDE_PATH})
elseif(FFTWF_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTWF_FOUND)
if(LOCAL_LIB_DIR)
	list(APPEND ML_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a )
endif(LOCAL_LIB_DIR)
# Executables ------------------------------------------------------------------

set(CMAKE_C_FLAGS_RELEASE "-ffast-math  -static-libgcc")
set(CMAKE_C_FLAGS_DEBUG "-g -ffast-math  -static-libgcc")
set(LOCAL_EXECUTABLES "")
foreach(i ${ML_TARGETS})
	set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/${i}.c PROPERTIES LANGUAGE CXX )
	add_executable(${i}  ${CMAKE_CURRENT_SOURCE_DIR}/${i}.c)
	target_link_libraries(${i} ${ML_LIBRARIES})
	list(APPEND LOCAL_EXECUTABLES ${i})
endforeach(i ${ML_TARGETS})

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
