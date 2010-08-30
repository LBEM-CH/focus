############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
cmake_minimum_required(VERSION 2.6)
################################################################################
# Application Name
project(ctf)
set(EXE_CTFFIND "2dx_ctffind3")
set(EXE_CTFTILT "2dx_ctftilt")
enable_language(Fortran)


# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

################################################################################
# Fortran Compiler -------------------------------------------------------------
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message(STATUS "adding ${Fortran_COMPILER_NAME} specific flags")

if(Fortran_COMPILER_NAME STREQUAL "gfortran")
	#gfortran
	#set(CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c")
	set(CMAKE_Fortran_FLAGS_RELEASE -O1 -ffixed-line-length-none -fno-second-underscore -fdollar-ok -ffast-math -w -dead_strip -static-libgcc -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -w) 
	#set(CMAKE_Fortran_FLAGS_RELEASE "-D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -w")
	set(CMAKE_Fortran_FLAGS_DEBUG "-fno-f2c -O0 -g")
elseif(Fortran_COMPILER_NAME STREQUAL "ifort")
	#ifort
	set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ipo -axN -fpp -parallel -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -w -132 -cxxlib")
	set(CMAKE_Fortran_FLAGS_DEBUG "--f77rtl -O0 -g")
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

# the linker will not be run
#set(CMAKE_Fortran_FLAGS_RELEASE  ${CMAKE_Fortran_FLAGS_RELEASE} "-c")
#set(CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS_DEBUG} "-c")

################################################################################
# C Compiler -------------------------------------------------------------------
#add_definitions(-DPROTOTYPE -Dalliant -Dpowerpc -w -ffast-math -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -c)
set(CMAKE_C_FLAGS_RELEASE "-DPROTOTYPE -Dalliant -Dpowerpc -w -ffast-math -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -c")
set(CMAKE_C_FLAGS_DEBUG   "-DPROTOTYPE -Dalliant -Dpowerpc -w -ffast-math -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -c")

# Header files -----------------------------------------------------------------
include_directories(${2DX_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
set(FORTRAN_SRC 
	2dx_ctffind3  
	2dx_ctftilt   
	mrc_ccp4
	)

set(FIND_OBJS 
	mrc_ccp4.o
	library.o
	)

set(TILT_OBJS
	mrc_ccp4.o
	library.o
	)

###############################################################################
set(CMAKE_DEBUG_POSTFIX -dbg)


 
#------------------------------------------------------------------------------
set(EXECUTABLES  ${EXE_CTFFIND} ${EXE_CTFTILT})

#Custom Fortran command
add_custom_command(OUTPUT mrc_ccp4.o COMMAND ${Fortran_COMPILER_NAME} ${CMAKE_Fortran_FLAGS_RELEASE} -c ${CMAKE_CURRENT_SOURCE_DIR}/mrc_ccp4.f)
#add_custom_command(TARGET EXE_CTFFIND PRE_LINK COMMAND ${Fortran_COMPILER_NAME} ARGS ${CMAKE_Fortran_FLAGS_RELEASE} -c ${CMAKE_CURRENT_SOURCE_DIR}/mrc_ccp4.f)
add_custom_command(OUTPUT 2dx_ctftilt.o  COMMAND ${Fortran_COMPILER_NAME} ${CMAKE_Fortran_FLAGS_RELEASE} -c 2dx_ctftilt.f)
#add_custom_command(TARGET EXE_CTFTILT PRE_LINK COMMAND ${Fortran_COMPILER_NAME} ARGS ${CMAKE_Fortran_FLAGS_RELEASE} -c 2dx_ctftilt.f)
add_custom_command(OUTPUT 2dx_ctffind3.o COMMAND ${Fortran_COMPILER_NAME}  ${CMAKE_Fortran_FLAGS_RELEASE} -c 2dx_ctffind3.f)
#add_custom_command(TARGET EXE_CTFFIND PRE_LINK COMMAND ${Fortran_COMPILER_NAME} ARGS ${CMAKE_Fortran_FLAGS_RELEASE} -c 2dx_ctffindi3.f)
	
add_custom_command(OUTPUT library.o COMMAND ${CMAKE_C_COMPILER} -DPROTOTYPE -Dalliant -Dpowerpc -w -ffast-math -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -c library.c )

#foreach(i ${FORTRAN_SRC})
#	add_executable(${i} "${i}.f")
#endforeach(i ${FORTRAN_SRC})

#strip the -c option from the Fortran compile command
#string(REPLACE " -c" 
#         "" CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_RELEASE})
#string(REPLACE " -c" 
#         "" CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS_DEBUG})
#message(STATUS "The striped fortran flags:" ${CMAKE_Fortran_FLAGS_RELEASE})
#add_executable(library library.c)


  
add_executable( ${EXE_CTFFIND} 2dx_ctffind3.o mrc_ccp4.o library )
add_executable( ${EXE_CTFTILT} 2dx_ctftilt.o mrc_ccp4.o library)

set_target_properties(${EXE_CTFFIND} ${EXE_CTFTILT} PROPERTIES LINKER_LANGUAGE Fortran)
#
 
#
# last thing we have to do is to tell CMake what libraries our executable
# needs,
#
# luckily FIND_PACKAGE prepared 2DX_LIBRARIES variable for us:
#
# target_link_libraries( ${EXETITLE} ${QT_LIBRARIES} ${2DX_LIBRARIES} ${LIBS})
 

# Installation
  install(TARGETS ${EXECUTABLES} RUNTIME  DESTINATION "bin")

