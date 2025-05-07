cmake_minimum_required(VERSION 3.10)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

cmake_policy(SET CMP0012 NEW)

#include_directories("/opt/intel/mkl/include/fftw/")

find_package(CppUnit)
if(CPPUNIT_FOUND)
	include_directories(${CPPUNIT_INCLUDE_DIR})
elseif(CPPUNIT_FOUND)
	message(FATAL_ERROR "CPPUNIT not found!")
endif(CPPUNIT_FOUND)

find_program(HOSTNAME_CMD NAMES hostname)
exec_program(${HOSTNAME_CMD} ARGS OUTPUT_VARIABLE HOSTNAME)

if(APPLE)
set(Boost_USE_STATIC_LIBS true)
endif(APPLE)

find_package(Boost COMPONENTS serialization system filesystem program_options REQUIRED)

if(Boost_FOUND)
	if( (${HOSTNAME} STREQUAL "bs-cina01.ethz.ch") OR (${HOSTNAME} STREQUAL "bs-cina02.ethz.ch") OR (${HOSTNAME} STREQUAL "bs-isvr02.ethz.ch") )
		include_directories(/usr/local/cina/boost_1.51/include)
	elseif( ${HOSTNAME} STREQUAL "maia.cluster" )
		message("Hi from MAIA")
		include_directories(/home/phil2/scherers/libs/boost_1_52/include)
	else()
	 	include_directories(${Boost_INCLUDE_DIR})
	endif()
elseif(Boost_FOUND)
	message(FATAL_ERROR "Boost not found!")
endif(Boost_FOUND)

if($ENV{BRUTUS_2DX})
	include_directories($ENV{FFTW3_ROOT_DIR})
	include_directories($ENV{EIGEN_INCLUDE_DIR})
else()
	set(USE_FFTWF TRUE)
	find_package(FFTW)
	if(FFTWF_FOUND)
		include_directories(${FFTW_INCLUDE_PATH})
	elseif(FFTWF_FOUND)
		message(FATAL_ERROR "FFTW not found!")
	endif(FFTWF_FOUND)

	find_package(Eigen3 REQUIRED)
	if(EIGEN3_FOUND)
		include_directories(${EIGEN3_INCLUDE_DIR})
	elseif(EIGEN3_FOUND)
		message(FATAL_ERROR "Eigen3 not found!")
	endif(EIGEN3_FOUND)
endif()

# Header files -----------------------------------------------------------------
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
if(APPLE)
	include_directories(/opt/local/include)
endif(APPLE)

if (ENABLE_CUDA)
	include_directories(/usr/local/cuda-5.5/include)
	find_package(CUDA)
	set(CUDA_NVCC_FLAGS ${CUDA_NVCC_FLAGS};-gencode arch=compute_30,code=sm_35)
	set(CUDA_NVCC_FLAGS ${CUDA_NVCC_FLAGS};--optimize=3)
endif (ENABLE_CUDA)


#EMAN2 specific include dir
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/eman2/libEM)

add_subdirectory(eman2)

################################################################################
# C++ Compiler -----------------------------------------------------------------
if(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(CMAKE_CXX_FLAGS "-openmp -g -g3 -std=c++11 -xhost -no-prec-div -opt-prefetch -unroll-aggressive -m64 -ansi-alias")
	#set(CMAKE_CXX_FLAGS "-openmp -std=c++11 -O3 -xhost")
	#set(CMAKE_CXX_FLAGS "-openmp -O3 -xhost -vec-report2")
else(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(CMAKE_CXX_FLAGS "-fopenmp -O3 -funroll-loops -pedantic -W -Waddress -Warray-bounds -Wmissing-field-initializers -Wchar-subscripts -Wenum-compare -Wcomment -Wformat -Wmain -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-attributes -Wno-long-long -Wextra")# -Werror -std=c++11") 
endif(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")


#set(CMAKE_CXX_FLAGS "-fopenmp -g -ggdb -pedantic -W -Wall -Wno-long-long -Wno-unused-variable") 
set(2DX_SP SingleParticle2dx)
set(2DX_SP_CUDA SingleParticleCUDA2dx)

file(GLOB_RECURSE EMAN eman2/*.cpp)
file(GLOB_RECURSE 2DX_SP_CPP *.cpp)
file(GLOB_RECURSE 2DX_CUDA ${CMAKE_CURRENT_SOURCE_DIR}/Methods/*.cu)
file(GLOB RUNNERS_CPP ${CMAKE_CURRENT_SOURCE_DIR}/Runners/*.cpp)
file(GLOB RUNNERS_OLD_CPP ${CMAKE_CURRENT_SOURCE_DIR}/Runners_old/*.cpp)
file(GLOB PARALLEL_RUNNERS_CPP ${CMAKE_CURRENT_SOURCE_DIR}/ParallelRunners/*.cpp)
file(GLOB TESTS_CPP ${CMAKE_CURRENT_SOURCE_DIR}/Test/*.cpp)

list(REMOVE_ITEM 2DX_SP_CPP ${RUNNERS_CPP})
list(REMOVE_ITEM 2DX_SP_CPP ${RUNNERS_OLD_CPP})
list(REMOVE_ITEM 2DX_SP_CPP ${PARALLEL_RUNNERS_CPP})
list(REMOVE_ITEM 2DX_SP_CPP ${TESTS_CPP})
list(REMOVE_ITEM 2DX_SP_CPP ${EMAN})


if (ENABLE_CUDA)
	MESSAGE("USING CUDA")
	cuda_add_library(${2DX_SP_CUDA} STATIC ${2DX_CUDA})
else (ENABLE_CUDA)
	list(REMOVE_ITEM 2DX_SP_CPP ${CMAKE_CURRENT_SOURCE_DIR}/Methods/ProjectionMethods/Cuda*.cpp)
	list(REMOVE_ITEM 2DX_SP_CPP ${CMAKE_CURRENT_SOURCE_DIR}/Methods/Utilities/cu*.cpp)
endif(ENABLE_CUDA)



add_library(${2DX_SP} STATIC ${2DX_SP_CPP})

add_subdirectory(Runners)

if( (${HOSTNAME} STREQUAL "sebastian-ThinkPad-T410s") OR (${HOSTNAME} STREQUAL "cina-buildtst02") OR (${HOSTNAME} STREQUAL "maia.cluster") OR ((${HOSTNAME} STREQUAL "bs-powerlinux-ethz-ch") AND NOT (CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc") ))
	add_subdirectory(ParallelRunners)
endif()

if($ENV{BRUTUS_2DX})
	add_subdirectory(ParallelRunners)
endif()

if($ENV{CSCS_2DX})
	add_subdirectory(ParallelRunners)
endif()

add_subdirectory(python_helper)
add_subdirectory(sparx_python)
add_subdirectory(sh_helper)


if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/${DIRECTORY})
endif(INSTALL_SRC_DIR)

install(FILES ${2DX_SP_CPP} DESTINATION ${INSTALL_SRC_DIR})

file(GLOB_RECURSE 2DX_SP_HPP *.hpp)
list(REMOVE_ITEM 2DX_SP_CPP ${CMAKE_CURRENT_SOURCE_DIR}/Test/*.hpp)
install(FILES ${2DX_SP_HPP} DESTINATION ${INSTALL_SRC_DIR})

