set(CMAKE_EXECUTABLE_SUFFIX ".exe")

#set(CMAKE_CXX_FLAGS "-fopenmp -O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra") 
#set(CMAKE_CXX_FLAGS "-fopenmp -g3 -pedantic -W -Wall -Wno-long-long -Wno-unused-variable") 

set(2DX_SP SingleParticle2dx)

################################################################################
# C++ Compiler -----------------------------------------------------------------
if(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(CMAKE_CXX_FLAGS "-openmp -g -g3 -std=c++17 -xhost -no-prec-div -opt-prefetch -unroll-aggressive -m64 -ansi-alias")
	#set(CMAKE_CXX_FLAGS "-openmp -O3 -std=c++17 -xhost")
	#set(CMAKE_CXX_FLAGS "-openmp -O3 -xhost -ipo -fast -xavx -no-prec-div -unroll")
	add_definitions( -DUSE_CILK )
else(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(CMAKE_CXX_FLAGS "-fopenmp -O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wno-attributes -Wvolatile-register-var -Wno-long-long -Wextra -Wno-unused-function")
endif(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")


file(GLOB CPP_SOURCE_SP *.cpp)

foreach(i ${CPP_SOURCE_SP})
	get_filename_component(EXECUTABLE ${i}  NAME_WE)
	add_executable(${EXECUTABLE} ${i})
	
	target_link_libraries(${EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/../libSingleParticle2dx.a)
	target_link_libraries(${EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/../eman2/libEM/libEM2.a)
	#target_link_libraries(${EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/../eman2/libEM/cuda/libEM2Cuda.a)
	
	if (ENABLE_CUDA)
		target_link_libraries(${EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/../libSingleParticleCUDA2dx.a)
		target_link_libraries(${EXECUTABLE} /usr/local/cuda/lib64/libcudart.so.5.5)
		target_link_libraries(${EXECUTABLE} /usr/local/cuda/lib64/libcufft.so)
	endif(ENABLE_CUDA)

	if($ENV{BRUTUS_2DX})
		target_link_libraries(${EXECUTABLE} $ENV{FFTW3F_ROOT_DIR}/lib64/libfftw3f.a)
	else()
		target_link_libraries(${EXECUTABLE} ${FFTWF_LIB})
	endif()
	
	if( (${HOSTNAME} STREQUAL "bs-cina01.ethz.ch") OR (${HOSTNAME} STREQUAL "bs-cina02.ethz.ch") OR (${HOSTNAME} STREQUAL "bs-isvr02.ethz.ch") )
		target_link_libraries(${EXECUTABLE} /usr/local/cina/boost_1.51/lib/libboost_system.so)
		target_link_libraries(${EXECUTABLE} /usr/local/cina/boost_1.51/lib/libboost_filesystem.so)
		target_link_libraries(${EXECUTABLE} /usr/local/cina/boost_1.51/lib/libboost_serialization.so)
		target_link_libraries(${EXECUTABLE} /usr/local/cina/boost_1.51/lib/libboost_program_options.so)
	elseif( ${HOSTNAME} STREQUAL "maia.cluster" )
		target_link_libraries(${EXECUTABLE} /home/phil2/scherers/libs/boost_1_52/lib/libboost_system.so)
		target_link_libraries(${EXECUTABLE} /home/phil2/scherers/libs/boost_1_52/lib/libboost_filesystem.so)
		target_link_libraries(${EXECUTABLE} /home/phil2/scherers/libs/boost_1_52/lib/libboost_serialization.so)
		target_link_libraries(${EXECUTABLE} /home/phil2/scherers/libs/boost_1_52/lib/libboost_program_options.so)
	else()
		target_link_libraries(${EXECUTABLE} ${Boost_LIBRARIES})
	endif()
	
	#linking for eman2 lib (libEM2.a)
	target_link_libraries(${EXECUTABLE} ${GSL_LIBRARY})
	target_link_libraries(${EXECUTABLE} ${GSL_CBLAS_LIBRARY})
	target_link_libraries(${EXECUTABLE} ${TIFF_LIBRARY})
	target_link_libraries(${EXECUTABLE} ${HDF5_LIBRARY})
	target_link_libraries(${EXECUTABLE} ${PNG_LIBRARY})
	target_link_libraries(${EXECUTABLE} ${JPEG_LIBRARY})
	
	list(APPEND EXECUTABLES_SP ${EXECUTABLE})
endforeach(i ${CPP_SOURCE_SP})


if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/${DIRECTORY})
endif(INSTALL_SRC_DIR)
set(INSTALL_BIN_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin/2dx_singleparticle)

# executables
install(TARGETS ${EXECUTABLES_SP} DESTINATION ${INSTALL_BIN_DIR})
# sources
file(GLOB SOURCES *.hpp *.cpp)
install(FILES ${SOURCES} DESTINATION ${INSTALL_SRC_DIR})
