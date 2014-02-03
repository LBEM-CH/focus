set(CMAKE_EXECUTABLE_SUFFIX ".exe")

set(CMAKE_CXX_FLAGS "-fopenmp -O3 -funroll-loops -pedantic -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-attributes -Wno-long-long -Wextra") 
#set(CMAKE_CXX_FLAGS "-fopenmp") 
set(2DX_SP SingleParticle2dx)

SET(CMAKE_CXX_COMPILER mpic++.openmpi)

if ( ${HOSTNAME} STREQUAL "maia.cluster" )
	SET(CMAKE_CXX_COMPILER mpic++)
endif()


if ( ${HOSTNAME} STREQUAL "bs-powerlinux-ethz-ch" )
	SET(CMAKE_CXX_COMPILER mpic++)
endif()

SET(CMAKE_CXX_COMPILER mpic++)

file(GLOB CPP_SOURCE_SP *.cpp)

foreach(i ${CPP_SOURCE_SP})
	get_filename_component(EXECUTABLE ${i}  NAME_WE)
	add_executable(${EXECUTABLE} ${i})
	
	target_link_libraries(${EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/../libSingleParticle2dx.a)
	target_link_libraries(${EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/../eman2/libEM/libEM2.a)
	
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
		target_link_libraries(${EXECUTABLE} /home/phil2/scherers/libs/boost_1_52/lib/libboost_mpi.so)
	elseif( ${HOSTNAME} STREQUAL "bs-powerlinux-ethz-ch" )
		find_package(Boost COMPONENTS serialization system filesystem program_options REQUIRED)
		target_link_libraries(${EXECUTABLE} ${Boost_LIBRARIES})
		
		if (ENABLE_CUDA)
			target_link_libraries(${EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/../libSingleParticleCUDA2dx.a)
			target_link_libraries(${EXECUTABLE} /usr/local/cuda/lib64/libcudart.so.5.5)
			target_link_libraries(${EXECUTABLE} /usr/local/cuda/lib64/libcufft.so)
		endif(ENABLE_CUDA)
		
		target_link_libraries(${EXECUTABLE} /usr/lib64/mpich/lib/libboost_mpi-mt.so)
		#target_link_libraries(${EXECUTABLE} /usr/lib64/mpich2/lib/libmpl.so)
		#target_link_libraries(${EXECUTABLE} /usr/lib64/mpich2/lib/libopa.so)
		
	else()
		find_package(Boost COMPONENTS serialization system filesystem program_options mpi REQUIRED)
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
