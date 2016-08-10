############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
set(INSTALL_DIR kernel)

# Libraries --------------------------------------------------------------------

set(USE_FFTWF TRUE)
find_package(FFTW)
if(FFTWF_FOUND)
	list(APPEND PER_LIBRARIES  ${FFTWF_LIB})
	include_directories(${FFTW_INCLUDE_PATH})
elseif(FFTWF_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTWF_FOUND)
if(LOCAL_LIB_DIR)
	list(APPEND PER_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a )
endif(LOCAL_LIB_DIR)

add_subdirectory(toolkit)
add_subdirectory(executables)
add_subdirectory(mrc)
add_custom_target(kernel)


