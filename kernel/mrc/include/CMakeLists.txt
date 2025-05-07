############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 3.5)

# Libraries --------------------------------------------------------------------i
if(FFTWF_FOUND)
	list(APPEND PER_LIBRARIES  ${FFTWF_LIB})
	include_directories(${FFTW_INCLUDE_PATH})
elseif(FFTWF_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTWF_FOUND)
if(LOCAL_LIB_DIR)
	list(APPEND PER_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a )
endif(LOCAL_LIB_DIR)

get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/${DIRECTORY})
endif(INSTALL_SRC_DIR)

file(GLOB HEADERS *.h)
install(FILES ${HEADERS} DESTINATION ${INSTALL_SRC_DIR})
