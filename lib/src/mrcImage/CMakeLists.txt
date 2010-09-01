############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
################################################################################
#

# A 2dx library
set(LIBTITLE mrcImage)
SET(LIBVERSION 1)
project(${LIBTITLE})

#------------------------------------------------------------------------------
# Configure Config file and include it.
# The dependencies are defined in the Config.cmake file
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake
  ${CMAKE_BINARY_DIR}/cmake/${LIBTITLE}Config.cmake
  @ONLY)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake
  ${CMAKE_BINARY_DIR}/cmake/${LIBTITLE}ConfigVersion.cmake
  @ONLY)

include(${CMAKE_BINARY_DIR}/cmake/${LIBTITLE}Config.cmake)

install(
  FILES       
    ${CMAKE_BINARY_DIR}/cmake/${LIBTITLE}Config.cmake
    ${CMAKE_BINARY_DIR}/cmake/${LIBTITLE}ConfigVersion.cmake
  DESTINATION ${CMAKE_INSTALL_PREFIX}/share/${LIBTITLE}-${LIBVERSION}/)

# Header files -----------------------------------------------------------------
if(NOT DEFINED 2DX_INCLUDE_DIR)
	set(2DX_INCLUDE_DIR ../../../include)
	message(STATUS "2DX_INCLUDE_DIR is not set. Setting it to: ${2DX_INCLUDE_DIR}" )
endif(NOT DEFINED 2DX_INCLUDE_DIR)
include_directories(${2DX_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
file(GLOB SRC *.cpp)

# Moc files --------------------------------------------------------------------
# these are all the headers containing Q_OBJECT
set(MOC_HDRS
	${2DX_INCLUDE_DIR}/mrcImage.h
	${2DX_INCLUDE_DIR}/largeMRC.h
	${2DX_INCLUDE_DIR}/mrcImageFast.h 
	)

if(QT4_FOUND)
	# generate moc files
	#QT4_WRAP_CPP(MOC_SRCS ${MOC_HDRS} OPTIONS "-I${2DX_INCLUDE_DIR} -p${CMAKE_CURRENT_SOURCE_DIR}")
	QT4_WRAP_CPP(MOC_SRCS ${MOC_HDRS}) 
endif(QT4_FOUND)

set(CMAKE_DEBUG_POSTFIX -dbg)

if(SRC)
add_library(${LIBTITLE} ${SRC} ${MOC_SRCS})

set_target_properties(${LIBTITLE} PROPERTIES
  VERSION ${LIBVERSION}
  SOVERSION ${LIBVERSION})

# Includes and libraries
#target_link_libraries(${LIBTITLE}
#  ${2DX_LIBRARIES})

# Installation
install(TARGETS ${LIBTITLE} DESTINATION lib)
endif(SRC)

