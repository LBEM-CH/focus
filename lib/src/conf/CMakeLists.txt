############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
set(CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR}/cmake ${CMAKE_MODULE_PATH})
################################################################################
#

# A 2dx library
set(LIBTITLE conf)
SET(LIBVERSION 1)
project(${LIBTITLE})

#------------------------------------------------------------------------------
# Configure Config file and include it.
# The dependencies are defined in the Config.cmake file
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake
  ${CMAKE_BINARY_DIR}/share/${LIBTITLE}Config.cmake
  @ONLY)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Version.cmake
  ${CMAKE_BINARY_DIR}/share/${LIBTITLE}ConfigVersion.cmake
  @ONLY)

include(${CMAKE_BINARY_DIR}/share/${LIBTITLE}Config.cmake)

#install(
#  FILES       
#    ${CMAKE_CURRENT_SOURCE_DIR}/share/${LIBTITLE}-${LIBVERSION}/${LIBTITLE}Config.cmake
#    ${CMAKE_CURRENT_SOURCE_DIR}/share/${LIBTITLE}-${LIBVERSION}/${LIBTITLE}ConfigVersion.cmake
#  DESTINATION ${CMAKE_INSTALL_PREFIX}/share/${LIBTITLE}-${LIBVERSION}/)

# Header files -----------------------------------------------------------------
if(NOT DEFINED 2DX_INCLUDE_DIR)
	set(2DX_INCLUDE_DIR ../../../include CACHE INTERNAL "the 2dx include dir")
	message(STATUS "2DX_INCLUDE_DIR is not set. Setting it to: ${2DX_INCLUDE_DIR}" )
endif(NOT DEFINED 2DX_INCLUDE_DIR)
include_directories(${2DX_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_INSTALL_PREFIX}/include)

# Source files -----------------------------------------------------------------
file(GLOB SRC *.cpp)

# Moc files --------------------------------------------------------------------
# these are all the headers containing Q_OBJECT
set(MOC_HDRS
	${2DX_INCLUDE_DIR}/confData.h	
	${2DX_INCLUDE_DIR}/confDelegate.h
	${2DX_INCLUDE_DIR}/confEditor.h
	${2DX_INCLUDE_DIR}/confElement.h
	${2DX_INCLUDE_DIR}/confInput.h
	${2DX_INCLUDE_DIR}/confInterface.h
	${2DX_INCLUDE_DIR}/confManual.h
	${2DX_INCLUDE_DIR}/confModel.h
	${2DX_INCLUDE_DIR}/confSection.h
	${2DX_INCLUDE_DIR}/confSectionHeader.h
	${2DX_INCLUDE_DIR}/confValidator.h
)

cmake_minimum_required(VERSION 2.6)

set(CMAKE_DEBUG_POSTFIX -dbg)

if(QT4_FOUND)
	# generate moc files
	QT4_WRAP_CPP(MOC_SRCS ${MOC_HDRS}) 
endif(QT4_FOUND)


add_library(${LIBTITLE} STATIC ${SRC} ${MOC_SRCS})

set_target_properties(${LIBTITLE} PROPERTIES
  VERSION ${LIBVERSION}
  SOVERSION ${LIBVERSION})


set_propagate_deps(${LIBTITLE})
# Includes and libraries
#target_link_libraries(${LIBTITLE}
# ${2DX_LIBRARIES})

# Installation
#install(TARGETS ${LIBTITLE} DESTINATION lib)

