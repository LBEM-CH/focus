#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @location 2dx/apps/resources
#------------------------------------------------------------------------------

#-- CMAKE Configurations ------------------------------------------------------
#
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.11)
#------------------------------------------------------------------------------

get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/resources)

file(GLOB IMAGES images/*.png images/*.icns)
install(FILES ${IMAGES} DESTINATION ${INSTALL_DIR}/images/)

#-- SUBDIRS -------------------------------------------------------------------
add_subdirectory(config)
#------------------------------------------------------------------------------
