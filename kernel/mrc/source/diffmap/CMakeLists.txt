############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2013 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
################################################################################


# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
else (NOT CMAKE_BUILD_TYPE)
	message(STATUS "THE BUILD TYPE IS SET TO: ${CMAKE_BUILD_TYPE}")
endif (NOT CMAKE_BUILD_TYPE)

# Python files -----------------------------------------------------------------
file(GLOB PYTHON_FILES *.py)

###############################################################################
# Installation  ---------------------------------------------------------------
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(INSTALL_BIN_DIR)
	set(INSTALL_DIFFMAP_DIR ${INSTALL_BIN_DIR}/${DIRECTORY})
else(INSTALL_BIN_DIR)
	set(INSTALL_DIFFMAP_DIR kernel/mrc/bin/${DIRECTORY})
endif(INSTALL_BIN_DIR)

install(FILES ${PYTHON_FILES} DESTINATION ${INSTALL_DIFFMAP_DIR})
###############################################################################
