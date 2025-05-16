############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 3.10)
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/lib/${DIRECTORY})
endif(INSTALL_SRC_DIR)

set (OLD_LIBS
	rebuild_genlib_osx
	rebuild_ifftlib_osx
	rebuild_imlib2k_osx
	rebuild_libs_osx
	rebuild_misclib_osx
	rebuild_plot2klib_osx)
	
install(FILES ${OLD_LIBS} DESTINATION ${INSTALL_SRC_DIR})
