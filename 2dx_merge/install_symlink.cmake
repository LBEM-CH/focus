############################################################################
# A 2dx CMake install script                                               #
############################################################################
# Creates the necessary symbolic links to the kernel                       #
# The author was:                                                          #
# 2011 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
if(CMAKE_HOST_UNIX)
	execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ../kernel/2dx_merge/scripts-merge2D ${CMAKE_INSTALL_PREFIX}/2dx_merge/scripts-merge2D)
        execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ../kernel/2dx_merge/scripts-merge2D ${CMAKE_INSTALL_PREFIX}/2dx_merge/scripts-merge3D)
	execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ../kernel/2dx_merge/scripts-custom ${CMAKE_INSTALL_PREFIX}/2dx_merge/scripts-custom)
	execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ../kernel/mrc/bin ${CMAKE_INSTALL_PREFIX}/2dx_merge/bin)
endif(CMAKE_HOST_UNIX)


