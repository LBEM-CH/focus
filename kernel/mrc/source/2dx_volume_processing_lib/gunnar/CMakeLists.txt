############################################################################
# CMakeLists.txt for compiling gunnar's stuff                              #
############################################################################
# Author:                                                                  #
# Nikhil Biyani  03/2015                                                   #
############################################################################
#
cmake_minimum_required(VERSION 2.6)

#==============================#
# Set Environment              #
#==============================#
SET(BEADGEN_EXE "beadgen_comp")
SET(MAPHIST_EXE "maphist")
SET(MAPSIZE_EXE "map_size")

#=============================#
# Create Executables          # 
#=============================#
add_executable(${BEADGEN_EXE} ./${BEADGEN_EXE}.c)
add_executable(${MAPHIST_EXE} ./${MAPHIST_EXE}.c)
add_executable(${MAPSIZE_EXE} ./${MAPSIZE_EXE}.c)

#=============================#
# Link Libraries              # 
#=============================#
target_link_libraries(${BEADGEN_EXE} m)
target_link_libraries(${MAPHIST_EXE} m)
target_link_libraries(${MAPSIZE_EXE} m)

#==============================#
# Installation                 #
#==============================#
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/source/${DIRECTORY})
endif(INSTALL_SRC_DIR)

if(NOT INSTALL_BIN_DIR)
	set(INSTALL_BIN_DIR kernel/mrc/bin)
endif(NOT INSTALL_BIN_DIR)

install(TARGETS ${BEADGEN_EXE} DESTINATION ${INSTALL_BIN_DIR})
install(TARGETS ${MAPHIST_EXE} DESTINATION ${INSTALL_BIN_DIR})
install(TARGETS ${MAPSIZE_EXE} DESTINATION ${INSTALL_BIN_DIR})
