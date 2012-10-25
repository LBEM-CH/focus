############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)

# Header files -----------------------------------------------------------------
if(NOT KERNEL_INCLUDE_DIR)
	set(KERNEL_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
endif(NOT KERNEL_INCLUDE_DIR)
include_directories(${KERNEL_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
file(GLOB C_SOURCE *.c *.h)
set(SOURCES ${C_SOURCE}) 
set(tif2mrc_TARGETS 2dx_tif2mrc)
# Libraries --------------------------------------------------------------------i
# if(LOCAL_LIB_DIR)
# 	list(APPEND tif2mrc_LIBRARIES ${LOCAL_LIB_DIR}/lib2dx.a )
# endif(LOCAL_LIB_DIR)
# Executables ------------------------------------------------------------------
set(LOCAL_EXECUTABLES "")
foreach(i ${tif2mrc_TARGETS})
	set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/${i}.c PROPERTIES LANGUAGE CXX )
	add_executable(${i}  ${CMAKE_CURRENT_SOURCE_DIR}/${i}.c)
	# target_link_libraries(${i} ${tif2mrc_LIBRARIES})
	list(APPEND LOCAL_EXECUTABLES ${i})
endforeach(i ${tif2mrc_TARGETS})

# Installation  ---------------------------------------------------------------
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/source/${DIRECTORY})
endif(INSTALL_SRC_DIR)

if(NOT INSTALL_BIN_DIR)
	set(INSTALL_BIN_DIR kernel/mrc/bin)
endif(NOT INSTALL_BIN_DIR)

install(FILES ${SOURCES} DESTINATION ${INSTALL_SRC_DIR})
install(TARGETS ${LOCAL_EXECUTABLES} DESTINATION ${INSTALL_BIN_DIR})
