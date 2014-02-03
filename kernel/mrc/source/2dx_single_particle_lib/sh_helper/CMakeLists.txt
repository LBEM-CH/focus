if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/${DIRECTORY})
endif(INSTALL_SRC_DIR)

set(INSTALL_BIN_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin/2dx_singleparticle)

# sh files
file(GLOB SH *.sh)
install(FILES ${SH} DESTINATION ${INSTALL_BIN_DIR})









