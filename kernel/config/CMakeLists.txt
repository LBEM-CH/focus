############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_DIR)
	set(INSTALL_DIR ${INSTALL_DIR}/${DIRECTORY})
else(INSTALL_DIR)
	set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/kernel/${DIRECTORY})
endif(INSTALL_DIR)

# configure 2dx.cfg according to the os it is built on
if(APPLE)
	configure_file(2dx-mac.cfg ${CMAKE_CURRENT_SOURCE_DIR}/2dx.cfg COPYONLY)	
else(APPLE)
	configure_file(2dx-linux.cfg ${CMAKE_CURRENT_SOURCE_DIR}/2dx.cfg COPYONLY)	
endif(APPLE)

set(CONF 
        VARIABLE.txt 
        status-window
        2dx.cfg	
        2dx.cshrc 
        2dx_image-algo-1.jpg 
        2dx_image-algo-2.jpg 
        2dx_master.cfg)

install(FILES ${CONF} DESTINATION ${INSTALL_DIR})
install(SCRIPT install_symlink.cmake)

set(CONF_2DX_IMAGE
        2dx_image/navigator_help.htm
        2dx_image/2dx_status.html
)

install(FILES ${CONF_2DX_IMAGE} DESTINATION ${INSTALL_DIR}/2dx_image)
file(GLOB CONF_2DX_MERGE 2dx_merge/*.inf)
install(FILES ${CONF_2DX_MERGE} DESTINATION ${INSTALL_DIR}/2dx_merge)
file(GLOB CONF_RESOURCE resource/*.png resource/*.psd  resource/*.icns)
install(FILES ${CONF_RESOURCE} DESTINATION ${INSTALL_DIR}/resource)
