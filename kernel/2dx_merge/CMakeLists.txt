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

file(GLOB SCRIPTS_2D scripts-merge2D/*.script )
file(GLOB SCRIPTS_3D scripts-merge3D/*.script )
file(GLOB CSTM_SCRIPTS scripts-custom/*.script)
file(GLOB SP_SCRIPTS scripts-singleparticle/*.script)
file(GLOB PROJ_SCRIPTS scripts-project/*.script)


set(EXECUTE_ALL OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
install(FILES ${SCRIPTS_2D}  DESTINATION ${INSTALL_DIR}/scripts-merge2D PERMISSIONS ${EXECUTE_ALL})
install(FILES ${SCRIPTS_3D}  DESTINATION ${INSTALL_DIR}/scripts-merge3D PERMISSIONS ${EXECUTE_ALL})
install(FILES ${CSTM_SCRIPTS}  DESTINATION ${INSTALL_DIR}/scripts-custom PERMISSIONS ${EXECUTE_ALL})
install(FILES ${SP_SCRIPTS}  DESTINATION ${INSTALL_DIR}/scripts-singleparticle PERMISSIONS ${EXECUTE_ALL})
install(FILES ${PROJ_SCRIPTS}  DESTINATION ${INSTALL_DIR}/scripts-project PERMISSIONS ${EXECUTE_ALL})