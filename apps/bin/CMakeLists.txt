############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
set(FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
if(APPLE)
	#	file(GLOB SCRIPTS *.mac)
        install(FILES 2dx_gui.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_gui)
        install(FILES 2dx_viewer.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_viewer)
	install(FILES 2dx_image.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_image)
        install(FILES 2dx_mrc_converter.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_mrc_converter)
	install(FILES 2dx_merge.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_merge)
	install(FILES 2dx_logbrowser.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_logbrowser)
else(APPLE)
	#file(GLOB SCRIPTS *.linux)
        install(FILES 2dx_gui.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_gui)
        install(FILES 2dx_viewer.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_viewer)
	install(FILES 2dx_image.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_image)
        install(FILES 2dx_mrc_converter.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_mrc_converter)
	install(FILES 2dx_merge.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_merge)
	install(FILES 2dx_logbrowser.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME 2dx_logbrowser)
endif(APPLE)


