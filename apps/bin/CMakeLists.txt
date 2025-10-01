############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 3.10)
set(FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
if(APPLE)
	#	file(GLOB SCRIPTS *.mac)
        install(FILES focus.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME focus)
        install(FILES fViewer.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME fViewer)
        install(FILES fMrcConverter.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME fMrcConverter)
	install(FILES fLogBrowser.mac DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME fLogBrowser)
else(APPLE)
	#file(GLOB SCRIPTS *.linux)
        install(FILES focus.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME focus)
        install(FILES fViewer.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME fViewer)
        install(FILES fMrcConverter.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME fMrcConverter)
	install(FILES fLogBrowser.linux DESTINATION bin PERMISSIONS ${FILE_PERMISSIONS} RENAME fLogBrowser)
endif(APPLE)


