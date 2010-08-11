############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.8)
set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX})
install(DIRECTORY config DESTINATION ${INSTALL_DIR} USE_SOURCE_PERMISSIONS REGEX [.]svn EXCLUDE)
install(DIRECTORY proc DESTINATION ${INSTALL_DIR} USE_SOURCE_PERMISSIONS REGEX [.]svn EXCLUDE)
#copy the 2dx_image directory to the build directory
install(DIRECTORY 2dx_image DESTINATION ${INSTALL_DIR} USE_SOURCE_PERMISSIONS REGEX [.]svn EXCLUDE)
#copy the 2dx_merge directory to the build directory
install(DIRECTORY 2dx_merge DESTINATION ${INSTALL_DIR} USE_SOURCE_PERMISSIONS REGEX [.]svn EXCLUDE)
install(DIRECTORY mrc/bin DESTINATION ${INSTALL_DIR}/2dx_image USE_SOURCE_PERMISSIONS REGEX [.]svn EXCLUDE)
# could also create a symlink, but to keep it as platform independent we install it double
install(DIRECTORY mrc/bin DESTINATION ${INSTALL_DIR}/2dx_merge USE_SOURCE_PERMISSIONS REGEX [.]svn EXCLUDE)


