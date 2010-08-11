############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.8)
install(DIRECTORY tools DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins USE_SOURCE_PERMISSIONS REGEX [.]svn EXCLUDE)
install(DIRECTORY translators DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins USE_SOURCE_PERMISSIONS REGEX [.]svn EXCLUDE)
