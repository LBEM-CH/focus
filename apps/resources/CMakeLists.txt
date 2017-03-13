#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @location 2dx/apps/resources
#------------------------------------------------------------------------------

#-- CMAKE Configurations ------------------------------------------------------
#
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.11)
#------------------------------------------------------------------------------

#-- Install directories -------------------------------------------------------
#
install(DIRECTORY config DESTINATION ${CMAKE_INSTALL_PREFIX}/resources)
install(DIRECTORY images DESTINATION ${CMAKE_INSTALL_PREFIX}/resources)
install(FILES about.htm DESTINATION ${CMAKE_INSTALL_PREFIX}/resources)
#------------------------------------------------------------------------------
