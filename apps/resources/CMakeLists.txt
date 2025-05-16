#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @location 2dx/apps/resources
#------------------------------------------------------------------------------

#-- CMAKE Configurations ------------------------------------------------------
#
CMAKE_MINIMUM_REQUIRED(VERSION 4.0)
#------------------------------------------------------------------------------

#-- Install directories -------------------------------------------------------
#
install(DIRECTORY config DESTINATION resources)
install(DIRECTORY images DESTINATION resources)
install(FILES about.htm DESTINATION resources)
#------------------------------------------------------------------------------
