#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @location 2dx/apps/resources
#------------------------------------------------------------------------------

#-- CMAKE Configurations ------------------------------------------------------
#
cmake_minimum_required(VERSION 3.10)
#------------------------------------------------------------------------------

#-- Install directories -------------------------------------------------------
#
install(DIRECTORY config DESTINATION resources)
install(DIRECTORY images DESTINATION resources)
install(FILES about.htm DESTINATION resources)
#------------------------------------------------------------------------------
