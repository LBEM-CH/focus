#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @author Marcel Arheit
# @location 2dx/apps/src/2dx_merge/
#------------------------------------------------------------------------------

#--- CMAKE Configurations ------------------------------------------------------
cmake_minimum_required(VERSION 2.8.11)
#-------------------------------------------------------------------------------

#--- Configuration -------------------------------------------------------------
set(EXETITLE 2dx_merge)           # Application name
#-------------------------------------------------------------------------------

#--- BASE DIRECTORIES ----------------------------------------------------------
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}) # Header Files
get_propagate_deps(2dx_app) # Get the header files from the lib
SET(SRC main.cpp) # Source Files
#-------------------------------------------------------------------------------


#--- Create Executable ---------------------------------------------------------
add_executable(${EXETITLE} ${SRC})
target_link_libraries(${EXETITLE}  2dx_app)
#-------------------------------------------------------------------------------

#--- INSTALL -------------------------------------------------------------------
install(TARGETS ${EXETITLE} DESTINATION ${EXETITLE})
#-------------------------------------------------------------------------------