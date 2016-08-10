#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @location 2dx/scripts
#------------------------------------------------------------------------------

#-- CMAKE Configurations ------------------------------------------------------
#
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.11)
#------------------------------------------------------------------------------

#-- INSTALL -------------------------------------------------------------------
set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/scripts)
set(EXECUTE_ALL OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
FILE(COPY "image" DESTINATION ${INSTALL_DIR} PATTERN * PERMISSIONS ${EXECUTE_ALL})
FILE(COPY "merge" DESTINATION ${INSTALL_DIR} PATTERN * PERMISSIONS ${EXECUTE_ALL})
FILE(COPY "project" DESTINATION ${INSTALL_DIR} PATTERN * PERMISSIONS ${EXECUTE_ALL})
FILE(COPY "proc" DESTINATION ${INSTALL_DIR} PATTERN * PERMISSIONS ${EXECUTE_ALL})
#------------------------------------------------------------------------------
