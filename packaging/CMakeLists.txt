############################################################################
# @file CMakeLists.txt
# @author Nikhil Biyani
# @author Marcel Arheit
# @location 2dx/installation
##########################################################################
#-- CMAKE Configurations ------------------------------------------------------
#
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

#-- CONFIGURE FILES -----------------------------------------------------------
#
# These will copy/ configure the files and write to build directory which can
# be later used to install
configure_file("resources/Welcome.html.in" "${CMAKE_CURRENT_BINARY_DIR}/resources/Welcome.html")
file(COPY "resources/License.rtf" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/resources")
file(COPY "upload_to_server.sh" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    file(COPY "resources/background.jpg" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/resources")
    file(COPY "osx/Distribution.xml" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
    file(COPY "osx/change_dylibs.sh" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
    file(COPY "osx/osx_package_builder.sh" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
    file(COPY "osx/scripts/preinstall" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/scripts")
    file(COPY "osx/scripts/preupgrade" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/scripts")
    file(COPY "osx/scripts/postinstall" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/scripts")
    file(COPY "osx/scripts/postupgrade" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/scripts")
endif()
#------------------------------------------------------------------------------

#-- INSTALL FILES -------------------------------------------------------------
#
# These will copy the files directly to installation location
if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    file(GLOB DES resources/*.desktop)
    install(FILES ${DES} DESTINATION Resource)
endif()
#------------------------------------------------------------------------------

#-- PACKAGING -----------------------------------------------------------------
#
# (only for Debian and RPM for OSX we use native tools)
# General Settings
set(CPACK_GENERATOR "DEB;RPM")

set(CPACK_PACKAGE_NAME 2dx)
set(CPACK_PACKAGE_VENDOR "2dx.org")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "2dx -Allows interactive processing of single EM-Micrographs of 2D membrane crystals.")
set(CPACK_PACKAGE_CONTACT "2dx.org")
set(CPACK_PACKAGE_VERSION_MAJOR ${2dx_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${2dx_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${2dx_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH})

set(CPACK_PACKAGING_INSTALL_PREFIX "/opt/2dx")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_BINARY_DIR}/resources/License.rtf")
set(CPACK_RESOURCE_FILE_WELCOME "${CMAKE_CURRENT_BINARY_DIR}/resources/Welcome.html")

# DEBIAN SETTINGS
set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${CMAKE_CURRENT_SOURCE_DIR}/debian/postinst;${CMAKE_CURRENT_SOURCE_DIR}/debian/preinst;${CMAKE_CURRENT_SOURCE_DIR}/debian/postrm")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "C-CINA Uni Basel <2dx.jenkins@gmail.com>")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "tcsh (>=6.17.00)")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>=2.14.00)")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "fftw3")

# RPM SETTINGS
set(CPACK_RPM_POST_INSTALL_SCRIPT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/redhat/postinst.sh")
set(CPACK_RPM_PRE_INSTALL_SCRIPT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/redhat/preinst.sh")
set(CPACK_RPM_POST_UNINSTALL_SCRIPT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/redhat/postrm.sh")

include(CPack)
#-----------------------------------------------------------------------------
