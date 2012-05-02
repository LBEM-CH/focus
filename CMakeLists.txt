############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
project(2dx)
cmake_minimum_required(VERSION 2.6)

set(CPACK_PACKAGE_VENDOR "2dx.org")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "2dx -Allows interactive processing of single EM-Micrographs of 2D membrane crystals.")

set(CPACK_PACKAGE_NAME 2dx)

set (2dx_VERSION_MAJOR 3)
set (2dx_VERSION_MINOR 4)
set (2dx_VERSION_PATCH 2)

set(CPACK_PACKAGE_VERSION_MAJOR ${2dx_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${2dx_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${2dx_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH})

set(2DX_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR} CACHE PATH "The path of the 2dx root source directory.")
message(STATUS "The variable 2DX_ROOT_DIR is set to: ${2DX_ROOT_DIR}")
set(2DX_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include CACHE PATH "The path of the 2dx include directory.")
message(STATUS "The variable 2DX_INCLUDE_DIR is set to: ${2DX_INCLUDE_DIR}")
set(2DX_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE PATH "The path of the 2dx binary directory.")
add_subdirectory(include)
add_subdirectory(cmake)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})
include(GetPropagateDeps)
include(SetPropagateDeps)
# the libraries have to be built before by the build_all script 
add_subdirectory(lib)
add_subdirectory(kernel)
add_subdirectory(2dx_image)
add_subdirectory(2dx_merge)
add_subdirectory(2dx_logbrowser)
add_subdirectory(plugins)
add_subdirectory(bin)
install(FILES LICENSE INSTALL.SOURCE DESTINATION ${CMAKE_BINARY_DIR} )

configure_file("changes.htm.in" "changes.htm")
configure_file("kernel/config/2dx_image/about.htm.in" "about.htm")

configure_file("build_scripts/make_pkg_on_jenkins.sh.in" "make_pkg_on_jenkins.sh")

configure_file("build_scripts/add_version_to_src.sh.in" "add_version_to_src.sh")

file(GLOB DES installation/*.desktop)
install(FILES ${DES} DESTINATION Resource)

file(GLOB DES installation/mac_osx_installer/Resources/background.pdf)
install(FILES ${DES} DESTINATION Resource)

file(GLOB DES installation/mac_osx_installer/Resources/License.rtf)
install(FILES ${DES} DESTINATION Resource)

file(GLOB DES installation/mac_osx_installer/Resources/Welcome.html)
install(FILES ${DES} DESTINATION Resource)

file(GLOB DES installation/mac_osx_installer/Resources/postinstall)
install(FILES ${DES} DESTINATION Resource)

file(GLOB DES installation/mac_osx_installer/Resources/postupgrade)
install(FILES ${DES} DESTINATION Resource)

file(GLOB DES installation/mac_osx_installer/Resources/preinstall)
install(FILES ${DES} DESTINATION Resource)

file(GLOB DES installation/mac_osx_installer/Resources/preupgrade)
install(FILES ${DES} DESTINATION Resource)

#packaging
set(CPACK_GENERATOR "DEB;RPM")
# set(CPACK_PACKAGE_DEFAULT_LOCATION "/opt/2dx")
# set(CPACK_PACKAGE_INSTALL_DIRECTORY "2dx")
set(CPACK_PACKAGING_INSTALL_PREFIX "/opt/2dx")

# set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY 1)
#set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/installation/mac_osx_installer/Resources/License.rtf")

#set(CPACK_RESOURCE_FILE_README "/Applications/CMake 2.8-1.app/Contents/share/cmake-2.8/Templates/CPack.GenericDescription.txt")

#set(CPACK_RESOURCE_FILE_WELCOME "${CMAKE_CURRENT_SOURCE_DIR}/installation/mac_osx_installer/Resources/WelcomeNightly.html")

set(CPACK_OSX_PACKAGE_VERSION "10.6")
set(CPACK_PACKAGE_CONTACT "2dx.org")
set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${CMAKE_CURRENT_SOURCE_DIR}/cmake/debian/postinst;${CMAKE_CURRENT_SOURCE_DIR}/cmake/debian/preinst;${CMAKE_CURRENT_SOURCE_DIR}/cmake/debian/postrm")

set(CPACK_DEBIAN_PACKAGE_DEPENDS "tcsh (>=6.17.00)")

set(CPACK_RPM_POST_INSTALL_SCRIPT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/cmake/redhat/postinst.sh")


include(CPack)

