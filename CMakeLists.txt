############################################################################
# A 2dx CMakeLists.txt                                                     #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#

find_program(HOSTNAME_CMD NAMES hostname)
exec_program(${HOSTNAME_CMD} ARGS OUTPUT_VARIABLE HOSTNAME)

#cmake_policy(SET CMP0012 NEW)

if ("$ENV{USE_CUDA}" STREQUAL "TRUE")
	message("Try to use the GPU")
else()
	message(No GPU-support)
endif()

#if( ${HOSTNAME} STREQUAL "bs-powerlinux-ethz-ch" )
#	if ("$ENV{USE_CUDA}" STREQUAL "TRUE")
#		option(INTEL_CC "Use the Intel compiler." OFF)
#	else()
#		option(INTEL_CC "Use the Intel compiler." ON)
#		message("icc on")
#	endif()
#else()
#	option(INTEL_CC "Use the Intel compiler." OFF)
#endif()

if (INTEL_CC)
  find_program(CMAKE_C_COMPILER NAMES icc)
  find_program(CMAKE_CXX_COMPILER NAMES icpc)
  find_program(CMAKE_AR NAMES xiar)
  find_program(CMAKE_LINKER NAMES xild)

  if (CMAKE_C_COMPILER MATCHES CMAKE_C_COMPILER-NOTFOUND OR
      CMAKE_CXX_COMPILER MATCHES CMAKE_CXX_COMPILER-NOTFOUND OR
      CMAKE_AR MATCHES CMAKE_AR-NOTFOUND OR
      CMAKE_LINKER MATCHES CMAKE_LINKER-NOTFOUND)
    message(FATAL_ERROR "Cannot find Intel compiler.  You may need to run `. /opt/intel/bin/compilervars.sh intel64'")
  endif ()
endif (INTEL_CC) 

project(2dx)
cmake_minimum_required(VERSION 2.6)


if ("$ENV{USE_CUDA}" STREQUAL "TRUE")
	FIND_PACKAGE(CUDA)
endif()


if(CUDA_FOUND)
	OPTION(ENABLE_CUDA "enable 2dx CUDA usage" ON)
	message("USING CUDA")
	add_definitions( -DUSE_CUDA )
	include_directories(/usr/local/cuda-5.5/include)
else()
	message("NOOOOOT USING CUDA")
	OPTION(ENABLE_CUDA "enable 2dx CUDA usage" OFF)
endif()


set(CPACK_PACKAGE_VENDOR "2dx.org")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "2dx -Allows interactive processing of single EM-Micrographs of 2D membrane crystals.")

set(CPACK_PACKAGE_NAME 2dx)

set (2dx_VERSION_MAJOR 3)
set (2dx_VERSION_MINOR 5)
set (2dx_VERSION_PATCH 0)

set(CPACK_PACKAGE_VERSION_MAJOR ${2dx_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${2dx_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${2dx_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH})

SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

#include(FindSubversion)
#FIND_PACKAGE(Subversion)

#EXECUTE_PROCESS(COMMAND svn log -l10 ${CMAKE_SOURCE_DIR}
#	RESULT_VARIABLE check_wc)

#exec_program(./test.sh ${CMAKE_SOURCE_DIR})

#EXECUTE_PROCESS(COMMAND svn info ${CMAKE_SOURCE_DIR}
#	RESULT_VARIABLE check_wc)
#message(STATUS "SVN dummy return: ${check_wc}")

#set(2dx_SVN_REVISION_NUMBER unknown)

#IF(NOT ${check_wc} EQUAL 0)
#	MESSAGE(STATUS "No svn revision found")
#ELSE(NOT ${check_wc} EQUAL 0)
#	IF(SUBVERSION_FOUND)
#		Subversion_WC_INFO(${CMAKE_SOURCE_DIR} myapp)
#		set(2dx_SVN_REVISION_NUMBER ${myapp_WC_REVISION})
#	ENDIF(SUBVERSION_FOUND)
#ENDIF(NOT ${check_wc} EQUAL 0)

#message(STATUS "SVN revision number: ${2dx_SVN_REVISION_NUMBER}")

execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

#message(${GIT_COMMIT_HASH})


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
add_subdirectory(kernel)
add_subdirectory(lib)
add_subdirectory(2dx_image)
add_subdirectory(2dx_merge)
add_subdirectory(2dx_logbrowser)
add_subdirectory(2dx_automator)
add_subdirectory(plugins)
add_subdirectory(bin)

install(FILES LICENSE INSTALL.SOURCE DESTINATION ${CMAKE_BINARY_DIR} )

configure_file("changes.htm.in" "changes.htm")
configure_file("kernel/config/2dx_image/about.htm.in" "about.htm")
configure_file("build_scripts/make_pkg_on_jenkins_old.sh.in" "make_pkg_on_jenkins_old.sh")
configure_file("build_scripts/make_pkg_on_jenkins.sh.in" "make_pkg_on_jenkins.sh")
configure_file("build_scripts/add_version_to_src.sh.in" "add_version_to_src.sh")

file(GLOB DES installation/*.desktop)
install(FILES ${DES} DESTINATION Resource)

file(GLOB DES ${CMAKE_BINARY_DIR}/*.htm)
install(FILES ${DES} DESTINATION Resource/..)

file(GLOB DES installation/mac_osx_installer/Resources/background.pdf)
install(FILES ${DES} DESTINATION Resource)
install(FILES ${DES} DESTINATION resource)

file(GLOB DES installation/mac_osx_installer/Resources/License.rtf)
install(FILES ${DES} DESTINATION Resource)
install(FILES ${DES} DESTINATION resource)

file(GLOB DES installation/mac_osx_installer/Resources/Welcome.html)
install(FILES ${DES} DESTINATION Resource)
install(FILES ${DES} DESTINATION resource)

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
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/installation/mac_osx_installer/Resources/License.rtf")

#set(CPACK_RESOURCE_FILE_README "/Applications/CMake 2.8-1.app/Contents/share/cmake-2.8/Templates/CPack.GenericDescription.txt")

set(CPACK_RESOURCE_FILE_WELCOME "${CMAKE_CURRENT_SOURCE_DIR}/installation/mac_osx_installer/Resources/WelcomeNightly.html")

set(CPACK_OSX_PACKAGE_VERSION "10.6")
set(CPACK_PACKAGE_CONTACT "2dx.org")
set(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${CMAKE_CURRENT_SOURCE_DIR}/cmake/debian/postinst;${CMAKE_CURRENT_SOURCE_DIR}/cmake/debian/preinst;${CMAKE_CURRENT_SOURCE_DIR}/cmake/debian/postrm")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "C-CINA Uni Basel <2dx.jenkins@gmail.com>")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "tcsh (>=6.17.00)")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>=2.14.00)")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "fftw3")


#set(CPACK_RPM_PACKAGE_REQUIRES "tcsh >= 6.17.0, fftw-libs >= 3.3")

set(CPACK_RPM_POST_INSTALL_SCRIPT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/cmake/redhat/postinst.sh")
set(CPACK_RPM_PRE_INSTALL_SCRIPT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/cmake/redhat/preinst.sh")

set(CPACK_RPM_POST_UNINSTALL_SCRIPT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/cmake/redhat/postrm.sh")


include(CPack)

