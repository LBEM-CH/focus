############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
################################################################################
# Application Name
set(EXETITLE 2dx_image)
project( ${EXETITLE} )

# set version
set( VERSION_MAJOR "3" )
set( VERSION_MINOR "2" )
set( VERSION_PATCH "0" )

cmake_minimum_required(VERSION 2.8)
#-- Need this for link line stuff?
if(COMMAND cmake_policy)
	   cmake_policy(SET CMP0011 NEW)
endif(COMMAND cmake_policy)

# OS specific project properties ----------------------------------------------
IF( APPLE )
	# for 64bit us the follwing line
	#SET( CMAKE_OSX_ARCHITECTURES ppc;i386;ppc64;x86_64) 
	#to be able to use the QT Carbon 32 bit
	#SET( CMAKE_OSX_ARCHITECTURES ppc;i386) 
ENDIF( APPLE )

# Libraries to use ------------------------------------------------------------
find_package(conf)
find_package(widgets)
find_package(mrcImage)
find_package(extentions)
find_package(Qt4 COMPONENTS QtCore QtNetwork QtWebKit QtScript REQUIRED)
if(QT4_FOUND)
	message(STATUS "Found QT4 at ${QT_LIBRARY_DIR}")
	include(${QT_USE_FILE})
        SET(2DX_LIBRARIES ${QT_LIBRARIES} ${2DX_LIBRARIES})
        #message(STATUS "2DX_LIBRARIES ${2DX_LIBRARIES}")
endif(QT4_FOUND)
find_package(OpenGL)
if(OPENGL_FOUND)
        message(STATUS "Found OpenGL at ${OPENGL_LIBRARIES}")
        INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR})
        SET(2DX_LIBRARIES  ${OPENGL_LIBRARIES} ${2DX_LIBRARIES})
endif(OPENGL_FOUND)

# Header files -----------------------------------------------------------------
if(NOT DEFINED 2DX_INCLUDE_DIR)
        set(2DX_INCLUDE_DIR ../include CACHE INTERNAL "the 2dx include dir")
        message(STATUS "2DX_INCLUDE_DIR is not set. Setting it to: ${2DX_INCLUDE_DIR}" )
endif(NOT DEFINED 2DX_INCLUDE_DIR)
include_directories(${2DX_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Source files -----------------------------------------------------------------
# takes all files ending with .cpp
file(GLOB SRC *.cpp)


# moc files --------------------------------------------------------------------
# these are all the headers containing Q_OBJECT
# TODO: glob these files with a cmake module
set(MOC_HDRS
	centralWindow.h
	controlActionsGroup.h
	mainWindow.h
	navigator.h
	progressStamps.h
	resultsFile.h
	resultsParser.h
	statusViewer.h
	warningBox.h
	)
###############################################################################
cmake_minimum_required(VERSION 2.6)
set(CMAKE_DEBUG_POSTFIX -dbg)


# generate moc files
QT4_WRAP_CPP(MOC_SRCS ${MOC_HDRS})



 

message(STATUS "Creating: ${EXETITLE}")
if(APPLE)
  # Define some settings for the Bundle
  set( MACOSX_BUNDLE_BUNDLE_NAME ${EXETITLE} )
  set( MACOSX_BUNDLE_GUI_IDENTIFIER "2dx image" )
  set( MACOSX_BUNDLE_ICON_FILE  icon_OSX.icns)
  set( MACOSX_BUNDLE_INFO_STRING ""${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}",www.2dx.org" )
  set( MACOSX_BUNDLE_SHORT_VERSION_STRING "${VERSION_MAJOR}.${VERSION_MINOR}" )
  set( MACOSX_BUNDLE_LONG_VERSION_STRING "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )
  set( MACOSX_BUNDLE_BUNDLE_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )
  
  # create a bundle with an icon too!
  add_executable( ${EXETITLE} MACOSX_BUNDLE  ${SRC} ${MOC_SRCS})

  #  # Allows for bundle re-creation just by running "make". Also installs bundle icon
  add_custom_command(TARGET ${EXETITLE} POST_BUILD
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/MacOS
    COMMAND cp ARGS ${CMAKE_CURRENT_SOURCE_DIR}/resource/${MACOSX_BUNDLE_ICON_FILE}
	${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources/${MACOSX_BUNDLE_ICON_FILE})
  #add_dependencies(${EXETITLE} osx_bundle_dirs )
  ## This tells cmake where to place files inside the bundle
  #set_source_files_properties( ${ProjectName_RESOURCES} ${ProjectName_TRANSLATIONS} PROPERTIES MACOSX_PACKAGE_LOCATION Resources )
else(APPLE)
  add_executable( ${EXETITLE} ${SRC} ${MOC_SRCS})
endif(APPLE)


# Libraries 
#
# last thing we have to do is to tell CMake what libraries our executable
# needs,
#
# luckily FIND_PACKAGE prepared 2DX_LIBRARIES variable for us:
#
#------------------------------------------------------------------------------

          target_link_libraries(${EXETITLE} ${2DX_LIBRARIES})
#  # Includes and libraries
#  # This rather compilcated approach is necessary to make gathering the libraries in 2DX_LIBRARIES possible
#  # Just linking it all in does not seem to work, for some unknown reason.
#    set(OPT_OR_DEBUG "general")
#    foreach(LIB ${2DX_LIBRARIES})       
#      if(LIB STREQUAL "debug") 
#        set(OPT_OR_DEBUG "debug")
#      elseif(LIB STREQUAL "optimized")
#        set(OPT_OR_DEBUG "optimized")
#      else(LIB STREQUAL "debug")
#        if(LIB)
#          target_link_libraries(${EXETITLE} ${OPT_OR_DEBUG} ${LIB}) 
#        endif(LIB)
#        set(OPT_OR_DEBUG "general")
#      endif(LIB STREQUAL "debug")
#    endforeach(LIB)

#--------------------------------------------------------------------------------
# Now the installation stuff below
#--------------------------------------------------------------------------------
SET(plugin_dest_dir ${EXETITLE/}bin)
SET(qtconf_dest_dir ${EXETITLE}/bin)
SET(APPS "\${CMAKE_INSTALL_PREFIX}/${EXETITLE}/${EXETITLE}")
IF(APPLE)
  SET(plugin_dest_dir ${EXETITLE}/${EXETITLE}.app/Contents/PlugIns)
  SET(qtconf_dest_dir ${EXETITLE}/${EXETITLE}.app/Contents/Resources)
  SET(APPS "\${CMAKE_INSTALL_PREFIX}/${EXETITLE}/${EXETITLE}.app/Contents/MacOS/${EXETITLE}")
ENDIF(APPLE)
# Installation
install(TARGETS ${EXETITLE} DESTINATION ${CMAKE_INSTALL_PREFIX}/${EXETITLE})
file(GLOB PNGS resource/*.png)

install(FILES ${PNG} DESTINATION resource)
#--------------------------------------------------------------------------------
# Install needed Qt plugins by copying directories from the qt installation
# One can cull what gets copied by using 'REGEX "..." EXCLUDE'
INSTALL(DIRECTORY "${QT_PLUGINS_DIR}/imageformats" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)

IF(APPLE)
#for cocoa qt in MAC OS X
INSTALL(DIRECTORY "${QT_QTGUI_LIBRARY}/Resources/qt_menu.nib" DESTINATION ${qtconf_dest_dir} COMPONENT Runtime)
ENDIF(APPLE)

#--------------------------------------------------------------------------------
# install a qt.conf file
# this inserts some cmake code into the install script to write the file
INSTALL(CODE "
    file(WRITE \"\${CMAKE_INSTALL_PREFIX}/${qtconf_dest_dir}/qt.conf\" \"[Paths]\nPlugins = PlugIns\")
    " COMPONENT Runtime)

#--------------------------------------------------------------------------------
# Use BundleUtilities to get all other dependencies for the application to work.
# It takes a bundle or executable along with possible plugins and inspects it
# for dependencies.  If they are not system dependencies, they are copied.


# Now the work of copying dependencies into the bundle/package
# The quotes are escaped and variables to use at install time have their $ escaped
# An alternative is the do a configure_file() on a script and use install(SCRIPT  ...).
# Note that the image plugins depend on QtSvg and QtXml, and it got those copied
# over.
INSTALL(CODE "
    include(BundleUtilities)
    fixup_bundle(\"${APPS}\" \"\${2DX_LIBRARIES}\" \"\")
    " COMPONENT Runtime)


# To Create a package, one can run "cpack -G DragNDrop CPackConfig.cmake" on Mac OS X
# where CPackConfig.cmake is created by including CPack
# And then there's ways to customize this as well
#set(CPACK_BINARY_DRAGNDROP ON)
#include(CPack)

