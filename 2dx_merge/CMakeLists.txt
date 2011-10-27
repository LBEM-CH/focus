############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
cmake_minimum_required(VERSION 2.6)
################################################################################
# Application Name
set(EXETITLE 2dx_merge)
project( ${EXETITLE} )

# set version
set( VERSION_MAJOR "3" )
set( VERSION_MINOR "3" )
set( VERSION_PATCH "0" )
## OS specific project properties ----------------------------------------------
IF( APPLE )
	# for 64bit us the follwing line
	#SET( CMAKE_OSX_ARCHITECTURES ppc;i386;ppc64;x86_64) 
	#to be able to use the QT Carbon 32 bit
	#SET( CMAKE_OSX_ARCHITECTURES ppc;i386) 
ENDIF( APPLE )

# Libraries to use ------------------------------------------------------------
set(USE_FFTWF TRUE)
find_package(FFTW)
if(FFTW_FOUND)
	message(STATUS "@LIBTITLE@: Found FFTW at ${FFTW_LIB}")
	SET(2DX_LIBRARIES  ${FFTW_LIB} ${2DX_LIBRARIES})
elseif(FFTW_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTW_FOUND)
find_package(Qt4 COMPONENTS QtCore QtGui QtScript QtNetwork QtWebKit QtOpenGL REQUIRED)
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
if(APPLE)
	include_directories(/opt/local/include)
endif(APPLE)

# Source files -----------------------------------------------------------------
# takes all files ending with .cpp
file(GLOB 2DX_MERGE_SRC *.cpp)

# QT forms ---------------------------------------------------------------------
file(GLOB 2DX_MERGE_UI_FORMS *.ui)



# moc files --------------------------------------------------------------------
# these are all the headers containing Q_OBJECT
# TODO: glob these files with a cmake module
set(MOC_HDRS
	albumControl.h
	albumDelegate.h
	albumModel.h
	albumView.h
	albumViewer.h
	imageAlbum.h
	importBox.h  
	importTool.h  
	mainWindow.h  
	projectDelegate.h  
	projectModel.h  
	resultsData.h  
	resultsModule.h  
	translator.h  
	)
###############################################################################
set(CMAKE_DEBUG_POSTFIX -dbg)


# generate moc files
QT4_WRAP_CPP(MOC_SRCS ${MOC_HDRS})

# generate headers from the ui files
QT4_WRAP_UI(2DX_MERGE_UI_HDRS ${2DX_MERGE_UI_FORMS})
# to wrap thethe UI files 
include_directories(${CMAKE_CURRENT_BINARY_DIR})




message(STATUS "Creating: ${EXETITLE}")

IF( APPLE )
# Define some settings for the Bundle
  set( MACOSX_BUNDLE_BUNDLE_NAME ${EXETITLE} )
  set( MACOSX_BUNDLE_GUI_IDENTIFIER "2dx merge" )
  set( MACOSX_BUNDLE_ICON_FILE  icon.icns )
  set( MACOSX_BUNDLE_INFO_STRING ""${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}",www.2dx.org" )
  set( MACOSX_BUNDLE_SHORT_VERSION_STRING "${VERSION_MAJOR}.${VERSION_MINOR}" )
  set( MACOSX_BUNDLE_LONG_VERSION_STRING "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )
  set( MACOSX_BUNDLE_BUNDLE_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )
  
  # create a bundle with an icon too!
  ADD_EXECUTABLE( ${EXETITLE} MACOSX_BUNDLE ${2DX_MERGE_SRC} ${MOC_SRCS} ${2DX_MERGE_UI_HDRS})
  # Allows for bundle re-creation just by running "make". Also installs bundle icon
  add_custom_command(TARGET ${EXETITLE} POST_BUILD
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/MacOS
    COMMAND cp ARGS ${CMAKE_CURRENT_SOURCE_DIR}/resource/${MACOSX_BUNDLE_ICON_FILE}
	${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources/${MACOSX_BUNDLE_ICON_FILE})
ELSE( APPLE )
  add_executable( ${EXETITLE} ${2DX_MERGE_SRC} ${MOC_SRCS} ${2DX_MERGE_UI_HDRS})
ENDIF( APPLE )

 
get_propagate_deps(conf widgets mrcImage extentions)
#
# last thing we have to do is to tell CMake what olibraries our executable
# needs,
#
# luckily FIND_PACKAGE prepared 2DX_LIBRARIES variable for us:
#
target_link_libraries(${EXETITLE} ${2DX_LIBRARIES} conf widgets mrcImage extentions)
 
#--------------------------------------------------------------------------------
# Now the installation stuff below
#--------------------------------------------------------------------------------
SET(plugin_dest_dir ${EXETITLE}/plugins)
SET(qtconf_dest_dir ${EXETITLE}/resources)
SET(APPS "\${CMAKE_INSTALL_PREFIX}/${EXETITLE}/${EXETITLE}")
IF(APPLE)
  SET(plugin_dest_dir ${EXETITLE}/${EXETITLE}.app/Contents/PlugIns)
  SET(qtconf_dest_dir ${EXETITLE}/${EXETITLE}.app/Contents/Resources)
  SET(APPS "\${CMAKE_INSTALL_PREFIX}/${EXETITLE}/${EXETITLE}.app/Contents/MacOS/${EXETITLE}")
ENDIF(APPLE)

	# Installation
install(TARGETS ${EXETITLE} DESTINATION ${EXETITLE})

# create symlink to the kernel
install(SCRIPT install_symlink.cmake)

file(GLOB PNG resource/*.png)
install(FILES ${PNG} DESTINATION resource)
#--------------------------------------------------------------------------------
# Install needed Qt plugins by copying directories from the qt installation
# One can cull what gets copied by using 'REGEX "..." EXCLUDE'
INSTALL(DIRECTORY "${QT_PLUGINS_DIR}/imageformats" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)
IF(APPLE)
	#for  cocoa qt in Mac OS X
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

# directories to look for dependencies
SET(DIRS ${QT_LIBRARY_DIRS})

# Now the work of copying dependencies into the bundle/package
# The quotes are escaped and variables to use at install time have their $ escaped
# An alternative is the do a configure_file() on a script and use install(SCRIPT  ...).
# Note that the image plugins depend on QtSvg and QtXml, and it got those copied
# over.
if(APPLE_BUNDLE)
	INSTALL(CODE "
	    include(BundleUtilities)
	    fixup_bundle(\"${APPS}\" \"\${2DX_LIBRARIES}\" \"\")
	    " COMPONENT Runtime)
endif(APPLE_BUNDLE)

# To Create a package, one can run "cpack -G DragNDrop CPackConfig.cmake" on Mac OS X
# where CPackConfig.cmake is created by including CPack
# And then there's ways to customize this as well
#set(CPACK_BINARY_DRAGNDROP ON)
#include(CPack)

