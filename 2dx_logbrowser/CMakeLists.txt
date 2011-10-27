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
set(EXETITLE 2dx_logbrowser)
project( ${EXETITLE} )

# set version
set( VERSION_MAJOR "3" )
set( VERSION_MINOR "3" )
set( VERSION_PATCH "0" )

# OS specific project properties ----------------------------------------------
IF( APPLE )
	# for 64bit us the follwing line
	#SET( CMAKE_OSX_ARCHITECTURES ppc;i386;ppc64;x86_64) 
	#SET( CMAKE_OSX_ARCHITECTURES ppc;i386) 
ENDIF( APPLE )

# Libraries to use ------------------------------------------------------------
find_package(Qt4 REQUIRED)
  if(QT4_FOUND)
    message(STATUS "@LIBTITLE@: Found QT4 at ${QT_LIBRARY_DIR}")
    SET(QT_INCLUDES  ${QT_QTCORE_INCLUDE_DIR}  ${QT_INCLUDES})
    include(${QT_USE_FILE})
    INCLUDE_DIRECTORIES(${QT_INCLUDES})
    SET(LIBRARIES   ${QT_LIBRARIES})
  else(QT4_FOUND)
    message(STATUS "@LIBTITLE@: Qt4 not Found!")
  endif(QT4_FOUND)
  

# Header files -----------------------------------------------------------------
set(HEADERS
	LBMainWindow.h
	LogBrowserCommons.h
	LogDirectory.h
	LogFileReader.h 
	LogViewer.h
	ViewerContainer.h)
# Source files -----------------------------------------------------------------
set(SOURCES
	LBMainWindow.cpp
	LogDirectory.cpp
	LogFileReader.cpp
	LogViewer.cpp 
	p2dxlogbrowser.cpp
	ViewerContainer.cpp)
# moc files --------------------------------------------------------------------
# these are all the headers containing Q_OBJECT
set(MOC_HDRS
	LBMainWindow.h      
	LogViewer.h 
	ViewerContainer.h 
	)

###############################################################################
# generate moc files
QT4_WRAP_CPP(MOC_SRCS ${MOC_HDRS})


message(STATUS "Creating: ${EXETITLE}")
if(APPLE)
  # Define some settings for the Bundle
  set( MACOSX_BUNDLE_BUNDLE_NAME ${EXETITLE} )
  set( MACOSX_BUNDLE_GUI_IDENTIFIER "2dx logbrowser" )
  set( MACOSX_BUNDLE_ICON_FILE icon_OSX.icns )
  set( MACOSX_BUNDLE_INFO_STRING ""${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}",www.2dx.org" )
  set( MACOSX_BUNDLE_SHORT_VERSION_STRING "${VERSION_MAJOR}.${VERSION_MINOR}" )
  set( MACOSX_BUNDLE_LONG_VERSION_STRING "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )
  set( MACOSX_BUNDLE_BUNDLE_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )
  
  # create a bundle with an icon too!
  add_executable( ${EXETITLE} MACOSX_BUNDLE ${SOURCES} ${MOC_SRCS})
  # Allows for bundle re-creation just by running "make". Also installs bundle icon
  add_custom_command(TARGET ${EXETITLE} POST_BUILD
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/MacOS
    COMMAND cp ARGS ${CMAKE_CURRENT_SOURCE_DIR}/resource/${MACOSX_BUNDLE_ICON_FILE}
	${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources/${MACOSX_BUNDLE_ICON_FILE})
 ## This tells cmake where to place files inside the bundle
  #set_source_files_properties( ${ProjectName_RESOURCES} ${ProjectName_TRANSLATIONS} PROPERTIES MACOSX_PACKAGE_LOCATION Resources )
else(APPLE)
  add_executable( ${EXETITLE} ${SOURCES} ${MOC_SRCS})
endif(APPLE)

# Linking
target_link_libraries(${EXETITLE} ${LIBRARIES}) 

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
install(TARGETS ${EXETITLE} DESTINATION ${EXETITLE})
file(GLOB PNGS resource/*.png)

install(FILES ${PNG} DESTINATION resource)
#--------------------------------------------------------------------------------
# Install needed Qt plugins by copying directories from the qt installation
# One can cull what gets copied by using 'REGEX "..." EXCLUDE'
#INSTALL(DIRECTORY "${QT_PLUGINS_DIR}/imageformats" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)
IF(APPLE)
	#for  cocoa qt in Mac OS X
	INSTALL(DIRECTORY "${QT_QTGUI_LIBRARY}/Resources/qt_menu.nib" DESTINATION ${qtconf_dest_dir} COMPONENT Runtime)
ENDIF(APPLE)

#--------------------------------------------------------------------------------
# install a qt.conf file
# this inserts some cmake code into the install script to write the file
INSTALL(CODE "
    file(WRITE \"\${CMAKE_INSTALL_PREFIX}/${qtconf_dest_dir}/qt.conf\" \"\")
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
