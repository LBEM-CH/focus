############################################################################
# CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
################################################################################
# Application Name
set(EXETITLE fLogBrowser)

# Libraries to use ------------------------------------------------------------
FIND_PACKAGE(Qt5 COMPONENTS Widgets REQUIRED)

IF(Qt5_FOUND)
    MESSAGE(STATUS "@LIBTITLE@: Found QT5 Widgets (Version: ${Qt5Widgets_VERSION_STRING})")
    INCLUDE_DIRECTORIES(${Qt5Widgets_INCLUDE_DIRS})
    ADD_DEFINITIONS(${Qt5Widgets_DEFINITIONS})
ELSE()
    MESSAGE(STATUS "@LIBTITLE@: QT5 not Found!")
ENDIF()

SET(FOCUS_LOG_LIBRARIES   Qt5::Widgets) 


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
	pfocusLogBrowser.cpp
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
QT5_WRAP_CPP(MOC_SRCS ${MOC_HDRS})


message(STATUS "Creating: ${EXETITLE}")
if(APPLE)
  # Define some settings for the Bundle
  set(MACOSX_BUNDLE_INFO_STRING "www.focus-em.org")
  set(MACOSX_BUNDLE_GUI_IDENTIFIER "org.cina.focus.${EXETITLE}")
  set(MACOSX_BUNDLE_LONG_VERSION_STRING "${EXETITLE} Version ${FOCUS_VERSION_MAJOR}.${FOCUS_VERSION_MINOR}.${FOCUS_VERSION_PATCH}")
  set(MACOSX_BUNDLE_BUNDLE_NAME "${EXETITLE}")
  set(MACOSX_BUNDLE_SHORT_VERSION_STRING "${FOCUS_VERSION_MAJOR}.${FOCUS_VERSION_MINOR}.${FOCUS_VERSION_PATCH}")
  set(MACOSX_BUNDLE_BUNDLE_VERSION "${FOCUS_VERSION_MAJOR}.${FOCUS_VERSION_MINOR}.${FOCUS_VERSION_PATCH}")
  set(MACOSX_BUNDLE_COPYRIGHT "Copyright C-CINA, 2015. All Rights Reserved.")
  set( MACOSX_BUNDLE_ICON_FILE  icon.icns)
  
  # create a bundle with an icon too!
  add_executable( ${EXETITLE} MACOSX_BUNDLE ${SOURCES} ${MOC_SRCS})
  # Allows for bundle re-creation just by running "make". Also installs bundle icon
  add_custom_command(TARGET ${EXETITLE} POST_BUILD
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/MacOS
    COMMAND cp ARGS ${CMAKE_CURRENT_SOURCE_DIR}/icons/${MACOSX_BUNDLE_ICON_FILE}
	${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources/${MACOSX_BUNDLE_ICON_FILE})
 ## This tells cmake where to place files inside the bundle
  #set_source_files_properties( ${ProjectName_RESOURCES} ${ProjectName_TRANSLATIONS} PROPERTIES MACOSX_PACKAGE_LOCATION Resources )
else(APPLE)
  add_executable( ${EXETITLE} ${SOURCES} ${MOC_SRCS})
endif(APPLE)

qt5_use_modules(${EXETITLE} Widgets)

# Linking
target_link_libraries(${EXETITLE} ${FOCUS_LOG_LIBRARIES}) 

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

file(GLOB PNG icons/*.png)
install(FILES ${PNG} DESTINATION ${EXETITLE}/icons/)

set(CMAKE_CXX_FLAGS "-O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra")

#--------------------------------------------------------------------------------
# Install needed Qt plugins by copying directories from the qt installation
# One can cull what gets copied by using 'REGEX "..." EXCLUDE'
#INSTALL(DIRECTORY "${QT_PLUGINS_DIR}/imageformats" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)
IF(APPLE)
	#for  cocoa qt in Mac OS X
	INSTALL(DIRECTORY "${Qt5_PLUGINS_DIR}/platforms" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)
ENDIF(APPLE)

#--------------------------------------------------------------------------------
# install a qt.conf file
# this inserts some cmake code into the install script to write the file
INSTALL(CODE "
    file(WRITE \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${qtconf_dest_dir}/qt.conf\" \"\")
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
            set(BU_CHMOD_BUNDLE_ITEMS ON)
	    include(InstallRequiredSystemLibraries)    
	    include(BundleUtilities)
	      fixup_bundle(\"${APPS}\" \"\${FOCUS_LIBRARIES}\" \"\")
	    verify_app(\"${APPS}\")
	    " COMPONENT Runtime)
endif(APPLE_BUNDLE)


# To Create a package, one can run "cpack -G DragNDrop CPackConfig.cmake" on Mac OS X
# where CPackConfig.cmake is created by including CPack
# And then there's ways to customize this as well
#set(CPACK_BINARY_DRAGNDROP ON)
#include(CPack)
