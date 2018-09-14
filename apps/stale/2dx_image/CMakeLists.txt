#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @author Marcel Arheit
# @location 2dx/apps/src/2dx_gui/
#------------------------------------------------------------------------------

#--- CMAKE Configurations ------------------------------------------------------
cmake_minimum_required(VERSION 2.8.11)
#-------------------------------------------------------------------------------

#--- Configuration -------------------------------------------------------------
set(EXETITLE 2dx_image)           # Application name
#-------------------------------------------------------------------------------

#--- BASE DIRECTORIES ----------------------------------------------------------
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}) # Header Files
get_propagate_deps(2dx_app) # Get the header files from the lib
SET(2DX_SRC main.cpp) # Source Files
#-------------------------------------------------------------------------------

#--- Create Executable ---------------------------------------------------------
MESSAGE(STATUS "Creating: ${EXETITLE}")

IF( APPLE )
    # Define some settings for the Bundle
    SET(MACOSX_BUNDLE_INFO_STRING "www.2dx.org")
    SET(MACOSX_BUNDLE_GUI_IDENTIFIER "org.cina.2dx.${EXETITLE}")
    SET(MACOSX_BUNDLE_LONG_VERSION_STRING "${EXETITLE} Version ${2dx_VERSION_MAJOR}.${2dx_VERSION_MINOR}.${2dx_VERSION_PATCH}")
    SET(MACOSX_BUNDLE_BUNDLE_NAME "${EXETITLE}")
    SET(MACOSX_BUNDLE_SHORT_VERSION_STRING "${2dx_VERSION_MAJOR}.${2dx_VERSION_MINOR}.${2dx_VERSION_PATCH}")
    SET(MACOSX_BUNDLE_BUNDLE_VERSION "${2dx_VERSION_MAJOR}.${2dx_VERSION_MINOR}.${2dx_VERSION_PATCH}")
    SET(MACOSX_BUNDLE_COPYRIGHT "Copyright C-CINA, 2016. All Rights Reserved.")
    SET(MACOSX_BUNDLE_ICON_FILE  icon.icns )

    # create a bundle with an icon too!
    ADD_EXECUTABLE( ${EXETITLE} MACOSX_BUNDLE ${2DX_SRC} )
    
    # Allows for bundle re-creation just by running "make". Also installs bundle icon
    ADD_CUSTOM_COMMAND(TARGET ${EXETITLE} POST_BUILD
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources
            COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/MacOS
            COMMAND cp ARGS ${CMAKE_CURRENT_SOURCE_DIR}/icons/${MACOSX_BUNDLE_ICON_FILE}
            ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources/${MACOSX_BUNDLE_ICON_FILE})
ELSE( APPLE )
    ADD_EXECUTABLE( ${EXETITLE} ${2DX_SRC} )
ENDIF( APPLE )

TARGET_LINK_LIBRARIES(${EXETITLE} ${FFTWF_LIB} ${2DX_LIBRARIES} 2dx_app)
#-------------------------------------------------------------------------------
 
#--- INSTALL -------------------------------------------------------------------
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


file(GLOB PNG icons/*.png)
install(FILES ${PNG} DESTINATION ${EXETITLE}/icons/)

# Install needed Qt plugins by copying directories from the qt installation
# One can cull what gets copied by using 'REGEX "..." EXCLUDE'
INSTALL(DIRECTORY "${Qt5_PLUGINS_DIR}/imageformats" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)
IF(APPLE)
	#for  cocoa qt in Mac OS X
	INSTALL(DIRECTORY "${Qt5_PLUGINS_DIR}/platforms" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)
ENDIF(APPLE)

# install a qt.conf file
# this inserts some cmake code into the install script to write the file
INSTALL(CODE "
    file(WRITE \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${qtconf_dest_dir}/qt.conf\" \"[Paths]\nPlugins = PlugIns\")
    " COMPONENT Runtime)

# Use BundleUtilities to get all other dependencies for the application to work.
# It takes a bundle or executable along with possible plugins and inspects it
# for dependencies.  If they are not system dependencies, they are copied.

# directories to look for dependencies
SET(DIRS ${Qt5_LIB_DIR})

file(GLOB Qt5Libs ${DIRS}/libQt5Gui*)
install(FILES ${Qt5Libs} DESTINATION lib)

# Now the work of copying dependencies into the bundle/package
# The quotes are escaped and variables to use at install time have their $ escaped
# An alternative is the do a configure_file() on a script and use install(SCRIPT  ...).
# Note that the image plugins depend on QtSvg and QtXml, and it got those copied
# over.
if(APPLE_BUNDLE)
	INSTALL(CODE "
	    file(GLOB_RECURSE QTPLUGINS
		       \"${CMAKE_INSTALL_PREFIX}/${plugin_dest_dir}/*${CMAKE_SHARED_LIBRARY_SUFFIX}\")
	    include(InstallRequiredSystemLibraries)    
	    include(BundleUtilities)
	    fixup_bundle(\"${APPS}\" \"\${QTPLUGINS}\" \"\${DIRS}\" \"\")
	    verify_app(\"${APPS}\")
	    " COMPONENT Runtime)
endif(APPLE_BUNDLE)

# To Create a package, one can run "cpack -G DragNDrop CPackConfig.cmake" on Mac OS X
# where CPackConfig.cmake is created by including CPack
# And then there's ways to customize this as well
#set(CPACK_BINARY_DRAGNDROP ON)
#include(CPack)
#-------------------------------------------------------------------------------