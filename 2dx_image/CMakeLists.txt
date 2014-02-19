############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
################################################################################
# Application Name
set(EXETITLE 2dx_image)
#project( ${EXETITLE} )

cmake_minimum_required(VERSION 2.6)

set(CMAKE_CXX_FLAGS "-O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra -Wno-cpp")

set(CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR}/share ${CMAKE_MODULE_PATH})
set(CMAKE_PREFIX_PATH ${CMAKE_BINARY_DIR}/share ${CMAKE_PREFIX_PATH})
message(STATUS "CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH}")

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
set(USE_FFTWF TRUE)
find_package(FFTW)
if(FFTWF_FOUND)
	message(STATUS "@EXETITLE@: Found FFTW at ${FFTWF_LIB}")
	SET(2DX_IMAGE_LIBRARIES  ${FFTWF_LIB} ${2DX_IMAGE_LIBRARIES})
else(FFTWF_FOUND)
	message(FATAL_ERROR "@EXETITLE@: FFTW not found!")
endif(FFTWF_FOUND)
 
find_package(Qt4 COMPONENTS QtCore QtGui QtWebKit QtNetwork QtScript QtOpenGL REQUIRED)
if(QT4_FOUND)
	message(STATUS "@EXETITLE@: Found QT4 at ${QT_LIBRARY_DIR}")
	include(${QT_USE_FILE})
        SET(2DX_IMAGE_LIBRARIES ${2DX_IMAGE_LIBRARIES} ${QT_LIBRARIES})
        #message(STATUS "2DX_IMAGE_LIBRARIES ${2DX_IMAGE_LIBRARIES}")
endif(QT4_FOUND)
find_package(OpenGL)
if(OPENGL_FOUND)
        message(STATUS "@EXETITLE@: Found OpenGL at ${OPENGL_LIBRARIES}")
        INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR})
        SET(2DX_IMAGE_LIBRARIES ${2DX_IMAGE_LIBRARIES} ${OPENGL_LIBRARIES})
endif(OPENGL_FOUND)

# Header files -----------------------------------------------------------------
if(NOT DEFINED 2DX_INCLUDE_DIR)
        set(2DX_INCLUDE_DIR ../include CACHE INTERNAL "the 2dx include dir")
        message(STATUS "2DX_INCLUDE_DIR is not set. Setting it to: ${2DX_INCLUDE_DIR}" )
endif(NOT DEFINED 2DX_INCLUDE_DIR)
include_directories(${2DX_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_BINARY_DIR}/include)

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
  set( MACOSX_BUNDLE_INFO_STRING ""${CPACK_PACKAGE_VERSION}",www.2dx.org" )
  set( MACOSX_BUNDLE_SHORT_VERSION_STRING "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}" )
  set( MACOSX_BUNDLE_LONG_VERSION_STRING "${CPACK_PACKAGE_VERSION}" )
  set( MACOSX_BUNDLE_BUNDLE_VERSION "${CPACK_PACKAGE_VERSION}" )
  
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
  add_executable(${EXETITLE} ${SRC} ${MOC_SRCS})
endif(APPLE)



#add the libraries that were built before
get_propagate_deps(conf widgets mrcImage extentions)

# Libraries 
#
# last thing we have to do is to tell CMake what libraries our executable
# needs,
#
# luckily FIND_PACKAGE prepared 2DX_IMAGE_LIBRARIES variable for us:
#
#------------------------------------------------------------------------------

target_link_libraries(${EXETITLE}  conf widgets mrcImage extentions ${2DX_IMAGE_LIBRARIES})

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
file(GLOB PNG resource/*.png)

#create symlink to the kernel
install(SCRIPT install_symlink.cmake)

install(FILES ${PNG} DESTINATION Resource/2dx_image)
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

# directories to look for dependencies
SET(DIRS ${QT_LIBRARY_DIRS})
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

