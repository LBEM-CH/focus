############################################################################
# A 2dx CMakeLists.txt                                                     #
#                                                                          #
# Author:                                                                  #
# Nikhil Biyani                                                            #
############################################################################


cmake_minimum_required(VERSION 2.8.11)

# Load Self defined CMake Modules ---------------------------------------------
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
#------------------------------------------------------------------------------

# Configuration ---------------------------------------------------------------
set(EXETITLE 2dx_gui)           # Application name
set(CMAKE_DEBUG_POSTFIX -dbg)   # Debug options
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -g") #C++ options
#------------------------------------------------------------------------------

# C++ Compiler -----------------------------------------------------------------
if(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	#set(CMAKE_CXX_FLAGS "-openmp -O3 -xhost -no-prec-div -opt-prefetch -unroll-aggressive -m64")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -openmp -O3 -xhost")
	#set(CMAKE_CXX_FLAGS "-openmp -O3 -xhost -ipo -fast -xavx -no-prec-div -unroll")
	add_definitions( -DUSE_CILK )
else(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp -O3 -funroll-loops -W -Waddress -Wchar-subscripts -Wcomment -Wformat -Wmissing-braces -Wparentheses -Wreorder -Wreturn-type -Wsequence-point -Wsign-compare -Wstrict-aliasing -Wstrict-overflow=1 -Wswitch -Wtrigraphs -Wuninitialized  -Wunknown-pragmas -Wunused-function -Wunused-label -Wunused-value -Wvolatile-register-var -Wno-long-long -Wextra")
endif(CMAKE_CXX_COMPILER STREQUAL "/opt/intel/composer_xe_2013.2.146/bin/intel64/icpc")

# Libraries to use ------------------------------------------------------------
set(USE_FFTWF TRUE)
find_package(FFTW)
if(FFTWF_FOUND)
	message(STATUS "@EXETITLE@: Found FFTW at ${FFTWF_LIB}")
	SET(2DX_MERGE_LIBRARIES ${2DX_MERGE_LIBRARIES} ${FFTWF_LIB})
elseif(FFTWF_FOUND)
	message(FATAL_ERROR "FFTW not found!")
endif(FFTWF_FOUND)

#--------------- QT5 -----------------------
FIND_PACKAGE(Qt5 COMPONENTS Widgets WebKitWidgets Network Script OpenGL REQUIRED)

IF(Qt5_FOUND)
    MESSAGE(STATUS "@EXETITLE@: Found QT5 Widgets (Version: ${Qt5Widgets_VERSION_STRING})")
    MESSAGE(STATUS "@EXETITLE@: Found QT5 WebKitWidgets (Version: ${Qt5WebKitWidgets_VERSION_STRING})")
    MESSAGE(STATUS "@EXETITLE@: Found QT5 Network (Version: ${Qt5Network_VERSION_STRING})")
    MESSAGE(STATUS "@EXETITLE@: Found QT5 Script (Version: ${Qt5Script_VERSION_STRING})")
    MESSAGE(STATUS "@EXETITLE@: Found QT5 OpenGL (Version: ${Qt5OpenGL_VERSION_STRING})")
    INCLUDE_DIRECTORIES(${Qt5Widgets_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5WebKitWidgets_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5Network_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5Script_INCLUDE_DIRS})
    INCLUDE_DIRECTORIES(${Qt5OpenGL_INCLUDE_DIRS})
    ADD_DEFINITIONS(${Qt5Widgets_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5WebKitWidgets_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5Network_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5Script_DEFINITIONS})
    ADD_DEFINITIONS(${Qt5OepnGL_DEFINITIONS})
ELSE()
    MESSAGE(STATUS "@EXETITLE@: QT5 not Found!")
ENDIF()

SET(2DX_LIBRARIES ${2DX_LIBRARIES} Qt5::Widgets Qt5::WebKitWidgets Qt5::Network Qt5::Script Qt5::OpenGL)

find_package(OpenGL)
if(OPENGL_FOUND)
        message(STATUS "Found OpenGL at ${OPENGL_LIBRARIES}")
        INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR})
        SET(2DX_LIBRARIES ${2DX_LIBRARIES} ${OPENGL_LIBRARIES})
endif(OPENGL_FOUND)

# Header files ----------------------------------------------------------------
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)
if(APPLE)
	include_directories(/opt/local/include)
endif(APPLE)


# Source files -----------------------------------------------------------------
file(GLOB_RECURSE 2DX_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

# QT forms ---------------------------------------------------------------------
file(GLOB 2DX_UI_FORMS ${CMAKE_CURRENT_SOURCE_DIR}/src/*.ui)

configure_file(src/confVersion.h.in ${CMAKE_CURRENT_SOURCE_DIR}/src/confVersion.h)

# moc files --------------------------------------------------------------------
# these are all the headers containing Q_OBJECT
# TODO: glob these files with a cmake module
set(MOC_HDRS
        mainWindow.h
        src/aboutWindow.h
        src/abstractTool.h
        src/libraryContainer.h
        src/albumDelegate.h
        src/albumModel.h
        src/albumView.h
        src/albumViewer.h
        src/blockContainer.h
        src/colorTool.h
        src/confData.h
        src/confDelegate.h
        src/confEditor.h
        src/confElement.h
        src/confInput.h
        src/confInterface.h
        src/confManual.h
        src/confModel.h
        src/confSection.h
        src/confSectionHeader.h
        src/confValidator.h
        src/controlActionsGroup.h
        src/controlBar.h
        src/ctfTool.h
        src/displayParametersTool.h
        src/eulerWindow.h
        src/fileWatcher.h
        src/fullScreenImage.h
        src/graphicalButton.h
        src/imageAlbum.h
        src/imageContainer.h
        src/imageNavigator.h
        src/imagePreview.h
        src/imageViewer.h
        src/imageWindow.h
        src/importBox.h
        src/importTool.h
        src/largeMRC.h
        src/latticeRefineTool.h
        src/latticeTool.h
        src/levelGroup.h
        src/libraryImageStatus.h
        src/LogViewer.h
        src/mergeContainer.h
        src/mouseAssignTool.h
        src/mrcGraphicsItem.h
        src/mrcImage.h
        src/navigator.h
        src/noScrollComboBox.h
        src/phaseView.h
        src/progressStamps.h
        src/projectDelegate.h
        src/projectModel.h
        src/reprojectWindow.h
        src/resizeableStackedWidget.h
        src/resultsData.h
        src/resultsFile.h
        src/resultsModule.h
        src/resultsParser.h
        src/scriptModule.h
        src/scriptParser.h
        src/scriptProgress.h
        src/scriptTab.h
        src/selectionFFT.h
        src/SpinBoxDelegate.h
        src/spotSelectTool.h
        src/statusViewer.h
        src/textBrowser.h
        src/translator.h
        src/updateWindow.h
        src/viewContainer.h
        src/warningBox.h
        src/zoomWindow.h
)

QT5_WRAP_CPP(MOC_SRCS ${MOC_HDRS})
###############################################################################


# generate headers from the ui files
QT5_WRAP_UI(2DX_UI_HDRS ${2DX_UI_FORMS})
# to wrap thethe UI files 
include_directories(${CMAKE_CURRENT_BINARY_DIR})

message(STATUS "Creating: ${EXETITLE}")

IF( APPLE )
# Define some settings for the Bundle
  # Define some settings for the Bundle
    set(MACOSX_BUNDLE_INFO_STRING "www.2dx.org")
    set(MACOSX_BUNDLE_GUI_IDENTIFIER "org.cina.2dx.${EXETITLE}")
    set(MACOSX_BUNDLE_LONG_VERSION_STRING "${EXETITLE} Version ${2dx_VERSION_MAJOR}.${2dx_VERSION_MINOR}.${2dx_VERSION_PATCH}")
    set(MACOSX_BUNDLE_BUNDLE_NAME "${EXETITLE}")
    set(MACOSX_BUNDLE_SHORT_VERSION_STRING "${2dx_VERSION_MAJOR}.${2dx_VERSION_MINOR}.${2dx_VERSION_PATCH}")
    set(MACOSX_BUNDLE_BUNDLE_VERSION "${2dx_VERSION_MAJOR}.${2dx_VERSION_MINOR}.${2dx_VERSION_PATCH}")
    set(MACOSX_BUNDLE_COPYRIGHT "Copyright C-CINA, 2016. All Rights Reserved.")
    set(MACOSX_BUNDLE_ICON_FILE  icon.icns )
  
  # create a bundle with an icon too!
  ADD_EXECUTABLE( ${EXETITLE} MACOSX_BUNDLE ${2DX_SRC} ${MOC_SRCS} ${2DX_UI_HDRS})
  # Allows for bundle re-creation just by running "make". Also installs bundle icon
  add_custom_command(TARGET ${EXETITLE} POST_BUILD
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources
    COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/MacOS
    COMMAND cp ARGS ${CMAKE_CURRENT_SOURCE_DIR}/resource/${MACOSX_BUNDLE_ICON_FILE}
	${CMAKE_CURRENT_BINARY_DIR}/${EXETITLE}.app/Contents/Resources/${MACOSX_BUNDLE_ICON_FILE})
ELSE( APPLE )
  add_executable( ${EXETITLE} ${2DX_SRC} ${MOC_SRCS} ${2DX_UI_HDRS})
ENDIF( APPLE )

qt5_use_modules(${EXETITLE} Widgets WebKitWidgets Network Script OpenGL)

include_directories(${CMAKE_BINARY_DIR}/include)
target_link_libraries(${EXETITLE} ${FFTWF_LIB} ${2DX_LIBRARIES})
 
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
install(FILES ${PNG} DESTINATION Resource/2dx_gui)
install(FILES ${PNG} DESTINATION Resource)

#--------------------------------------------------------------------------------
# Install needed Qt plugins by copying directories from the qt installation
# One can cull what gets copied by using 'REGEX "..." EXCLUDE'
INSTALL(DIRECTORY "${Qt5_PLUGINS_DIR}/imageformats" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)
IF(APPLE)
	#for  cocoa qt in Mac OS X
	INSTALL(DIRECTORY "${Qt5_PLUGINS_DIR}/platforms" DESTINATION ${plugin_dest_dir} COMPONENT Runtime)
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

