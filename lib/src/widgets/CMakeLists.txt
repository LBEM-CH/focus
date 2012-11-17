############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
set(CMAKE_MODULE_PATH ${CMAKE_INSTALL_PREFIX}/cmake ${CMAKE_MODULE_PATH})
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_MODULE_PATH})
################################################################################
#

# A 2dx library
set(LIBTITLE widgets)
SET(LIBVERSION 1)
project(${LIBTITLE})

cmake_minimum_required(VERSION 2.6)


include(GetPropagateDeps)
include(SetPropagateDeps)
#------------------------------------------------------------------------------
# Configure Config file and include it.
# The dependencies are defined in the Config.cmake file
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake
  ${CMAKE_BINARY_DIR}/share/${LIBTITLE}Config.cmake
  @ONLY)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/Version.cmake
  ${CMAKE_BINARY_DIR}/share/${LIBTITLE}ConfigVersion.cmake
  @ONLY)

include(${CMAKE_BINARY_DIR}/share/${LIBTITLE}Config.cmake)

# Header files -----------------------------------------------------------------
if(NOT DEFINED 2DX_INCLUDE_DIR)
	set(2DX_INCLUDE_DIR ../../../include CACHE INTERNAL "the 2dx include dir")
	message(STATUS "2DX_INCLUDE_DIR is not set. Setting it to: ${2DX_INCLUDE_DIR}" )
endif(NOT DEFINED 2DX_INCLUDE_DIR)
include_directories(${2DX_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_BINARY_DIR}/include)


# Source files -----------------------------------------------------------------
file(GLOB SRC *.cpp)

# Moc files --------------------------------------------------------------------
# these are all the headers containing Q_OBJECT
set(MOC_HDRS
	${2DX_INCLUDE_DIR}/scriptProgress.h
	${2DX_INCLUDE_DIR}/aboutWindow.h
	${2DX_INCLUDE_DIR}/graphicalButton.h
	${2DX_INCLUDE_DIR}/viewContainer.h
	${2DX_INCLUDE_DIR}/LogViewer.h
	${2DX_INCLUDE_DIR}/controlBar.h
	${2DX_INCLUDE_DIR}/confInterface.h
	${2DX_INCLUDE_DIR}/confInput.h
	${2DX_INCLUDE_DIR}/confValidator.h
	${2DX_INCLUDE_DIR}/levelGroup.h
	${2DX_INCLUDE_DIR}/imagePreview.h
	${2DX_INCLUDE_DIR}/scriptModule.h
	${2DX_INCLUDE_DIR}/SpinBoxDelegate.h
	${2DX_INCLUDE_DIR}/phaseView.h
	${2DX_INCLUDE_DIR}/confSectionHeader.h
	${2DX_INCLUDE_DIR}/imageViewer.h
	${2DX_INCLUDE_DIR}/confManual.h
	${2DX_INCLUDE_DIR}/confModel.h
	${2DX_INCLUDE_DIR}/textBrowser.h
	${2DX_INCLUDE_DIR}/updateWindow.h
	${2DX_INCLUDE_DIR}/mrcGraphicsItem.h
	${2DX_INCLUDE_DIR}/resizeableStackedWidget.h
	${2DX_INCLUDE_DIR}/abstractTool.h
	${2DX_INCLUDE_DIR}/latticeTool.h
	${2DX_INCLUDE_DIR}/imageNavigator.h
	${2DX_INCLUDE_DIR}/ctfTool.h
	${2DX_INCLUDE_DIR}/fullScreenImage.h
	${2DX_INCLUDE_DIR}/selectionFFT.h
	${2DX_INCLUDE_DIR}/zoomWindow.h
	${2DX_INCLUDE_DIR}/latticeRefineTool.h
	${2DX_INCLUDE_DIR}/spotSelectTool.h
	${2DX_INCLUDE_DIR}/mouseAssignTool.h
	${2DX_INCLUDE_DIR}/displayParametersTool.h
	${2DX_INCLUDE_DIR}/confEditor.h
	${2DX_INCLUDE_DIR}/colorTool.h
	${2DX_INCLUDE_DIR}/noScrollComboBox.h
)

set(CMAKE_DEBUG_POSTFIX -dbg)
  
if(QT4_FOUND)
	# generate moc files
	#QT4_WRAP_CPP(MOC_SRCS ${MOC_HDRS} OPTIONS "-I${2DX_INCLUDE_DIR} -p${CMAKE_CURRENT_SOURCE_DIR}")
	QT4_WRAP_CPP(MOC_SRCS ${MOC_HDRS}) 
endif(QT4_FOUND)

add_library(${LIBTITLE} STATIC ${SRC} ${MOC_SRCS}) 

set_target_properties(${LIBTITLE} PROPERTIES
  VERSION ${LIBVERSION}
  SOVERSION ${LIBVERSION})

set_propagate_deps(${LIBTITLE})
# Includes and libraries
#target_link_libraries(${LIBTITLE}
#  ${2DX_LIBRARIES})

# Installation
#install(TARGETS ${LIBTITLE} DESTINATION lib)

