#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @author Marcel Arheit
# @location focus/apps/src
#------------------------------------------------------------------------------

#--- CMAKE Configurations ------------------------------------------------------
cmake_minimum_required(VERSION 4.0)
#-------------------------------------------------------------------------------

#--- Configuration -------------------------------------------------------------
SET(LIBTITLE focus_app)
SET(LIBVERSION 1)
PROJECT(${LIBTITLE})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/data/Version.h.in ${CMAKE_CURRENT_BINARY_DIR}/Version.h)
#-------------------------------------------------------------------------------

#--- BASE DIRECTORIES ----------------------------------------------------------
# Header files
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/data)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/extentions)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/models)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/mrcImage)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/settings)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/widgets)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Source Files
FILE(GLOB_RECURSE SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

# MOC files
set(MOC_HDRS
        data/ParameterConfiguration.h
        data/ParameterElementData.h
        data/ParameterSectionData.h
        data/ProjectData.h
        data/ProjectImage.h
        data/ResultsData.h
        data/ScriptData.h
        extentions/FileWatcher.h
        extentions/ImageScriptProcessor.h
        extentions/ScriptParser.h
        models/ProcessingModel.h
        models/ProjectDelegate.h
        models/ProjectModel.h
        mrcImage/largeMRC.h
        mrcImage/mrcImage.h
        widgets/AbstractTool.h
        widgets/BlockContainer.h
        widgets/BrowserWidget.h
        widgets/ColorTool.h
        widgets/CtfTool.h
        widgets/DisplayParametersTool.h
        widgets/FullScreenImage.h
        widgets/GraphicalButton.h
        widgets/SelectionCountHeader.h
        widgets/ImageNavigator.h
        widgets/ImageViewer.h
        widgets/LatticeRefineTool.h
        widgets/LibraryImageStatus.h
        widgets/LineEditSet.h
        widgets/LogViewer.h
        widgets/MouseAssignTool.h
        widgets/NoScrollComboBox.h
        widgets/ParameterInput.h
        widgets/ParameterSection.h
        widgets/ParameterWidget.h
        widgets/PhaseView.h
        widgets/ProcessDialog.h
        widgets/ProcessingManager.h
        widgets/ResultsModule.h
        widgets/ScriptModule.h
        widgets/ScriptHelp.h
        widgets/ScriptSelectorDialog.h
        widgets/SelectionCountHeader.h
        widgets/SelectionFFT.h
        widgets/SpinBoxDelegate.h
        widgets/SpotSelectTool.h
        widgets/StatusViewer.h
        widgets/TextBrowser.h
        widgets/Translator.h
        widgets/YesNoWidget.h
        widgets/ZoomWindow.h
        
)

# Create MOC Sources
QT5_WRAP_CPP(MOC_SRCS ${MOC_HDRS})

#-------------------------------------------------------------------------------

#--- Add Library --------------------------------------------------------------
ADD_LIBRARY(${LIBTITLE} STATIC ${SRC} ${MOC_SRCS})
QT5_USE_MODULES(${LIBTITLE} Widgets Network Script OpenGL)
TARGET_LINK_LIBRARIES(${LIBTITLE} ${FOCUS_LIBRARIES})
set_propagate_deps(${LIBTITLE}) # Include all the current header directories
#-------------------------------------------------------------------------------
