#------------------------------------------------------------------------------
# @file CMakeLists.txt
# @author Nikhil Biyani
# @author Marcel Arheit
# @location 2dx/apps/src
#------------------------------------------------------------------------------

#--- CMAKE Configurations ------------------------------------------------------
cmake_minimum_required(VERSION 2.8.11)
#-------------------------------------------------------------------------------

#--- Configuration -------------------------------------------------------------
SET(LIBTITLE 2dx_app)
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
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/windows)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Source Files
FILE(GLOB_RECURSE SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

# MOC files
set(MOC_HDRS
        data/ParameterConfiguration.h
        data/ParameterElementData.h
        data/ParameterSectionData.h
        data/ProjectData.h
        data/ResultsData.h
        data/ScriptData.h
        extentions/FileWatcher.h
        extentions/ScriptParser.h
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
        widgets/ImageLibrary.h
        widgets/ImageNavigator.h
        widgets/ImageThumbnails.h
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
        widgets/ResultsModule.h
        widgets/ScriptModule.h
        widgets/SelectionFFT.h
        widgets/SpinBoxDelegate.h
        widgets/SpotSelectTool.h
        widgets/StatusViewer.h
        widgets/TextBrowser.h
        widgets/Translator.h
        widgets/YesNoWidget.h
        widgets/ZoomWindow.h
        windows/AboutWindow.h
        windows/ExecutionWindow.h
        windows/ImageTab.h
        windows/LibraryTab.h
        windows/OpenProjectWizard.h
        windows/PreferencesDialog.h
        windows/UpdateWindow.h
)

# Create MOC Sources
QT5_WRAP_CPP(MOC_SRCS ${MOC_HDRS})

#-------------------------------------------------------------------------------

#--- Add Library --------------------------------------------------------------
ADD_LIBRARY(${LIBTITLE} STATIC ${SRC} ${MOC_SRCS})
QT5_USE_MODULES(${LIBTITLE} Widgets WebKitWidgets Network Script OpenGL)
TARGET_LINK_LIBRARIES(${LIBTITLE} ${2DX_LIBRARIES})
set_propagate_deps(${LIBTITLE}) # Include all the current header directories
#-------------------------------------------------------------------------------
