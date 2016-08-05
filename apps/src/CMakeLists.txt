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
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/widgets/confVersion.h.in ${CMAKE_CURRENT_SOURCE_DIR}/widgets/confVersion.h)
#-------------------------------------------------------------------------------

#--- BASE DIRECTORIES ----------------------------------------------------------
# Header files
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/data)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/dialogs)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/extentions)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/models)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/mrcImage)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/settings)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/widgets)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/windows)

# Source Files
FILE(GLOB_RECURSE SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

# MOC files
set(MOC_HDRS
        data/resultsData.h
        data/confData.h
        data/confElement.h
        data/confDelegate.h
        data/confSection.h
        widgets/libraryImageStatus.h
        widgets/confInterface.h
        widgets/resizeableStackedWidget.h
        widgets/confInput.h
        widgets/translator.h
        widgets/noScrollComboBox.h
        widgets/imageViewer.h
        widgets/statusViewer.h
        widgets/confValidator.h
        widgets/mrcGraphicsItem.h
        widgets/zoomWindow.h
        widgets/textBrowser.h
        widgets/resultsParser.h
        widgets/viewContainer.h
        widgets/confEditor.h
        widgets/selectionFFT.h
        widgets/colorTool.h
        widgets/SpinBoxDelegate.h
        widgets/graphicalButton.h
        widgets/imagePreview.h
        widgets/displayParametersTool.h
        widgets/latticeTool.h
        widgets/scriptModule.h
        widgets/confSectionHeader.h
        widgets/abstractTool.h
        widgets/fullScreenImage.h
        widgets/ctfTool.h
        widgets/resultsModule.h
        widgets/latticeRefineTool.h
        widgets/warningBox.h
        widgets/mouseAssignTool.h
        widgets/log_viewer.h
        widgets/blockContainer.h
        widgets/imageNavigator.h
        widgets/resultsFile.h
        widgets/phaseView.h
        widgets/spotSelectTool.h
        extentions/scriptParser.h
        extentions/fileWatcher.h
        mrcImage/largeMRC.h
        mrcImage/mrcImage.h
        windows/library_tab.h
        windows/image_window.h
        windows/image_tab.h
        windows/merge_tab.h
        dialogs/updateWindow.h
        dialogs/aboutWindow.h
        dialogs/eulerWindow.h
        dialogs/reprojectWindow.h
        dialogs/project_tools.h
        dialogs/preferences.h
        dialogs/open_project_wizard.h
        models/confModel.h
        models/albumModel.h
        models/projectModel.h
        models/projectDelegate.h
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