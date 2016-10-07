################################################################
# A config file to load Qt5
# Sets the following variables
#   Qt5_Found
#   Qt5_NOT_FOUND_MESSAGE (if not found)
#   Qt5_VERSION
#   Qt5_DIR
#   Qt5_LIB_DIR
#   Qt5_INCLUDE_DIR
#   Qt5_PLUGINS_DIR
#   Qt5_IMPORTS_DIR
#   Qt5_CMAKE_DIR
#   
################################################################
IF(CMAKE_VERSION VERSION_LESS 2.8.10)
    MESSAGE(FATAL_ERROR "Qt5 requires at least CMake version 2.8.10")
ENDIF()

IF(NOT DEFINED Qt5_FIND_COMPONENTS)
    MESSAGE(WARNING "Qt5 FIND_PACKAGE: No Componenets specified, just finding Widgets!")
    SET(Qt5_FIND_COMPONENTS "Widgets")
ENDIF()

#Check if qmake is present
FIND_PROGRAM(QMAKE_EXECUTABLE NAMES qmake)
if(${QMAKE_EXECUTABLE} MATCHES "/usr/local/cina/qt4/qt-4.8.6/bin/qmake")
    set(QMAKE_EXECUTABLE "/usr/local/cina/qt5/Qt5.5.0/5.5/gcc_64/bin/qmake")
endif()

IF(${QMAKE_EXECUTABLE} MATCHES "NOTFOUND$" )
    SET(_Qt5_NOTFOUND_MESSAGE "qmake installation NOT FOUND!\nMake sure QT5 is installed it's binaries are in search path\n")
ELSE()
    MESSAGE(STATUS "Using QMAKE at: ${QMAKE_EXECUTABLE}")
    EXECUTE_PROCESS(COMMAND ${QMAKE_EXECUTABLE} -query QT_VERSION COMMAND tr -d "\n" OUTPUT_VARIABLE Qt5_VERSION)
    EXECUTE_PROCESS(COMMAND ${QMAKE_EXECUTABLE} -query QT_INSTALL_PREFIX COMMAND tr -d "\n" OUTPUT_VARIABLE Qt5_DIR)
    EXECUTE_PROCESS(COMMAND ${QMAKE_EXECUTABLE} -query QT_INSTALL_LIBS COMMAND tr -d "\n" OUTPUT_VARIABLE Qt5_LIB_DIR)
    EXECUTE_PROCESS(COMMAND ${QMAKE_EXECUTABLE} -query QT_INSTALL_HEADERS COMMAND tr -d "\n" OUTPUT_VARIABLE Qt5_INCLUDE_DIR)
    EXECUTE_PROCESS(COMMAND ${QMAKE_EXECUTABLE} -query QT_INSTALL_PLUGINS COMMAND tr -d "\n" OUTPUT_VARIABLE Qt5_PLUGINS_DIR)
    EXECUTE_PROCESS(COMMAND ${QMAKE_EXECUTABLE} -query QT_INSTALL_IMPORTS COMMAND tr -d "\n" OUTPUT_VARIABLE Qt5_IMPORTS_DIR)
    SET(Qt5_CMAKE_DIR "${Qt5_LIB_DIR}/cmake")
    MESSAGE(STATUS "=======> Qt5 (Version ${Qt5_VERSION}) found at: ${Qt5_DIR} ")
ENDIF()

IF(NOT DEFINED _Qt5_NOTFOUND_MESSAGE)
    IF(${Qt5_VERSION} MATCHES "^5")
        FOREACH(MODULE ${Qt5_FIND_COMPONENTS})
            FIND_PACKAGE(Qt5${MODULE}
                            ${_Qt5_FIND_PARTS_QUIET}
                            ${_Qt5_FIND_PARTS_REQUIRED}
                            PATHS "${Qt5_CMAKE_DIR}/Qt5${MODULE}" NO_DEFAULT_PATH
                        )
            IF (NOT Qt5${MODULE}_FOUND)
                IF (Qt5_FIND_REQUIRED_${MODULE})
                    SET(_Qt5_NOTFOUND_MESSAGE "${_Qt5_NOTFOUND_MESSAGE}Failed to find Qt5 component \"${MODULE}\" config file at \"${Qt5_CMAKE_DIR}/Qt5${MODULE}/Qt5${MODULE}Config.cmake\"\n")
                ELSEIF (NOT Qt5_FIND_QUIETLY)
                    MESSAGE(WARNING "Failed to find Qt5 component \"${MODULE}\" config file at \"${Qt5_CMAKE_DIR}/Qt5${MODULE}/Qt5${MODULE}Config.cmake\"")
                ENDIF()
            ELSE()
                MESSAGE(STATUS "Found Qt5 component \"${MODULE}\" ")
            ENDIF()
        ENDFOREACH()
    ELSE()
        SET(_Qt5_NOTFOUND_MESSAGE "Version(${Qt5_VERSION}) found. Should be a version 5.x.x\n")
    ENDIF()
ENDIF()

IF (DEFINED _Qt5_NOTFOUND_MESSAGE)
    SET(Qt5_NOT_FOUND_MESSAGE "${_Qt5_NOTFOUND_MESSAGE}")
    MESSAGE(FATAL_ERROR "Qt5 NOT FOUND:\n${_Qt5_NOTFOUND_MESSAGE}")
    SET(Qt5_FOUND False)
ELSE()
    SET(Qt5_FOUND True)
ENDIF()
