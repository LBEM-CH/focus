set(CMAKE_EXECUTABLE_SUFFIX ".exe")

add_executable(TestRunner_Terminal TestRunner2.cpp)
target_link_libraries(TestRunner_Terminal ${CMAKE_CURRENT_BINARY_DIR}/../libSingleParticle2dx.a)
target_link_libraries(TestRunner_Terminal ${FFTWF_LIB})
target_link_libraries(TestRunner_Terminal ${CPPUNIT_LIBRARY})
target_link_libraries( TestRunner_Terminal ${Boost_LIBRARIES})

add_executable(TestRunner_Jenkins TestRunner.cpp)
target_link_libraries(TestRunner_Jenkins ${CMAKE_CURRENT_BINARY_DIR}/../libSingleParticle2dx.a)
target_link_libraries(TestRunner_Jenkins ${FFTWF_LIB})
target_link_libraries(TestRunner_Jenkins ${CPPUNIT_LIBRARY})
target_link_libraries(TestRunner_Jenkins ${Boost_LIBRARIES})

if(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR ${INSTALL_SRC_DIR}/${DIRECTORY})
else(INSTALL_SRC_DIR)
	set(INSTALL_SRC_DIR kernel/mrc/${DIRECTORY})
endif(INSTALL_SRC_DIR)

set(INSTALL_BIN_DIR ${CMAKE_INSTALL_PREFIX}/kernel/mrc/bin/2dx_singleparticle)

install(TARGETS TestRunner_Terminal DESTINATION ${INSTALL_BIN_DIR})
install(TARGETS TestRunner_Jenkins DESTINATION ${INSTALL_BIN_DIR})

file(GLOB TEST_MRC *.mrc)
install(FILES ${TEST_MRC} DESTINATION ${INSTALL_BIN_DIR})

file(GLOB FILE_CONFIG *.cfg)
install(FILES ${FILE_CONFIG} DESTINATION ${INSTALL_BIN_DIR})

file(GLOB FILE_TXT *.txt)
install(FILES ${FILE_TXT} DESTINATION ${INSTALL_BIN_DIR})

# sources
file(GLOB SOURCES *.hpp *.cpp)
install(FILES ${SOURCES} DESTINATION ${INSTALL_SRC_DIR})









