############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#

file(GLOB install_files *.h)
install(FILES ${install_files} DESTINATION include)

configure_file(confVersion.h.in ${CMAKE_CURRENT_BINARY_DIR}/confVersion.h)



