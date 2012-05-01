############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#

file(GLOB install_files *.h)
install(FILES ${install_files} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)

configure_file("confVersion.h.in" "confVersion.h")

