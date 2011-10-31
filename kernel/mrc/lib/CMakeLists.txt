############################################################################
# A 2dx CMakeLists.txt                                                  #
############################################################################
# The author was:                                                          #
# 2010 Marcel Arheit                                                       #
############################################################################
#
cmake_minimum_required(VERSION 2.6)
get_filename_component(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} NAME)
if(INSTALL_DIR)
	set(INSTALL_DIR ${INSTALL_DIR}/${DIRECTORY})
else(INSTALL_DIR)
	set(INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/kernel/${DIRECTORY})
endif(INSTALL_DIR)
set (LIBS
	2dx_conv.cpp
	2dx_conv.o
	2dx_fftlib-2.for
	2dx_fftlib.for
	2dx_func.for
	ccplib.for
	configure.in
	diskio.for
	fftlib.cpp
	fftlib.o
	font84.dat
	fsplit
	fsplit.c
	ifftsub.for
	imsubs2000.for
	imsubs2.for
	imsubs_common.for
	lcflib.for
	lib2dx.a
	libconv.a
	libfft.a
	libgen.a
	libim2k.a
	libmisc.a
	libplot2k.a
	library.c
	library.h
	Makefile.in
	miscsubs.for
	mrcImage.cpp
	mrcImage.o
	mtzlib.for
	parser.for
	plot2k.c
	plot2k.o
	README
	subs.for
	symlib.for
	symop.lib
	unix.for
	unix.m4)
install(FILES ${LIBS} DESTINATION ${INSTALL_DIR})
add_subdirectory(old)
