#
# Autoconf file for 2dx
#  Thu Feb  1 11:32:50 CET 2007 -- Remco Wouts
#     started
#
AC_INIT(2dx, 3.1.0)
AC_PREFIX_DEFAULT([/usr/local/2dx])
AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC
AC_PROG_INSTALL

#AC_DEFUN([AC_PROG_QMAKE],[
if test x"$QMAKE" == x""; then
	AC_PATH_PROG(QMAKE, qmake-qt4)
  if test x"$QMAKE" == x""; then
    AC_PATH_PROG(QMAKE,qmake) 
  fi
fi
AC_SUBST(QMAKE,"$QMAKE")
#])

#AC_PROG_QMAKE

#
# Some macros so we can use qmake
#
# We can use AC_CONFIG_QMAKE much like the normal AC_CONFIG_... macros
# 
AC_DEFUN([concat],[m4_if($#,0,,$#,1,$1,[concat([$1 ]m4_shift($@))])])
AC_DEFUN([qproject],
         [(if test x"$QMAKE" == x""; then QMAKE="qmake"; fi; cd $1; $QMAKE -project concat(m4_shift($@)); $QMAKE -config release)])
AC_DEFUN([AC_CONFIG_QPRO],
         [AC_CONFIG_COMMANDS($1/Makefile,qproject($@))
	  AC_SUBST(qsubdirs,["$1 $qsubdirs"])])
AC_DEFUN([AC_CONFIG_QMAKE],
         [AC_CONFIG_COMMANDS($1/Makefile,[(cd $1; if test x"$QMAKE" == x""; then QMAKE="qmake"; fi; $QMAKE -config release qmake.pro)])
	  AC_SUBST(qsubdirs,["$qsubdirs $1"])])

#
# Generate the config stuff
#
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([.qmake.cache])
AC_CONFIG_QPRO([./lib/src],
	       [-t subdirs -nopwd -after "SUBDIRS += conf widgets mrcImage extentions"])
AC_CONFIG_QMAKE([./lib/src/conf])
AC_CONFIG_QMAKE([./lib/src/widgets])
AC_CONFIG_QMAKE([./lib/src/mrcImage])
AC_CONFIG_QMAKE([./lib/src/extentions])
AC_CONFIG_QMAKE([./2dx_image])
AC_CONFIG_QMAKE([./2dx_merge])
AC_CONFIG_QMAKE([./2dx_logbrowser])

AC_CONFIG_SUBDIRS([./kernel])

# Required
AC_OUTPUT


