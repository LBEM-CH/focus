DESTDIR += $$TOP_SRCDIR/lib/common
INCLUDEPATH += $$TOP_SRCDIR/include

CONFIG += foo

staticlib:QMAKE_CLEAN += $(DESTDIR)/$(TARGET)

contains(TEMPLATE,app) {
  QMAKE_CLEAN += $(TARGET)
  macx {
#    ICON = "./resource/icon_OSX.icns"
  }

	contains(CONFIG,STATIC2DX)
	{
		linux-g++ {
#			QTPLUGIN += qjpeg qgif
			LIBS+=-ljpeg
#      DEFINES += _USE_STATIC_PLUGINS_
		}
	}

}

