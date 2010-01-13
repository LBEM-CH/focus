/****************************************************************************
** Meta object code from reading C++ file 'imageViewer.h'
**
** Created: Tue Dec 16 15:18:32 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/imageViewer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'imageViewer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_imageViewerPrototype[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      35,   21,   22,   21, 0x0a,
      44,   21,   21,   21, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_imageViewerPrototype[] = {
    "imageViewerPrototype\0\0QScriptValue\0"
    "create()\0addToWindow()\0"
};

const QMetaObject imageViewerPrototype::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_imageViewerPrototype,
      qt_meta_data_imageViewerPrototype, 0 }
};

const QMetaObject *imageViewerPrototype::metaObject() const
{
    return &staticMetaObject;
}

void *imageViewerPrototype::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_imageViewerPrototype))
	return static_cast<void*>(const_cast< imageViewerPrototype*>(this));
    if (!strcmp(_clname, "QScriptable"))
	return static_cast< QScriptable*>(const_cast< imageViewerPrototype*>(this));
    return QObject::qt_metacast(_clname);
}

int imageViewerPrototype::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: { QScriptValue _r = create();
            if (_a[0]) *reinterpret_cast< QScriptValue*>(_a[0]) = _r; }  break;
        case 1: addToWindow(); break;
        }
        _id -= 2;
    }
    return _id;
}
static const uint qt_meta_data_imageViewer[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      13,   12,   12,   12, 0x0a,
      20,   12,   12,   12, 0x0a,
      29,   12,   12,   12, 0x0a,
      41,   12,   12,   12, 0x0a,
      50,   12,   12,   12, 0x0a,
      65,   60,   12,   12, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_imageViewer[] = {
    "imageViewer\0\0hide()\0center()\0resetView()\0"
    "zoomIn()\0zoomOut()\0tool\0addTool(abstractTool*)\0"
};

const QMetaObject imageViewer::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_imageViewer,
      qt_meta_data_imageViewer, 0 }
};

const QMetaObject *imageViewer::metaObject() const
{
    return &staticMetaObject;
}

void *imageViewer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_imageViewer))
	return static_cast<void*>(const_cast< imageViewer*>(this));
    return QWidget::qt_metacast(_clname);
}

int imageViewer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: hide(); break;
        case 1: center(); break;
        case 2: resetView(); break;
        case 3: zoomIn(); break;
        case 4: zoomOut(); break;
        case 5: addTool((*reinterpret_cast< abstractTool*(*)>(_a[1]))); break;
        }
        _id -= 6;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
