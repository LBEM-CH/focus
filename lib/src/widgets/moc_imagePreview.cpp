/****************************************************************************
** Meta object code from reading C++ file 'imagePreview.h'
**
** Created: Tue Dec 16 15:18:32 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/imagePreview.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'imagePreview.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_imagePreview[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       9,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      20,   14,   13,   13, 0x05,
      37,   13,   13,   13, 0x05,

 // slots: signature, parameters, type, tag, flags
      54,   44,   13,   13, 0x0a,
      72,   13,   13,   13, 0x0a,
      89,   13,   13,   13, 0x0a,
      97,   13,   13,   13, 0x0a,
     110,   13,   13,   13, 0x0a,
     127,   13,   13,   13, 0x0a,
     152,  145,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_imagePreview[] = {
    "imagePreview\0\0value\0setProgress(int)\0"
    "load()\0imageName\0setImage(QString)\0"
    "clearNavigator()\0shade()\0toggleInfo()\0"
    "progressDialog()\0launchNavigator()\0"
    "enable\0enableNewViewer(bool)\0"
};

const QMetaObject imagePreview::staticMetaObject = {
    { &QFrame::staticMetaObject, qt_meta_stringdata_imagePreview,
      qt_meta_data_imagePreview, 0 }
};

const QMetaObject *imagePreview::metaObject() const
{
    return &staticMetaObject;
}

void *imagePreview::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_imagePreview))
	return static_cast<void*>(const_cast< imagePreview*>(this));
    return QFrame::qt_metacast(_clname);
}

int imagePreview::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setProgress((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: load(); break;
        case 2: setImage((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: clearNavigator(); break;
        case 4: shade(); break;
        case 5: toggleInfo(); break;
        case 6: progressDialog(); break;
        case 7: launchNavigator(); break;
        case 8: enableNewViewer((*reinterpret_cast< bool(*)>(_a[1]))); break;
        }
        _id -= 9;
    }
    return _id;
}

// SIGNAL 0
void imagePreview::setProgress(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void imagePreview::load()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
