/****************************************************************************
** Meta object code from reading C++ file 'colorTool.h'
**
** Created: Mon Oct 27 20:27:12 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/colorTool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'colorTool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_colorTool[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      16,   11,   10,   10, 0x05,
      41,   34,   10,   10, 0x05,

 // slots: signature, parameters, type, tag, flags
      71,   60,   10,   10, 0x0a,
      99,   90,   10,   10, 0x0a,
     116,   10,   10,   10, 0x0a,
     127,   10,   10,   10, 0x0a,
     141,   10,   10,   10, 0x0a,
     155,   10,   10,   10, 0x0a,
     165,   10,   10,   10, 0x0a,
     174,   11,   10,   10, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_colorTool[] = {
    "colorTool\0\0show\0togglePhase(bool)\0"
    "invert\0toggleInvert(bool)\0brightness\0"
    "setBrightness(int)\0contrast\0"
    "setContrast(int)\0complete()\0completeMax()\0"
    "completeMin()\0rescale()\0invert()\0"
    "showPhase(bool)\0"
};

const QMetaObject colorTool::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_colorTool,
      qt_meta_data_colorTool, 0 }
};

const QMetaObject *colorTool::metaObject() const
{
    return &staticMetaObject;
}

void *colorTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_colorTool))
	return static_cast<void*>(const_cast< colorTool*>(this));
    return QDialog::qt_metacast(_clname);
}

int colorTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: togglePhase((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: toggleInvert((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: setBrightness((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: setContrast((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: complete(); break;
        case 5: completeMax(); break;
        case 6: completeMin(); break;
        case 7: rescale(); break;
        case 8: invert(); break;
        case 9: showPhase((*reinterpret_cast< bool(*)>(_a[1]))); break;
        }
        _id -= 10;
    }
    return _id;
}

// SIGNAL 0
void colorTool::togglePhase(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void colorTool::toggleInvert(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
