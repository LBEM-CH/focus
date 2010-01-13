/****************************************************************************
** Meta object code from reading C++ file 'progressStamps.h'
**
** Created: Mon Oct 5 13:42:13 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "progressStamps.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'progressStamps.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_progressStamps[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      25,   16,   15,   15, 0x0a,
      36,   15,   15,   15, 0x2a,

       0        // eod
};

static const char qt_meta_stringdata_progressStamps[] = {
    "progressStamps\0\0modified\0load(bool)\0"
    "load()\0"
};

const QMetaObject progressStamps::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_progressStamps,
      qt_meta_data_progressStamps, 0 }
};

const QMetaObject *progressStamps::metaObject() const
{
    return &staticMetaObject;
}

void *progressStamps::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_progressStamps))
        return static_cast<void*>(const_cast< progressStamps*>(this));
    return QWidget::qt_metacast(_clname);
}

int progressStamps::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: load((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: load(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
