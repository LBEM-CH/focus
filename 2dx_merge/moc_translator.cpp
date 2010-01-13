/****************************************************************************
** Meta object code from reading C++ file 'translator.h'
**
** Created: Thu Oct 16 09:54:18 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "translator.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'translator.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_translator[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      21,   12,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_translator[] = {
    "translator\0\0fileName\0open(QString)\0"
};

const QMetaObject translator::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_translator,
      qt_meta_data_translator, 0 }
};

const QMetaObject *translator::metaObject() const
{
    return &staticMetaObject;
}

void *translator::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_translator))
	return static_cast<void*>(const_cast< translator*>(this));
    return QObject::qt_metacast(_clname);
}

int translator::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: open((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
