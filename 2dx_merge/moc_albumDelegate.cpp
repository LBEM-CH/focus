/****************************************************************************
** Meta object code from reading C++ file 'albumDelegate.h'
**
** Created: Thu Oct 16 09:54:24 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "albumDelegate.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'albumDelegate.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_albumDelegate[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets

       0        // eod
};

static const char qt_meta_stringdata_albumDelegate[] = {
    "albumDelegate\0"
};

const QMetaObject albumDelegate::staticMetaObject = {
    { &QItemDelegate::staticMetaObject, qt_meta_stringdata_albumDelegate,
      qt_meta_data_albumDelegate, 0 }
};

const QMetaObject *albumDelegate::metaObject() const
{
    return &staticMetaObject;
}

void *albumDelegate::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_albumDelegate))
	return static_cast<void*>(const_cast< albumDelegate*>(this));
    return QItemDelegate::qt_metacast(_clname);
}

int albumDelegate::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QItemDelegate::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
