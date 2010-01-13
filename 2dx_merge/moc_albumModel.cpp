/****************************************************************************
** Meta object code from reading C++ file 'albumModel.h'
**
** Created: Thu Oct 16 09:54:22 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "albumModel.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'albumModel.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_albumModel[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      12,   11,   11,   11, 0x0a,
      20,   11,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_albumModel[] = {
    "albumModel\0\0clear()\0reload()\0"
};

const QMetaObject albumModel::staticMetaObject = {
    { &QAbstractListModel::staticMetaObject, qt_meta_stringdata_albumModel,
      qt_meta_data_albumModel, 0 }
};

const QMetaObject *albumModel::metaObject() const
{
    return &staticMetaObject;
}

void *albumModel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_albumModel))
	return static_cast<void*>(const_cast< albumModel*>(this));
    return QAbstractListModel::qt_metacast(_clname);
}

int albumModel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QAbstractListModel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: clear(); break;
        case 1: reload(); break;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
