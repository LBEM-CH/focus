/****************************************************************************
** Meta object code from reading C++ file 'warningBox.h'
**
** Created: Wed Jan 20 11:57:49 2010
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "warningBox.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'warningBox.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_warningBox[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      12,   11,   11,   11, 0x0a,
      28,   19,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_warningBox[] = {
    "warningBox\0\0load()\0fileName\0load(QString)\0"
};

const QMetaObject warningBox::staticMetaObject = {
    { &QFrame::staticMetaObject, qt_meta_stringdata_warningBox,
      qt_meta_data_warningBox, 0 }
};

const QMetaObject *warningBox::metaObject() const
{
    return &staticMetaObject;
}

void *warningBox::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_warningBox))
        return static_cast<void*>(const_cast< warningBox*>(this));
    return QFrame::qt_metacast(_clname);
}

int warningBox::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: load(); break;
        case 1: load((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
