/****************************************************************************
** Meta object code from reading C++ file 'navigator.h'
**
** Created: Wed Jan 20 11:57:41 2010
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "navigator.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'navigator.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_navigator[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      11,   10,   10,   10, 0x0a,
      33,   20,   10,   10, 0x0a,
      73,   65,   10,   10, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_navigator[] = {
    "navigator\0\0center()\0item,visible\0"
    "toggleItem(QGraphicsItem*,bool)\0visible\0"
    "toggleLattice(bool)\0"
};

const QMetaObject navigator::staticMetaObject = {
    { &QGraphicsView::staticMetaObject, qt_meta_stringdata_navigator,
      qt_meta_data_navigator, 0 }
};

const QMetaObject *navigator::metaObject() const
{
    return &staticMetaObject;
}

void *navigator::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_navigator))
        return static_cast<void*>(const_cast< navigator*>(this));
    return QGraphicsView::qt_metacast(_clname);
}

int navigator::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QGraphicsView::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: center(); break;
        case 1: toggleItem((*reinterpret_cast< QGraphicsItem*(*)>(_a[1])),(*reinterpret_cast< bool(*)>(_a[2]))); break;
        case 2: toggleLattice((*reinterpret_cast< bool(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
