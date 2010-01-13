/****************************************************************************
** Meta object code from reading C++ file 'abstractTool.h'
**
** Created: Mon Oct 27 23:26:18 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/abstractTool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'abstractTool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_abstractTool[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      21,   14,   13,   13, 0x0a,
      49,   41,   13,   13, 0x0a,
      66,   13,   13,   13, 0x2a,
      79,   41,   13,   13, 0x0a,
      96,   13,   13,   13, 0x2a,
     114,  109,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_abstractTool[] = {
    "abstractTool\0\0action\0addAction(QAction*)\0"
    "enabled\0setEnabled(bool)\0setEnabled()\0"
    "setVisible(bool)\0setVisible()\0item\0"
    "addGraphicsItem(QGraphicsItem*)\0"
};

const QMetaObject abstractTool::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_abstractTool,
      qt_meta_data_abstractTool, 0 }
};

const QMetaObject *abstractTool::metaObject() const
{
    return &staticMetaObject;
}

void *abstractTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_abstractTool))
	return static_cast<void*>(const_cast< abstractTool*>(this));
    return QObject::qt_metacast(_clname);
}

int abstractTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: addAction((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        case 1: setEnabled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: setEnabled(); break;
        case 3: setVisible((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 4: setVisible(); break;
        case 5: addGraphicsItem((*reinterpret_cast< QGraphicsItem*(*)>(_a[1]))); break;
        }
        _id -= 6;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
