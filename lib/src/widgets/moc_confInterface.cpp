/****************************************************************************
** Meta object code from reading C++ file 'confInterface.h'
**
** Created: Tue Dec 16 15:18:30 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/confInterface.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'confInterface.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_confInterface[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      13,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      15,   14,   14,   14, 0x05,
      25,   14,   14,   14, 0x05,
      35,   14,   14,   14, 0x05,

 // slots: signature, parameters, type, tag, flags
      53,   14,   14,   14, 0x0a,
      60,   14,   14,   14, 0x0a,
      67,   14,   14,   14, 0x0a,
      83,   77,   14,   14, 0x0a,
     101,   77,   14,   14, 0x0a,
     134,  128,   14,   14, 0x0a,
     149,   14,   14,   14, 0x2a,
     160,   14,   14,   14, 0x0a,
     177,   14,   14,   14, 0x0a,
     187,   14,   14,   14, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_confInterface[] = {
    "confInterface\0\0saveAll()\0loadAll()\0"
    "fontInfoUpdated()\0save()\0load()\0"
    "execute()\0level\0setUserLevel(int)\0"
    "setSelectionUserLevel(int)\0value\0"
    "reselect(bool)\0reselect()\0updateFontInfo()\0"
    "hideAll()\0openAllSections()\0"
};

const QMetaObject confInterface::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_confInterface,
      qt_meta_data_confInterface, 0 }
};

const QMetaObject *confInterface::metaObject() const
{
    return &staticMetaObject;
}

void *confInterface::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_confInterface))
	return static_cast<void*>(const_cast< confInterface*>(this));
    return QWidget::qt_metacast(_clname);
}

int confInterface::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: saveAll(); break;
        case 1: loadAll(); break;
        case 2: fontInfoUpdated(); break;
        case 3: save(); break;
        case 4: load(); break;
        case 5: execute(); break;
        case 6: setUserLevel((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 7: setSelectionUserLevel((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 8: reselect((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 9: reselect(); break;
        case 10: updateFontInfo(); break;
        case 11: hideAll(); break;
        case 12: openAllSections(); break;
        }
        _id -= 13;
    }
    return _id;
}

// SIGNAL 0
void confInterface::saveAll()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void confInterface::loadAll()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void confInterface::fontInfoUpdated()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
