/****************************************************************************
** Meta object code from reading C++ file 'levelGroup.h'
**
** Created: Tue Dec 16 15:18:30 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/levelGroup.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'levelGroup.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_levelGroup[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      14,   12,   11,   11, 0x05,
      38,   32,   11,   11, 0x05,

 // slots: signature, parameters, type, tag, flags
      60,   12,   11,   11, 0x0a,
      80,   74,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_levelGroup[] = {
    "levelGroup\0\0v\0levelChanged(int)\0title\0"
    "titleChanged(QString)\0setTitle(int)\0"
    "level\0setLevel(int)\0"
};

const QMetaObject levelGroup::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_levelGroup,
      qt_meta_data_levelGroup, 0 }
};

const QMetaObject *levelGroup::metaObject() const
{
    return &staticMetaObject;
}

void *levelGroup::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_levelGroup))
	return static_cast<void*>(const_cast< levelGroup*>(this));
    return QWidget::qt_metacast(_clname);
}

int levelGroup::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: levelChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: titleChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 2: setTitle((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: setLevel((*reinterpret_cast< int(*)>(_a[1]))); break;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void levelGroup::levelChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void levelGroup::titleChanged(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
