/****************************************************************************
** Meta object code from reading C++ file 'resultsData.h'
**
** Created: Thu Oct 16 09:54:15 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "resultsData.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'resultsData.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_resultsData[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      24,   13,   12,   12, 0x05,
      37,   13,   12,   12, 0x05,

 // slots: signature, parameters, type, tag, flags
      63,   54,   49,   12, 0x0a,
      77,   12,   49,   12, 0x0a,
      84,   12,   49,   12, 0x0a,
      97,   91,   12,   12, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_resultsData[] = {
    "resultsData\0\0successful\0loaded(bool)\0"
    "saved(bool)\0bool\0fileName\0load(QString)\0"
    "load()\0save()\0value\0setDryRunMode(bool)\0"
};

const QMetaObject resultsData::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_resultsData,
      qt_meta_data_resultsData, 0 }
};

const QMetaObject *resultsData::metaObject() const
{
    return &staticMetaObject;
}

void *resultsData::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_resultsData))
	return static_cast<void*>(const_cast< resultsData*>(this));
    return QObject::qt_metacast(_clname);
}

int resultsData::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: loaded((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: saved((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: { bool _r = load((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 3: { bool _r = load();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 4: { bool _r = save();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 5: setDryRunMode((*reinterpret_cast< bool(*)>(_a[1]))); break;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void resultsData::loaded(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void resultsData::saved(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
