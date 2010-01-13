/****************************************************************************
** Meta object code from reading C++ file 'resultsFile.h'
**
** Created: Mon Oct 5 13:42:14 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "resultsFile.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'resultsFile.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_resultsFile[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      13,   12,   12,   12, 0x05,

 // slots: signature, parameters, type, tag, flags
      27,   12,   22,   12, 0x0a,
      34,   12,   12,   12, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_resultsFile[] = {
    "resultsFile\0\0loaded()\0bool\0load()\0"
    "clear()\0"
};

const QMetaObject resultsFile::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_resultsFile,
      qt_meta_data_resultsFile, 0 }
};

const QMetaObject *resultsFile::metaObject() const
{
    return &staticMetaObject;
}

void *resultsFile::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_resultsFile))
        return static_cast<void*>(const_cast< resultsFile*>(this));
    return QObject::qt_metacast(_clname);
}

int resultsFile::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: loaded(); break;
        case 1: { bool _r = load();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 2: clear(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void resultsFile::loaded()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
