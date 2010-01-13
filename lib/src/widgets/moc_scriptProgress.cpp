/****************************************************************************
** Meta object code from reading C++ file 'scriptProgress.h'
**
** Created: Mon Oct 27 20:27:07 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/scriptProgress.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'scriptProgress.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_scriptProgress[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      21,   16,   15,   15, 0x0a,
      43,   38,   15,   15, 0x0a,
      88,   84,   15,   15, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_scriptProgress[] = {
    "scriptProgress\0\0text\0setText(QString)\0"
    "type\0setProgressType(scriptProgress::barType)\0"
    "inc\0incrementValue(int)\0"
};

const QMetaObject scriptProgress::staticMetaObject = {
    { &QProgressBar::staticMetaObject, qt_meta_stringdata_scriptProgress,
      qt_meta_data_scriptProgress, 0 }
};

const QMetaObject *scriptProgress::metaObject() const
{
    return &staticMetaObject;
}

void *scriptProgress::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_scriptProgress))
	return static_cast<void*>(const_cast< scriptProgress*>(this));
    return QProgressBar::qt_metacast(_clname);
}

int scriptProgress::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QProgressBar::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setText((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 1: setProgressType((*reinterpret_cast< scriptProgress::barType(*)>(_a[1]))); break;
        case 2: incrementValue((*reinterpret_cast< int(*)>(_a[1]))); break;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
