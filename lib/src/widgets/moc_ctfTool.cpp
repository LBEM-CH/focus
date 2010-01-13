/****************************************************************************
** Meta object code from reading C++ file 'ctfTool.h'
**
** Created: Tue Dec 16 15:18:33 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/ctfTool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'ctfTool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_ctfTool[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      39,    9,    8,    8, 0x05,

 // slots: signature, parameters, type, tag, flags
      73,    8,    8,    8, 0x0a,
      88,    8,    8,    8, 0x0a,
      95,    8,    8,    8, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_ctfTool[] = {
    "ctfTool\0\0defocusX,defocusY,astigmatism\0"
    "defocusChanged(float,float,float)\0"
    "valueChanged()\0save()\0load()\0"
};

const QMetaObject ctfTool::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_ctfTool,
      qt_meta_data_ctfTool, 0 }
};

const QMetaObject *ctfTool::metaObject() const
{
    return &staticMetaObject;
}

void *ctfTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_ctfTool))
	return static_cast<void*>(const_cast< ctfTool*>(this));
    return QWidget::qt_metacast(_clname);
}

int ctfTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: defocusChanged((*reinterpret_cast< float(*)>(_a[1])),(*reinterpret_cast< float(*)>(_a[2])),(*reinterpret_cast< float(*)>(_a[3]))); break;
        case 1: valueChanged(); break;
        case 2: save(); break;
        case 3: load(); break;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void ctfTool::defocusChanged(float _t1, float _t2, float _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
