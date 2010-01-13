/****************************************************************************
** Meta object code from reading C++ file 'selectionFFT.h'
**
** Created: Mon Oct 27 20:27:11 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/selectionFFT.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'selectionFFT.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_selectionFFT[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      14,   13,   13,   13, 0x0a,
      29,   13,   13,   13, 0x0a,
      44,   13,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_selectionFFT[] = {
    "selectionFFT\0\0increaseZoom()\0"
    "decreaseZoom()\0zoomStandard()\0"
};

const QMetaObject selectionFFT::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_selectionFFT,
      qt_meta_data_selectionFFT, 0 }
};

const QMetaObject *selectionFFT::metaObject() const
{
    return &staticMetaObject;
}

void *selectionFFT::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_selectionFFT))
	return static_cast<void*>(const_cast< selectionFFT*>(this));
    return QWidget::qt_metacast(_clname);
}

int selectionFFT::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: increaseZoom(); break;
        case 1: decreaseZoom(); break;
        case 2: zoomStandard(); break;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
