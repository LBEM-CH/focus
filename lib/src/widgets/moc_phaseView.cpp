/****************************************************************************
** Meta object code from reading C++ file 'phaseView.h'
**
** Created: Mon Oct 27 20:27:09 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/phaseView.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'phaseView.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_phaseView[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      17,   11,   10,   10, 0x0a,
      39,   33,   10,   10, 0x0a,
      57,   52,   10,   10, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_phaseView[] = {
    "phaseView\0\0theta\0setPhase(float)\0value\0"
    "invert(bool)\0view\0show(bool)\0"
};

const QMetaObject phaseView::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_phaseView,
      qt_meta_data_phaseView, 0 }
};

const QMetaObject *phaseView::metaObject() const
{
    return &staticMetaObject;
}

void *phaseView::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_phaseView))
	return static_cast<void*>(const_cast< phaseView*>(this));
    return QWidget::qt_metacast(_clname);
}

int phaseView::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setPhase((*reinterpret_cast< float(*)>(_a[1]))); break;
        case 1: invert((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: show((*reinterpret_cast< bool(*)>(_a[1]))); break;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
