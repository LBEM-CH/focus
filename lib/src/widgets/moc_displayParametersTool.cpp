/****************************************************************************
** Meta object code from reading C++ file 'displayParametersTool.h'
**
** Created: Tue Dec 16 15:18:33 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/displayParametersTool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'displayParametersTool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_displayParametersTool[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      18,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      28,   23,   22,   22, 0x05,
      59,   52,   22,   22, 0x05,
      85,   23,   22,   22, 0x05,
     106,   23,   22,   22, 0x05,
     139,  133,   22,   22, 0x05,
     169,  163,   22,   22, 0x05,
     197,  190,   22,   22, 0x05,
     248,  243,   22,   22, 0x05,

 // slots: signature, parameters, type, tag, flags
     275,  269,   22,   22, 0x0a,
     298,  269,   22,   22, 0x0a,
     323,  269,   22,   22, 0x0a,
     343,  269,   22,   22, 0x0a,
     375,  369,   22,   22, 0x0a,
     399,  190,   22,   22, 0x0a,
     423,   22,   22,   22, 0x0a,
     438,  431,   22,   22, 0x0a,
     457,   22,   22,   22, 0x0a,
     468,   22,   22,   22, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_displayParametersTool[] = {
    "displayParametersTool\0\0size\0"
    "latticeSizeChanged(int)\0orders\0"
    "latticeOrdersChanged(int)\0"
    "spotSizeChanged(int)\0refinementSizeChanged(int)\0"
    "range\0searchRangeChanged(int)\0sigma\0"
    "sigmaChanged(double)\0method\0"
    "searchMethodChanged(mrcImage::maxValueMethod)\0"
    "view\0viewFitChanged(bool)\0value\0"
    "changeLatticeSize(int)\0changeLatticeOrders(int)\0"
    "changeSpotSize(int)\0changeRefinementSize(int)\0"
    "state\0changeViewFitState(int)\0"
    "changeSearchMethod(int)\0flush()\0widget\0"
    "complete(QWidget*)\0complete()\0"
    "setDefaults()\0"
};

const QMetaObject displayParametersTool::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_displayParametersTool,
      qt_meta_data_displayParametersTool, 0 }
};

const QMetaObject *displayParametersTool::metaObject() const
{
    return &staticMetaObject;
}

void *displayParametersTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_displayParametersTool))
	return static_cast<void*>(const_cast< displayParametersTool*>(this));
    return QWidget::qt_metacast(_clname);
}

int displayParametersTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: latticeSizeChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: latticeOrdersChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: spotSizeChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: refinementSizeChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: searchRangeChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: sigmaChanged((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 6: searchMethodChanged((*reinterpret_cast< mrcImage::maxValueMethod(*)>(_a[1]))); break;
        case 7: viewFitChanged((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 8: changeLatticeSize((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 9: changeLatticeOrders((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 10: changeSpotSize((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 11: changeRefinementSize((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 12: changeViewFitState((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 13: changeSearchMethod((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 14: flush(); break;
        case 15: complete((*reinterpret_cast< QWidget*(*)>(_a[1]))); break;
        case 16: complete(); break;
        case 17: setDefaults(); break;
        }
        _id -= 18;
    }
    return _id;
}

// SIGNAL 0
void displayParametersTool::latticeSizeChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void displayParametersTool::latticeOrdersChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void displayParametersTool::spotSizeChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void displayParametersTool::refinementSizeChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void displayParametersTool::searchRangeChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void displayParametersTool::sigmaChanged(double _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void displayParametersTool::searchMethodChanged(mrcImage::maxValueMethod _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void displayParametersTool::viewFitChanged(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}
QT_END_MOC_NAMESPACE
