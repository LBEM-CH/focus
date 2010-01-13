/****************************************************************************
** Meta object code from reading C++ file 'zoomWindow.h'
**
** Created: Mon Oct 27 20:27:11 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/zoomWindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'zoomWindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_zoomWindow[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      16,   12,   11,   11, 0x05,
      34,   12,   11,   11, 0x05,
      58,   12,   11,   11, 0x05,

 // slots: signature, parameters, type, tag, flags
      75,   12,   11,   11, 0x0a,
      88,   11,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_zoomWindow[] = {
    "zoomWindow\0\0pos\0zoomClick(QPoint)\0"
    "zoomDoubleClick(QPoint)\0zoomMove(QPoint)\0"
    "zoom(QPoint)\0zoom()\0"
};

const QMetaObject zoomWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_zoomWindow,
      qt_meta_data_zoomWindow, 0 }
};

const QMetaObject *zoomWindow::metaObject() const
{
    return &staticMetaObject;
}

void *zoomWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_zoomWindow))
	return static_cast<void*>(const_cast< zoomWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int zoomWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: zoomClick((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 1: zoomDoubleClick((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 2: zoomMove((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 3: zoom((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 4: zoom(); break;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void zoomWindow::zoomClick(const QPoint & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void zoomWindow::zoomDoubleClick(const QPoint & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void zoomWindow::zoomMove(const QPoint & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
QT_END_MOC_NAMESPACE
