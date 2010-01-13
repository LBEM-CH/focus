/****************************************************************************
** Meta object code from reading C++ file 'mrcGraphicsItem.h'
**
** Created: Mon Oct 27 20:27:10 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/mrcGraphicsItem.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'mrcGraphicsItem.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_mrcGraphicsItem[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      17,   16,   16,   16, 0x05,
      29,   24,   16,   16, 0x05,

 // slots: signature, parameters, type, tag, flags
      45,   16,   16,   16, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_mrcGraphicsItem[] = {
    "mrcGraphicsItem\0\0load()\0rect\0"
    "updated(QRectF)\0loadPixmap()\0"
};

const QMetaObject mrcGraphicsItem::staticMetaObject = {
    { &QThread::staticMetaObject, qt_meta_stringdata_mrcGraphicsItem,
      qt_meta_data_mrcGraphicsItem, 0 }
};

const QMetaObject *mrcGraphicsItem::metaObject() const
{
    return &staticMetaObject;
}

void *mrcGraphicsItem::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_mrcGraphicsItem))
	return static_cast<void*>(const_cast< mrcGraphicsItem*>(this));
    if (!strcmp(_clname, "QGraphicsPixmapItem"))
	return static_cast< QGraphicsPixmapItem*>(const_cast< mrcGraphicsItem*>(this));
    return QThread::qt_metacast(_clname);
}

int mrcGraphicsItem::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QThread::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: load(); break;
        case 1: updated((*reinterpret_cast< const QRectF(*)>(_a[1]))); break;
        case 2: loadPixmap(); break;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void mrcGraphicsItem::load()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void mrcGraphicsItem::updated(const QRectF & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
