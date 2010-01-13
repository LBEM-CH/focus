/****************************************************************************
** Meta object code from reading C++ file 'imageAlbum.h'
**
** Created: Thu Oct 16 09:54:21 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "imageAlbum.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'imageAlbum.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_imageAlbum[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      22,   12,   11,   11, 0x05,
      54,   45,   11,   11, 0x05,

 // slots: signature, parameters, type, tag, flags
      82,   76,   11,   11, 0x0a,
     105,   76,   11,   11, 0x0a,
     132,  126,   11,   11, 0x0a,
     177,  162,   11,   11, 0x0a,
     234,  217,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_imageAlbum[] = {
    "imageAlbum\0\0imageName\0imageSelected(QString)\0"
    "confFile\0confSelected(QString)\0index\0"
    "viewImage(QModelIndex)\0setConf(QModelIndex)\0"
    "model\0setModel(QAbstractItemModel*)\0"
    "selectionModel\0setSelectionModel(QItemSelectionModel*)\0"
    "current,previous\0"
    "currentSelectionChanged(QModelIndex,QModelIndex)\0"
};

const QMetaObject imageAlbum::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_imageAlbum,
      qt_meta_data_imageAlbum, 0 }
};

const QMetaObject *imageAlbum::metaObject() const
{
    return &staticMetaObject;
}

void *imageAlbum::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_imageAlbum))
	return static_cast<void*>(const_cast< imageAlbum*>(this));
    return QWidget::qt_metacast(_clname);
}

int imageAlbum::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: imageSelected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: confSelected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 2: viewImage((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 3: setConf((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 4: setModel((*reinterpret_cast< QAbstractItemModel*(*)>(_a[1]))); break;
        case 5: setSelectionModel((*reinterpret_cast< QItemSelectionModel*(*)>(_a[1]))); break;
        case 6: currentSelectionChanged((*reinterpret_cast< const QModelIndex(*)>(_a[1])),(*reinterpret_cast< const QModelIndex(*)>(_a[2]))); break;
        }
        _id -= 7;
    }
    return _id;
}

// SIGNAL 0
void imageAlbum::imageSelected(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void imageAlbum::confSelected(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
