/****************************************************************************
** Meta object code from reading C++ file 'mrcImage.h'
**
** Created: Thu Oct 16 09:53:19 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/mrcImage.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'mrcImage.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_mrcImage[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      10,    9,    9,    9, 0x05,
      31,   25,    9,    9, 0x05,

 // slots: signature, parameters, type, tag, flags
      48,   25,    9,    9, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_mrcImage[] = {
    "mrcImage\0\0showProgress()\0value\0"
    "setProgress(int)\0setViewPhase(bool)\0"
};

const QMetaObject mrcImage::staticMetaObject = {
    { &QThread::staticMetaObject, qt_meta_stringdata_mrcImage,
      qt_meta_data_mrcImage, 0 }
};

const QMetaObject *mrcImage::metaObject() const
{
    return &staticMetaObject;
}

void *mrcImage::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_mrcImage))
	return static_cast<void*>(const_cast< mrcImage*>(this));
    return QThread::qt_metacast(_clname);
}

int mrcImage::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QThread::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: showProgress(); break;
        case 1: setProgress((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: setViewPhase((*reinterpret_cast< bool(*)>(_a[1]))); break;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void mrcImage::showProgress()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void mrcImage::setProgress(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
