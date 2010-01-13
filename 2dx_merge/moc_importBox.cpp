/****************************************************************************
** Meta object code from reading C++ file 'importBox.h'
**
** Created: Thu Oct 16 09:54:26 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "importBox.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'importBox.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_importBox[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      22,   11,   10,   10, 0x05,

 // slots: signature, parameters, type, tag, flags
      60,   10,   10,   10, 0x0a,
      74,   10,   10,   10, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_importBox[] = {
    "importBox\0\0imageCodes\0"
    "acceptedImage(QHash<QString,QString>)\0"
    "acceptImage()\0accept()\0"
};

const QMetaObject importBox::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_importBox,
      qt_meta_data_importBox, 0 }
};

const QMetaObject *importBox::metaObject() const
{
    return &staticMetaObject;
}

void *importBox::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_importBox))
	return static_cast<void*>(const_cast< importBox*>(this));
    return QDialog::qt_metacast(_clname);
}

int importBox::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: acceptedImage((*reinterpret_cast< const QHash<QString,QString>(*)>(_a[1]))); break;
        case 1: acceptImage(); break;
        case 2: accept(); break;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void importBox::acceptedImage(const QHash<QString,QString> & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
