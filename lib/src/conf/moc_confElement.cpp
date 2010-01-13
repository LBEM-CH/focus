/****************************************************************************
** Meta object code from reading C++ file 'confElement.h'
**
** Created: Thu Oct 16 09:51:46 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/confElement.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'confElement.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_confElement[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      13,   12,   12,   12, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_confElement[] = {
    "confElement\0\0dataChanged()\0"
};

const QMetaObject confElement::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_confElement,
      qt_meta_data_confElement, 0 }
};

const QMetaObject *confElement::metaObject() const
{
    return &staticMetaObject;
}

void *confElement::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_confElement))
	return static_cast<void*>(const_cast< confElement*>(this));
    return QObject::qt_metacast(_clname);
}

int confElement::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: dataChanged(); break;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void confElement::dataChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
