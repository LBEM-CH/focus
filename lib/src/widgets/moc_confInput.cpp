/****************************************************************************
** Meta object code from reading C++ file 'confInput.h'
**
** Created: Tue Dec 16 15:18:30 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/confInput.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'confInput.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_confInput[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       9,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      11,   10,   10,   10, 0x05,

 // slots: signature, parameters, type, tag, flags
      19,   10,   10,   10, 0x0a,
      26,   10,   10,   10, 0x0a,
      39,   33,   10,   10, 0x0a,
      61,   10,   10,   10, 0x0a,
      76,   10,   10,   10, 0x0a,
      93,   10,   10,   10, 0x0a,
     108,   10,   10,   10, 0x0a,
     126,   10,   10,   10, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_confInput[] = {
    "confInput\0\0shown()\0save()\0load()\0state\0"
    "setReadOnlyState(int)\0dataModified()\0"
    "updateFontInfo()\0updateStatus()\0"
    "updateWhatsThis()\0show()\0"
};

const QMetaObject confInput::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_confInput,
      qt_meta_data_confInput, 0 }
};

const QMetaObject *confInput::metaObject() const
{
    return &staticMetaObject;
}

void *confInput::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_confInput))
	return static_cast<void*>(const_cast< confInput*>(this));
    return QWidget::qt_metacast(_clname);
}

int confInput::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: shown(); break;
        case 1: save(); break;
        case 2: load(); break;
        case 3: setReadOnlyState((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: dataModified(); break;
        case 5: updateFontInfo(); break;
        case 6: updateStatus(); break;
        case 7: updateWhatsThis(); break;
        case 8: show(); break;
        }
        _id -= 9;
    }
    return _id;
}

// SIGNAL 0
void confInput::shown()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
