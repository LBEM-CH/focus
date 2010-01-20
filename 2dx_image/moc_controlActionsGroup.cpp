/****************************************************************************
** Meta object code from reading C++ file 'controlActionsGroup.h'
**
** Created: Wed Jan 20 11:57:43 2010
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "controlActionsGroup.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'controlActionsGroup.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_controlActionsGroup[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      16,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      21,   20,   20,   20, 0x05,
      34,   20,   20,   20, 0x05,
      47,   20,   20,   20, 0x05,
      66,   20,   20,   20, 0x05,
      73,   20,   20,   20, 0x05,
      87,   20,   20,   20, 0x05,
      97,   20,   20,   20, 0x05,
     108,   20,   20,   20, 0x05,

 // slots: signature, parameters, type, tag, flags
     126,  120,   20,   20, 0x0a,
     147,  143,   20,   20, 0x0a,
     175,  170,   20,   20, 0x0a,
     192,   20,   20,   20, 0x0a,
     209,   20,   20,   20, 0x0a,
     233,  223,   20,   20, 0x0a,
     253,   20,   20,   20, 0x0a,
     275,  270,   20,   20, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_controlActionsGroup[] = {
    "controlActionsGroup\0\0hideWidget()\0"
    "toggleInfo()\0toggleManual(bool)\0save()\0"
    "execute(bool)\0refresh()\0viewHelp()\0"
    "reportBug()\0value\0setProgress(int)\0"
    "inc\0incrementProgress(int)\0text\0"
    "setText(QString)\0scriptFinished()\0"
    "saveClicked()\0available\0saveAvailable(bool)\0"
    "executeClicked()\0show\0setManual(bool)\0"
};

const QMetaObject controlActionsGroup::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_controlActionsGroup,
      qt_meta_data_controlActionsGroup, 0 }
};

const QMetaObject *controlActionsGroup::metaObject() const
{
    return &staticMetaObject;
}

void *controlActionsGroup::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_controlActionsGroup))
        return static_cast<void*>(const_cast< controlActionsGroup*>(this));
    return QWidget::qt_metacast(_clname);
}

int controlActionsGroup::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: hideWidget(); break;
        case 1: toggleInfo(); break;
        case 2: toggleManual((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 3: save(); break;
        case 4: execute((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 5: refresh(); break;
        case 6: viewHelp(); break;
        case 7: reportBug(); break;
        case 8: setProgress((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 9: incrementProgress((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 10: setText((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 11: scriptFinished(); break;
        case 12: saveClicked(); break;
        case 13: saveAvailable((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 14: executeClicked(); break;
        case 15: setManual((*reinterpret_cast< bool(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 16;
    }
    return _id;
}

// SIGNAL 0
void controlActionsGroup::hideWidget()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void controlActionsGroup::toggleInfo()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void controlActionsGroup::toggleManual(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void controlActionsGroup::save()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void controlActionsGroup::execute(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void controlActionsGroup::refresh()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}

// SIGNAL 6
void controlActionsGroup::viewHelp()
{
    QMetaObject::activate(this, &staticMetaObject, 6, 0);
}

// SIGNAL 7
void controlActionsGroup::reportBug()
{
    QMetaObject::activate(this, &staticMetaObject, 7, 0);
}
QT_END_MOC_NAMESPACE
