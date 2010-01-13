/****************************************************************************
** Meta object code from reading C++ file 'controlBar.h'
**
** Created: Tue Dec 16 15:18:29 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/controlBar.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'controlBar.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_controlBar[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      14,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      12,   11,   11,   11, 0x05,
      25,   11,   11,   11, 0x05,
      38,   11,   11,   11, 0x05,
      53,   11,   11,   11, 0x05,
      60,   11,   11,   11, 0x05,
      70,   11,   11,   11, 0x05,
      80,   11,   11,   11, 0x05,

 // slots: signature, parameters, type, tag, flags
      97,   91,   11,   11, 0x0a,
     119,  114,   11,   11, 0x0a,
     136,   11,   11,   11, 0x0a,
     153,   11,   11,   11, 0x0a,
     177,  167,   11,   11, 0x0a,
     197,   11,   11,   11, 0x0a,
     219,  214,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_controlBar[] = {
    "controlBar\0\0hideWidget()\0toggleInfo()\0"
    "toggleManual()\0save()\0execute()\0"
    "refresh()\0viewHelp()\0value\0setProgress(int)\0"
    "text\0setText(QString)\0scriptFinished()\0"
    "saveClicked()\0available\0saveAvailable(bool)\0"
    "executeClicked()\0show\0setManual(bool)\0"
};

const QMetaObject controlBar::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_controlBar,
      qt_meta_data_controlBar, 0 }
};

const QMetaObject *controlBar::metaObject() const
{
    return &staticMetaObject;
}

void *controlBar::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_controlBar))
	return static_cast<void*>(const_cast< controlBar*>(this));
    return QWidget::qt_metacast(_clname);
}

int controlBar::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: hideWidget(); break;
        case 1: toggleInfo(); break;
        case 2: toggleManual(); break;
        case 3: save(); break;
        case 4: execute(); break;
        case 5: refresh(); break;
        case 6: viewHelp(); break;
        case 7: setProgress((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 8: setText((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 9: scriptFinished(); break;
        case 10: saveClicked(); break;
        case 11: saveAvailable((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 12: executeClicked(); break;
        case 13: setManual((*reinterpret_cast< bool(*)>(_a[1]))); break;
        }
        _id -= 14;
    }
    return _id;
}

// SIGNAL 0
void controlBar::hideWidget()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void controlBar::toggleInfo()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void controlBar::toggleManual()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void controlBar::save()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void controlBar::execute()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void controlBar::refresh()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}

// SIGNAL 6
void controlBar::viewHelp()
{
    QMetaObject::activate(this, &staticMetaObject, 6, 0);
}
QT_END_MOC_NAMESPACE
