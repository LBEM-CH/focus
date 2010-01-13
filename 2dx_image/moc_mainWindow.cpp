/****************************************************************************
** Meta object code from reading C++ file 'mainWindow.h'
**
** Created: Wed Oct 29 21:09:44 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "mainWindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'mainWindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_mainWindow[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      11,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      12,   11,   11,   11, 0x0a,
      19,   11,   11,   11, 0x0a,
      26,   11,   11,   11, 0x0a,
      49,   11,   11,   11, 0x0a,
      74,   11,   11,   11, 0x0a,
      83,   11,   11,   11, 0x0a,
     102,   11,   11,   11, 0x0a,
     121,   11,   11,   11, 0x0a,
     138,   11,   11,   11, 0x0a,
     147,   11,   11,   11, 0x0a,
     161,   11,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_mainWindow[] = {
    "mainWindow\0\0open()\0save()\0"
    "saveAsProjectDefault()\0saveAsTiltRangeDefault()\0"
    "saveAs()\0increaseFontSize()\0"
    "decreaseFontSize()\0updateAutoSave()\0"
    "revert()\0showUpdates()\0editHelperConf()\0"
};

const QMetaObject mainWindow::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_mainWindow,
      qt_meta_data_mainWindow, 0 }
};

const QMetaObject *mainWindow::metaObject() const
{
    return &staticMetaObject;
}

void *mainWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_mainWindow))
	return static_cast<void*>(const_cast< mainWindow*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int mainWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: open(); break;
        case 1: save(); break;
        case 2: saveAsProjectDefault(); break;
        case 3: saveAsTiltRangeDefault(); break;
        case 4: saveAs(); break;
        case 5: increaseFontSize(); break;
        case 6: decreaseFontSize(); break;
        case 7: updateAutoSave(); break;
        case 8: revert(); break;
        case 9: showUpdates(); break;
        case 10: editHelperConf(); break;
        }
        _id -= 11;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
