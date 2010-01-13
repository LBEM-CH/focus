/****************************************************************************
** Meta object code from reading C++ file 'updateWindow.h'
**
** Created: Tue Dec 16 15:18:33 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/updateWindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'updateWindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_updateWindow[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      14,   13,   13,   13, 0x0a,
      30,   13,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_updateWindow[] = {
    "updateWindow\0\0updateTextBox()\0"
    "updateVersion()\0"
};

const QMetaObject updateWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_updateWindow,
      qt_meta_data_updateWindow, 0 }
};

const QMetaObject *updateWindow::metaObject() const
{
    return &staticMetaObject;
}

void *updateWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_updateWindow))
	return static_cast<void*>(const_cast< updateWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int updateWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: updateTextBox(); break;
        case 1: updateVersion(); break;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
