/****************************************************************************
** Meta object code from reading C++ file 'albumViewer.h'
**
** Created: Thu Oct 16 09:54:20 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "albumViewer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'albumViewer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_albumViewer[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      22,   13,   12,   12, 0x0a,
      49,   40,   12,   12, 0x0a,
      66,   12,   12,   12, 0x0a,
      87,   12,   12,   12, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_albumViewer[] = {
    "albumViewer\0\0fileName\0setImage(QString)\0"
    "confName\0setConf(QString)\0"
    "loadCellParameters()\0loadConfParameters()\0"
};

const QMetaObject albumViewer::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_albumViewer,
      qt_meta_data_albumViewer, 0 }
};

const QMetaObject *albumViewer::metaObject() const
{
    return &staticMetaObject;
}

void *albumViewer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_albumViewer))
	return static_cast<void*>(const_cast< albumViewer*>(this));
    return QWidget::qt_metacast(_clname);
}

int albumViewer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setImage((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: setConf((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 2: loadCellParameters(); break;
        case 3: loadConfParameters(); break;
        }
        _id -= 4;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
