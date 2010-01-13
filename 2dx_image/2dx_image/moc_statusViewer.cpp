/****************************************************************************
** Meta object code from reading C++ file 'statusViewer.h'
**
** Created: Mon Oct 5 13:42:15 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "statusViewer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'statusViewer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_statusViewer[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      19,   14,   13,   13, 0x0a,
      41,   36,   13,   13, 0x0a,
      60,   13,   13,   13, 0x0a,
      67,   13,   13,   13, 0x0a,
      76,   13,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_statusViewer[] = {
    "statusViewer\0\0file\0setFile(QString)\0"
    "conf\0setConf(confData*)\0load()\0loaded()\0"
    "timedLoad()\0"
};

const QMetaObject statusViewer::staticMetaObject = {
    { &QWebView::staticMetaObject, qt_meta_stringdata_statusViewer,
      qt_meta_data_statusViewer, 0 }
};

const QMetaObject *statusViewer::metaObject() const
{
    return &staticMetaObject;
}

void *statusViewer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_statusViewer))
        return static_cast<void*>(const_cast< statusViewer*>(this));
    return QWebView::qt_metacast(_clname);
}

int statusViewer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWebView::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setFile((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: setConf((*reinterpret_cast< confData*(*)>(_a[1]))); break;
        case 2: load(); break;
        case 3: loaded(); break;
        case 4: timedLoad(); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
