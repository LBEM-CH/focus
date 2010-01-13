/****************************************************************************
** Meta object code from reading C++ file 'LogViewer.h'
**
** Created: Mon Oct 27 20:27:08 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/LogViewer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'LogViewer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_LogViewer[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      13,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      11,   10,   10,   10, 0x05,

 // slots: signature, parameters, type, tag, flags
      29,   10,   10,   10, 0x0a,
      44,   10,   10,   10, 0x0a,
      57,   52,   10,   10, 0x0a,
      77,   52,   10,   10, 0x0a,
     107,  101,   10,   10, 0x0a,
     131,   10,   10,   10, 0x0a,
     163,  153,  148,   10, 0x0a,
     173,   10,  148,   10, 0x2a,
     180,  153,   10,   10, 0x0a,
     210,  198,   10,   10, 0x0a,
     231,   10,   10,   10, 0x0a,
     249,   10,   10,   10, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_LogViewer[] = {
    "LogViewer\0\0fontInfoUpdated()\0"
    "updateViewer()\0clear()\0text\0"
    "insertText(QString)\0insertText(QStringList)\0"
    "error\0insertError(QByteArray)\0"
    "updateFontInfo()\0bool\0verbosity\0"
    "load(int)\0load()\0setVerbosity(int)\0"
    "logFileName\0loadLogFile(QString)\0"
    "setStandardFont()\0setErrorFont()\0"
};

const QMetaObject LogViewer::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_LogViewer,
      qt_meta_data_LogViewer, 0 }
};

const QMetaObject *LogViewer::metaObject() const
{
    return &staticMetaObject;
}

void *LogViewer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_LogViewer))
	return static_cast<void*>(const_cast< LogViewer*>(this));
    return QWidget::qt_metacast(_clname);
}

int LogViewer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: fontInfoUpdated(); break;
        case 1: updateViewer(); break;
        case 2: clear(); break;
        case 3: insertText((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 4: insertText((*reinterpret_cast< const QStringList(*)>(_a[1]))); break;
        case 5: insertError((*reinterpret_cast< const QByteArray(*)>(_a[1]))); break;
        case 6: updateFontInfo(); break;
        case 7: { bool _r = load((*reinterpret_cast< int(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 8: { bool _r = load();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 9: setVerbosity((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 10: loadLogFile((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 11: setStandardFont(); break;
        case 12: setErrorFont(); break;
        }
        _id -= 13;
    }
    return _id;
}

// SIGNAL 0
void LogViewer::fontInfoUpdated()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
