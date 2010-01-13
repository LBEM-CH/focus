/****************************************************************************
** Meta object code from reading C++ file 'textBrowser.h'
**
** Created: Tue Dec 16 15:18:32 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/textBrowser.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'textBrowser.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_textBrowser[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      20,   13,   12,   12, 0x0a,
      36,   13,   12,   12, 0x0a,
      62,   57,   12,   12, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_textBrowser[] = {
    "textBrowser\0\0source\0setSource(QUrl)\0"
    "setLocalSource(QUrl)\0link\0linkClicked(QUrl)\0"
};

const QMetaObject textBrowser::staticMetaObject = {
    { &QTextBrowser::staticMetaObject, qt_meta_stringdata_textBrowser,
      qt_meta_data_textBrowser, 0 }
};

const QMetaObject *textBrowser::metaObject() const
{
    return &staticMetaObject;
}

void *textBrowser::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_textBrowser))
	return static_cast<void*>(const_cast< textBrowser*>(this));
    return QTextBrowser::qt_metacast(_clname);
}

int textBrowser::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QTextBrowser::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setSource((*reinterpret_cast< const QUrl(*)>(_a[1]))); break;
        case 1: setLocalSource((*reinterpret_cast< const QUrl(*)>(_a[1]))); break;
        case 2: linkClicked((*reinterpret_cast< const QUrl(*)>(_a[1]))); break;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
