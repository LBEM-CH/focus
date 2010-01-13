/****************************************************************************
** Meta object code from reading C++ file 'importTool.h'
**
** Created: Thu Oct 16 09:54:27 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "importTool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'importTool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_importTool[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      14,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      22,   12,   11,   11, 0x05,

 // slots: signature, parameters, type, tag, flags
      84,   77,   11,   11, 0x0a,
     103,   11,   11,   11, 0x0a,
     116,   11,   11,   11, 0x0a,
     132,   11,   11,   11, 0x0a,
     151,   77,   11,   11, 0x0a,
     177,   11,   11,   11, 0x0a,
     208,  204,   11,   11, 0x0a,
     247,   11,  242,   11, 0x0a,
     270,  265,  242,   11, 0x0a,
     295,   11,  242,   11, 0x0a,
     313,  265,  242,   11, 0x0a,
     338,   11,   11,   11, 0x0a,
     345,   11,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_importTool[] = {
    "importTool\0\0imageList\0"
    "acceptedImages(QHash<QString,QHash<QString,QString> >)\0"
    "regExp\0setRegExp(QString)\0addPattern()\0"
    "removePattern()\0updateParsedView()\0"
    "updateParsedView(QString)\0"
    "updateParsedViewRequired()\0i,j\0"
    "updateParsedViewRequired(int,int)\0"
    "bool\0savePatternList()\0path\0"
    "savePatternList(QString)\0loadPatternList()\0"
    "loadPatternList(QString)\0show()\0"
    "accept()\0"
};

const QMetaObject importTool::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_importTool,
      qt_meta_data_importTool, 0 }
};

const QMetaObject *importTool::metaObject() const
{
    return &staticMetaObject;
}

void *importTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_importTool))
	return static_cast<void*>(const_cast< importTool*>(this));
    return QDialog::qt_metacast(_clname);
}

int importTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: acceptedImages((*reinterpret_cast< const QHash<QString,QHash<QString,QString> >(*)>(_a[1]))); break;
        case 1: setRegExp((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 2: addPattern(); break;
        case 3: removePattern(); break;
        case 4: updateParsedView(); break;
        case 5: updateParsedView((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 6: updateParsedViewRequired(); break;
        case 7: updateParsedViewRequired((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 8: { bool _r = savePatternList();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 9: { bool _r = savePatternList((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 10: { bool _r = loadPatternList();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 11: { bool _r = loadPatternList((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 12: show(); break;
        case 13: accept(); break;
        }
        _id -= 14;
    }
    return _id;
}

// SIGNAL 0
void importTool::acceptedImages(const QHash<QString,QHash<QString,QString> > & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
