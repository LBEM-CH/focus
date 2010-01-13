/****************************************************************************
** Meta object code from reading C++ file 'confData.h'
**
** Created: Tue Dec 16 15:18:23 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/confData.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'confData.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_confData[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      20,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      10,    9,    9,    9, 0x05,
      29,    9,    9,    9, 0x05,
      39,    9,    9,    9, 0x05,

 // slots: signature, parameters, type, tag, flags
      48,    9,    9,    9, 0x0a,
      64,   55,    9,    9, 0x0a,
      80,   55,    9,    9, 0x0a,
     101,    9,    9,    9, 0x0a,
     108,    9,    9,    9, 0x0a,
     122,  117,    9,    9, 0x0a,
     156,  142,    9,    9, 0x0a,
     206,  195,    9,    9, 0x0a,
     230,  224,    9,    9, 0x0a,
     248,    9,    9,    9, 0x0a,
     281,  268,  263,    9, 0x0a,
     321,  312,  263,    9, 0x2a,
     344,    9,  263,    9, 0x2a,
     360,  117,    9,    9, 0x0a,
     382,    9,    9,    9, 0x2a,
     416,  408,  395,    9, 0x0a,
     451,  437,  429,    9, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_confData[] = {
    "confData\0\0dataModified(bool)\0loading()\0"
    "saving()\0save()\0fileName\0saveAs(QString)\0"
    "setSaveName(QString)\0load()\0reload()\0"
    "conf\0loadConf(confData*)\0conf,defaults\0"
    "loadDefaultConf(confData*,QStringList)\0"
    "isModified\0setModified(bool)\0value\0"
    "setAutoSave(bool)\0loadDefaults()\0bool\0"
    "variable,exp\0syncWithUpper(QString,QRegExp)\0"
    "variable\0syncWithUpper(QString)\0"
    "syncWithUpper()\0setAppConf(confData*)\0"
    "setAppConf()\0confElement*\0element\0"
    "get(QString)\0QString\0element,value\0"
    "get(QString,QString)\0"
};

const QMetaObject confData::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_confData,
      qt_meta_data_confData, 0 }
};

const QMetaObject *confData::metaObject() const
{
    return &staticMetaObject;
}

void *confData::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_confData))
	return static_cast<void*>(const_cast< confData*>(this));
    return QObject::qt_metacast(_clname);
}

int confData::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: dataModified((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: loading(); break;
        case 2: saving(); break;
        case 3: save(); break;
        case 4: saveAs((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 5: setSaveName((*reinterpret_cast< QString(*)>(_a[1]))); break;
        case 6: load(); break;
        case 7: reload(); break;
        case 8: loadConf((*reinterpret_cast< confData*(*)>(_a[1]))); break;
        case 9: loadDefaultConf((*reinterpret_cast< confData*(*)>(_a[1])),(*reinterpret_cast< const QStringList(*)>(_a[2]))); break;
        case 10: setModified((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 11: setAutoSave((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 12: loadDefaults(); break;
        case 13: { bool _r = syncWithUpper((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QRegExp(*)>(_a[2])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 14: { bool _r = syncWithUpper((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 15: { bool _r = syncWithUpper();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 16: setAppConf((*reinterpret_cast< confData*(*)>(_a[1]))); break;
        case 17: setAppConf(); break;
        case 18: { confElement* _r = get((*reinterpret_cast< QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< confElement**>(_a[0]) = _r; }  break;
        case 19: { QString _r = get((*reinterpret_cast< QString(*)>(_a[1])),(*reinterpret_cast< QString(*)>(_a[2])));
            if (_a[0]) *reinterpret_cast< QString*>(_a[0]) = _r; }  break;
        }
        _id -= 20;
    }
    return _id;
}

// SIGNAL 0
void confData::dataModified(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void confData::loading()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void confData::saving()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
