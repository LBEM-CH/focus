/****************************************************************************
** Meta object code from reading C++ file 'resultsParser.h'
**
** Created: Wed Jan 20 11:57:46 2010
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "resultsParser.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'resultsParser.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_resultsParser[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      21,   15,   14,   14, 0x05,

 // slots: signature, parameters, type, tag, flags
      44,   14,   14,   14, 0x0a,
      65,   51,   14,   14, 0x0a,
     114,   14,   14,   14, 0x0a,
     131,   14,   14,   14, 0x0a,
     142,   14,   14,   14, 0x0a,
     165,  159,   14,   14, 0x0a,
     183,  159,   14,   14, 0x0a,
     213,  205,   14,   14, 0x0a,
     232,   14,   14,   14, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_resultsParser[] = {
    "resultsParser\0\0image\0imageSelected(QString)\0"
    "load()\0item,previous\0"
    "selectImage(QTableWidgetItem*,QTableWidgetItem*)\0"
    "updateFontInfo()\0openFile()\0"
    "resizeContents()\0value\0setImportant(int)\0"
    "setShowFilenames(int)\0results\0"
    "setResult(QString)\0refreshWatcher()\0"
};

const QMetaObject resultsParser::staticMetaObject = {
    { &QTableWidget::staticMetaObject, qt_meta_stringdata_resultsParser,
      qt_meta_data_resultsParser, 0 }
};

const QMetaObject *resultsParser::metaObject() const
{
    return &staticMetaObject;
}

void *resultsParser::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_resultsParser))
        return static_cast<void*>(const_cast< resultsParser*>(this));
    return QTableWidget::qt_metacast(_clname);
}

int resultsParser::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QTableWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: imageSelected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: load(); break;
        case 2: selectImage((*reinterpret_cast< QTableWidgetItem*(*)>(_a[1])),(*reinterpret_cast< QTableWidgetItem*(*)>(_a[2]))); break;
        case 3: updateFontInfo(); break;
        case 4: openFile(); break;
        case 5: resizeContents(); break;
        case 6: setImportant((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 7: setShowFilenames((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 8: setResult((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 9: refreshWatcher(); break;
        default: ;
        }
        _id -= 10;
    }
    return _id;
}

// SIGNAL 0
void resultsParser::imageSelected(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
