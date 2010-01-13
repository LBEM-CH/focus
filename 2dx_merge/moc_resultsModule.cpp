/****************************************************************************
** Meta object code from reading C++ file 'resultsModule.h'
**
** Created: Thu Oct 16 09:54:16 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "resultsModule.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'resultsModule.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_resultsModule[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      22,   15,   14,   14, 0x05,

 // slots: signature, parameters, type, tag, flags
      45,   14,   14,   14, 0x0a,
      57,   52,   14,   14, 0x0a,
      88,   52,   14,   14, 0x0a,
     120,   14,   14,   14, 0x0a,
     138,   14,   14,   14, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_resultsModule[] = {
    "resultsModule\0\0result\0resultChanged(QString)\0"
    "load()\0item\0itemSelected(QTreeWidgetItem*)\0"
    "itemActivated(QTreeWidgetItem*)\0"
    "setImportant(int)\0setShowFilenames(int)\0"
};

const QMetaObject resultsModule::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_resultsModule,
      qt_meta_data_resultsModule, 0 }
};

const QMetaObject *resultsModule::metaObject() const
{
    return &staticMetaObject;
}

void *resultsModule::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_resultsModule))
	return static_cast<void*>(const_cast< resultsModule*>(this));
    return QWidget::qt_metacast(_clname);
}

int resultsModule::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: resultChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: load(); break;
        case 2: itemSelected((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1]))); break;
        case 3: itemActivated((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1]))); break;
        case 4: setImportant((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: setShowFilenames((*reinterpret_cast< int(*)>(_a[1]))); break;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void resultsModule::resultChanged(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
