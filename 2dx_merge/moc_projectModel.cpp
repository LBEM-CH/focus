/****************************************************************************
** Meta object code from reading C++ file 'projectModel.h'
**
** Created: Thu Oct 16 09:54:14 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "projectModel.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'projectModel.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_projectModel[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      25,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      14,   13,   13,   13, 0x05,
      36,   13,   13,   13, 0x05,

 // slots: signature, parameters, type, tag, flags
      54,   48,   13,   13, 0x0a,
      83,   81,   13,   13, 0x0a,
     131,  126,   13,   13, 0x0a,
     157,   13,  152,   13, 0x0a,
     205,  174,  152,   13, 0x0a,
     237,   13,   13,   13, 0x0a,
     251,   13,  152,   13, 0x0a,
     268,  260,   13,   13, 0x0a,
     296,   13,   13,   13, 0x0a,
     305,   13,   13,   13, 0x0a,
     321,  314,   13,   13, 0x0a,
     343,   13,   13,   13, 0x2a,
     361,  314,   13,   13, 0x0a,
     377,   13,   13,   13, 0x2a,
     389,  314,   13,   13, 0x0a,
     410,   13,   13,   13, 0x2a,
     456,  427,   13,   13, 0x0a,
     522,  500,   13,   13, 0x2a,
     567,  558,  152,   13, 0x0a,
     590,   13,  152,   13, 0x2a,
     618,  606,  152,   13, 0x0a,
     638,   13,  152,   13, 0x0a,
     652,  606,  152,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_projectModel[] = {
    "projectModel\0\0currentImage(QString)\0"
    "reloading()\0index\0itemActivated(QModelIndex)\0"
    ",\0currentRowChanged(QModelIndex,QModelIndex)\0"
    "path\0confChanged(QString)\0bool\0"
    "removeSelected()\0currentItem,itemCount,saveFile\0"
    "save(QStandardItem*,int,QFile&)\0"
    "maskResults()\0submit()\0element\0"
    "updateItems(QStandardItem*)\0update()\0"
    "reload()\0commit\0invertSelection(bool)\0"
    "invertSelection()\0selectAll(bool)\0"
    "selectAll()\0clearSelection(bool)\0"
    "clearSelection()\0currentItem,itemCount,action\0"
    "changeSelection(QStandardItem*,int,QString)\0"
    "currentItem,itemCount\0"
    "changeSelection(QStandardItem*,int)\0"
    "fileName\0loadSelection(QString)\0"
    "loadSelection()\0columnsFile\0"
    "loadHidden(QString)\0saveColumns()\0"
    "saveColumns(QString)\0"
};

const QMetaObject projectModel::staticMetaObject = {
    { &QStandardItemModel::staticMetaObject, qt_meta_stringdata_projectModel,
      qt_meta_data_projectModel, 0 }
};

const QMetaObject *projectModel::metaObject() const
{
    return &staticMetaObject;
}

void *projectModel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_projectModel))
	return static_cast<void*>(const_cast< projectModel*>(this));
    return QStandardItemModel::qt_metacast(_clname);
}

int projectModel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QStandardItemModel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: currentImage((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: reloading(); break;
        case 2: itemActivated((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 3: currentRowChanged((*reinterpret_cast< const QModelIndex(*)>(_a[1])),(*reinterpret_cast< const QModelIndex(*)>(_a[2]))); break;
        case 4: confChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 5: { bool _r = removeSelected();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 6: { bool _r = save((*reinterpret_cast< QStandardItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< QFile(*)>(_a[3])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 7: maskResults(); break;
        case 8: { bool _r = submit();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 9: updateItems((*reinterpret_cast< QStandardItem*(*)>(_a[1]))); break;
        case 10: update(); break;
        case 11: reload(); break;
        case 12: invertSelection((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 13: invertSelection(); break;
        case 14: selectAll((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 15: selectAll(); break;
        case 16: clearSelection((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 17: clearSelection(); break;
        case 18: changeSelection((*reinterpret_cast< QStandardItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< const QString(*)>(_a[3]))); break;
        case 19: changeSelection((*reinterpret_cast< QStandardItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 20: { bool _r = loadSelection((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 21: { bool _r = loadSelection();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 22: { bool _r = loadHidden((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 23: { bool _r = saveColumns();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 24: { bool _r = saveColumns((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        }
        _id -= 25;
    }
    return _id;
}

// SIGNAL 0
void projectModel::currentImage(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void projectModel::reloading()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
