/****************************************************************************
** Meta object code from reading C++ file 'latticeRefineTool.h'
**
** Created: Tue Dec 16 15:18:33 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/latticeRefineTool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'latticeRefineTool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_latticeRefineTool[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      12,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      23,   19,   18,   18, 0x0a,
      44,   18,   18,   18, 0x0a,
      58,   18,   18,   18, 0x0a,
      76,   18,   18,   18, 0x0a,
      95,   18,   18,   18, 0x0a,
     111,   18,   18,   18, 0x0a,
     133,   18,   18,   18, 0x0a,
     144,   18,   18,   18, 0x0a,
     172,  163,  158,   18, 0x0a,
     200,   18,  158,   18, 0x0a,
     207,  163,  158,   18, 0x0a,
     235,   18,  158,   18, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_latticeRefineTool[] = {
    "latticeRefineTool\0\0pos\0updatePoint(QPointF)\0"
    "insertPoint()\0updateTableView()\0"
    "calculateLattice()\0commitLattice()\0"
    "commitSecondLattice()\0clearAll()\0"
    "deletePoint()\0bool\0fileName\0"
    "saveRefinementList(QString)\0save()\0"
    "loadRefinementList(QString)\0load()\0"
};

const QMetaObject latticeRefineTool::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_latticeRefineTool,
      qt_meta_data_latticeRefineTool, 0 }
};

const QMetaObject *latticeRefineTool::metaObject() const
{
    return &staticMetaObject;
}

void *latticeRefineTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_latticeRefineTool))
	return static_cast<void*>(const_cast< latticeRefineTool*>(this));
    return QWidget::qt_metacast(_clname);
}

int latticeRefineTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: updatePoint((*reinterpret_cast< const QPointF(*)>(_a[1]))); break;
        case 1: insertPoint(); break;
        case 2: updateTableView(); break;
        case 3: calculateLattice(); break;
        case 4: commitLattice(); break;
        case 5: commitSecondLattice(); break;
        case 6: clearAll(); break;
        case 7: deletePoint(); break;
        case 8: { bool _r = saveRefinementList((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 9: { bool _r = save();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 10: { bool _r = loadRefinementList((*reinterpret_cast< const QString(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 11: { bool _r = load();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        }
        _id -= 12;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
