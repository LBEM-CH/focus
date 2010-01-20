/****************************************************************************
** Meta object code from reading C++ file 'ViewerContainer.h'
**
** Created: Wed Jan 20 16:56:03 2010
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "ViewerContainer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'ViewerContainer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_ViewerContainer[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      28,   17,   16,   16, 0x0a,
      54,   16,   16,   16, 0x0a,
      79,   73,   16,   16, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_ViewerContainer[] = {
    "ViewerContainer\0\0verb_level\0"
    "changeVerbosityLevel(int)\0updateLogViewers()\0"
    "index\0updateSelectedViewer(int)\0"
};

const QMetaObject ViewerContainer::staticMetaObject = {
    { &QTabWidget::staticMetaObject, qt_meta_stringdata_ViewerContainer,
      qt_meta_data_ViewerContainer, 0 }
};

const QMetaObject *ViewerContainer::metaObject() const
{
    return &staticMetaObject;
}

void *ViewerContainer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_ViewerContainer))
        return static_cast<void*>(const_cast< ViewerContainer*>(this));
    return QTabWidget::qt_metacast(_clname);
}

int ViewerContainer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QTabWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: changeVerbosityLevel((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: updateLogViewers(); break;
        case 2: updateSelectedViewer((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
