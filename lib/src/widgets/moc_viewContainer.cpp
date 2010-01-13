/****************************************************************************
** Meta object code from reading C++ file 'viewContainer.h'
**
** Created: Mon Oct 27 20:27:08 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/viewContainer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'viewContainer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_viewContainer[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       9,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      15,   14,   14,   14, 0x05,

 // slots: signature, parameters, type, tag, flags
      31,   14,   14,   14, 0x0a,
      44,   39,   14,   14, 0x0a,
      67,   61,   14,   14, 0x0a,
      90,   14,   14,   14, 0x2a,
     110,   61,   14,   14, 0x0a,
     136,   14,   14,   14, 0x2a,
     159,   14,   14,   14, 0x0a,
     169,   14,   14,   14, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_viewContainer[] = {
    "viewContainer\0\0doubleClicked()\0shade()\0"
    "text\0setText(QString)\0state\0"
    "saveSplitterState(int)\0saveSplitterState()\0"
    "restoreSplitterState(int)\0"
    "restoreSplitterState()\0showAll()\0"
    "resizeSplitter()\0"
};

const QMetaObject viewContainer::staticMetaObject = {
    { &QFrame::staticMetaObject, qt_meta_stringdata_viewContainer,
      qt_meta_data_viewContainer, 0 }
};

const QMetaObject *viewContainer::metaObject() const
{
    return &staticMetaObject;
}

void *viewContainer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_viewContainer))
	return static_cast<void*>(const_cast< viewContainer*>(this));
    return QFrame::qt_metacast(_clname);
}

int viewContainer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: doubleClicked(); break;
        case 1: shade(); break;
        case 2: setText((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: saveSplitterState((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: saveSplitterState(); break;
        case 5: restoreSplitterState((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 6: restoreSplitterState(); break;
        case 7: showAll(); break;
        case 8: resizeSplitter(); break;
        }
        _id -= 9;
    }
    return _id;
}

// SIGNAL 0
void viewContainer::doubleClicked()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
