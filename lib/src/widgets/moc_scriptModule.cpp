/****************************************************************************
** Meta object code from reading C++ file 'scriptModule.h'
**
** Created: Tue Dec 16 15:18:31 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/scriptModule.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'scriptModule.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_scriptModule[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      23,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      14,   13,   13,   13, 0x05,
      26,   21,   13,   13, 0x05,
      51,   21,   13,   13, 0x05,
      83,   77,   13,   13, 0x05,
     101,   97,   13,   13, 0x05,
     124,   13,   13,   13, 0x05,
     139,  133,   13,   13, 0x05,
     173,  133,   13,   13, 0x05,
     207,   13,   13,   13, 0x05,
     224,  133,   13,   13, 0x05,
     253,   13,   13,   13, 0x05,

 // slots: signature, parameters, type, tag, flags
     271,  267,   13,   13, 0x0a,
     285,   13,   13,   13, 0x0a,
     302,   13,   13,   13, 0x0a,
     328,   13,   13,   13, 0x0a,
     341,   13,   13,   13, 0x0a,
     359,  354,   13,   13, 0x0a,
     397,  388,   13,   13, 0x0a,
     417,   13,   13,   13, 0x0a,
     429,  133,   13,   13, 0x0a,
     469,  449,   13,   13, 0x0a,
     507,   13,   13,   13, 0x0a,
     520,   77,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_scriptModule[] = {
    "scriptModule\0\0halt()\0text\0"
    "standardOut(QStringList)\0"
    "standardError(QByteArray)\0value\0"
    "progress(int)\0inc\0incrementProgress(int)\0"
    "reload()\0index\0currentScriptChanged(QModelIndex)\0"
    "runningScriptChanged(QModelIndex)\0"
    "scriptLaunched()\0scriptCompleted(QModelIndex)\0"
    "initialized()\0run\0execute(bool)\0"
    "clearSelection()\0clearExtendedSelections()\0"
    "readStdOut()\0readStdErr()\0item\0"
    "scriptActivated(QModelIndex)\0exitCode\0"
    "scriptFinished(int)\0selectAll()\0"
    "select(QModelIndex)\0selected,deselected\0"
    "select(QItemSelection,QItemSelection)\0"
    "initialize()\0setVerbosity(int)\0"
};

const QMetaObject scriptModule::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_scriptModule,
      qt_meta_data_scriptModule, 0 }
};

const QMetaObject *scriptModule::metaObject() const
{
    return &staticMetaObject;
}

void *scriptModule::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_scriptModule))
	return static_cast<void*>(const_cast< scriptModule*>(this));
    return QWidget::qt_metacast(_clname);
}

int scriptModule::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: halt(); break;
        case 1: standardOut((*reinterpret_cast< const QStringList(*)>(_a[1]))); break;
        case 2: standardError((*reinterpret_cast< const QByteArray(*)>(_a[1]))); break;
        case 3: progress((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: incrementProgress((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: reload(); break;
        case 6: currentScriptChanged((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 7: runningScriptChanged((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 8: scriptLaunched(); break;
        case 9: scriptCompleted((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 10: initialized(); break;
        case 11: execute((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 12: clearSelection(); break;
        case 13: clearExtendedSelections(); break;
        case 14: readStdOut(); break;
        case 15: readStdErr(); break;
        case 16: scriptActivated((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 17: scriptFinished((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 18: selectAll(); break;
        case 19: select((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 20: select((*reinterpret_cast< const QItemSelection(*)>(_a[1])),(*reinterpret_cast< const QItemSelection(*)>(_a[2]))); break;
        case 21: initialize(); break;
        case 22: setVerbosity((*reinterpret_cast< int(*)>(_a[1]))); break;
        }
        _id -= 23;
    }
    return _id;
}

// SIGNAL 0
void scriptModule::halt()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void scriptModule::standardOut(const QStringList & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void scriptModule::standardError(const QByteArray & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void scriptModule::progress(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void scriptModule::incrementProgress(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void scriptModule::reload()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}

// SIGNAL 6
void scriptModule::currentScriptChanged(QModelIndex _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void scriptModule::runningScriptChanged(QModelIndex _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void scriptModule::scriptLaunched()
{
    QMetaObject::activate(this, &staticMetaObject, 8, 0);
}

// SIGNAL 9
void scriptModule::scriptCompleted(QModelIndex _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 9, _a);
}

// SIGNAL 10
void scriptModule::initialized()
{
    QMetaObject::activate(this, &staticMetaObject, 10, 0);
}
QT_END_MOC_NAMESPACE
