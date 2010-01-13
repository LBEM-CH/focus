/****************************************************************************
** Meta object code from reading C++ file 'mainWindow.h'
**
** Created: Mon Oct 27 20:18:24 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "mainWindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'mainWindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_mainWindow[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      34,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      17,   12,   11,   11, 0x05,
      31,   11,   11,   11, 0x05,

 // slots: signature, parameters, type, tag, flags
      57,   44,   11,   11, 0x0a,
     104,   98,   11,   11, 0x0a,
     139,   98,   11,   11, 0x0a,
     172,   44,   11,   11, 0x0a,
     214,   44,   11,   11, 0x0a,
     257,   98,   11,   11, 0x0a,
     294,   98,   11,   11, 0x0a,
     329,   11,   11,   11, 0x0a,
     348,   11,   11,   11, 0x0a,
     367,   11,   11,   11, 0x0a,
     390,  384,   11,   11, 0x0a,
     414,  409,   11,   11, 0x0a,
     430,   11,   11,   11, 0x2a,
     444,  442,   11,   11, 0x0a,
     472,  465,   11,   11, 0x0a,
     492,   11,   11,   11, 0x0a,
     501,   11,   11,   11, 0x0a,
     524,  514,   11,   11, 0x0a,
     596,  576,   11,   11, 0x0a,
     639,   11,   11,   11, 0x0a,
     656,   11,   11,   11, 0x0a,
     670,   11,   11,   11, 0x0a,
     687,   11,   11,   11, 0x0a,
     706,   11,   11,   11, 0x0a,
     725,   11,   11,   11, 0x0a,
     732,   11,   11,   11, 0x0a,
     745,  741,   11,   11, 0x0a,
     767,  762,   11,   11, 0x0a,
     788,  409,   11,   11, 0x0a,
     805,   11,   11,   11, 0x0a,
     830,   11,   11,   11, 0x0a,
     862,  855,   11,   11, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_mainWindow[] = {
    "mainWindow\0\0halt\0execute(bool)\0"
    "saveConfig()\0module,index\0"
    "scriptChanged(scriptModule*,QModelIndex)\0"
    "index\0standardScriptChanged(QModelIndex)\0"
    "customScriptChanged(QModelIndex)\0"
    "scriptLaunched(scriptModule*,QModelIndex)\0"
    "scriptCompleted(scriptModule*,QModelIndex)\0"
    "standardScriptCompleted(QModelIndex)\0"
    "customScriptCompleted(QModelIndex)\0"
    "saveProjectState()\0loadProjectState()\0"
    "editHelperConf()\0state\0setSaveState(bool)\0"
    "show\0showAlbum(bool)\0showAlbum()\0i\0"
    "columnActivated(int)\0option\0"
    "maximizeWindow(int)\0import()\0autoImport()\0"
    "imageList\0importFiles(QHash<QString,QHash<QString,QString> >)\0"
    "fileName,imageCodes\0"
    "importFile(QString,QHash<QString,QString>)\0"
    "importFinished()\0updateModel()\0"
    "updateFontInfo()\0increaseFontSize()\0"
    "decreaseFontSize()\0open()\0reload()\0"
    "url\0openURL(QString)\0path\0"
    "launchAlbum(QString)\0showManual(bool)\0"
    "saveDirectorySelection()\0"
    "loadDirectorySelection()\0enable\0"
    "showSelected(bool)\0"
};

const QMetaObject mainWindow::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_mainWindow,
      qt_meta_data_mainWindow, 0 }
};

const QMetaObject *mainWindow::metaObject() const
{
    return &staticMetaObject;
}

void *mainWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_mainWindow))
	return static_cast<void*>(const_cast< mainWindow*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int mainWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: execute((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: saveConfig(); break;
        case 2: scriptChanged((*reinterpret_cast< scriptModule*(*)>(_a[1])),(*reinterpret_cast< QModelIndex(*)>(_a[2]))); break;
        case 3: standardScriptChanged((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 4: customScriptChanged((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 5: scriptLaunched((*reinterpret_cast< scriptModule*(*)>(_a[1])),(*reinterpret_cast< QModelIndex(*)>(_a[2]))); break;
        case 6: scriptCompleted((*reinterpret_cast< scriptModule*(*)>(_a[1])),(*reinterpret_cast< QModelIndex(*)>(_a[2]))); break;
        case 7: standardScriptCompleted((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 8: customScriptCompleted((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 9: saveProjectState(); break;
        case 10: loadProjectState(); break;
        case 11: editHelperConf(); break;
        case 12: setSaveState((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 13: showAlbum((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 14: showAlbum(); break;
        case 15: columnActivated((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 16: maximizeWindow((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 17: import(); break;
        case 18: autoImport(); break;
        case 19: importFiles((*reinterpret_cast< const QHash<QString,QHash<QString,QString> >(*)>(_a[1]))); break;
        case 20: importFile((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QHash<QString,QString>(*)>(_a[2]))); break;
        case 21: importFinished(); break;
        case 22: updateModel(); break;
        case 23: updateFontInfo(); break;
        case 24: increaseFontSize(); break;
        case 25: decreaseFontSize(); break;
        case 26: open(); break;
        case 27: reload(); break;
        case 28: openURL((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 29: launchAlbum((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 30: showManual((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 31: saveDirectorySelection(); break;
        case 32: loadDirectorySelection(); break;
        case 33: showSelected((*reinterpret_cast< bool(*)>(_a[1]))); break;
        }
        _id -= 34;
    }
    return _id;
}

// SIGNAL 0
void mainWindow::execute(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void mainWindow::saveConfig()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
