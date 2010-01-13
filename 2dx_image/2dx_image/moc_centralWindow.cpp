/****************************************************************************
** Meta object code from reading C++ file 'centralWindow.h'
**
** Created: Mon Oct 5 13:42:14 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "centralWindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'centralWindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_centralWindow[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      23,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      15,   14,   14,   14, 0x05,

 // slots: signature, parameters, type, tag, flags
      46,   33,   14,   14, 0x0a,
      93,   87,   14,   14, 0x0a,
     128,   87,   14,   14, 0x0a,
     161,   33,   14,   14, 0x0a,
     204,   87,   14,   14, 0x0a,
     241,   87,   14,   14, 0x0a,
     276,   33,   14,   14, 0x0a,
     324,   87,   14,   14, 0x0a,
     364,   87,   14,   14, 0x0a,
     424,  411,  406,   14, 0x0a,
     456,   14,  406,   14, 0x0a,
     478,  471,   14,   14, 0x0a,
     515,  510,   14,   14, 0x0a,
     539,  532,   14,   14, 0x0a,
     558,   14,   14,   14, 0x0a,
     567,   14,   14,   14, 0x0a,
     577,   14,   14,   14, 0x0a,
     586,   14,   14,   14, 0x0a,
     597,   14,   14,   14, 0x0a,
     609,   14,   14,   14, 0x0a,
     628,   14,   14,   14, 0x0a,
     645,  510,   14,   14, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_centralWindow[] = {
    "centralWindow\0\0fontInfoUpdated()\0"
    "module,index\0scriptChanged(scriptModule*,QModelIndex)\0"
    "index\0standardScriptChanged(QModelIndex)\0"
    "customScriptChanged(QModelIndex)\0"
    "scriptCompleted(scriptModule*,QModelIndex)\0"
    "standardScriptCompleted(QModelIndex)\0"
    "customScriptCompleted(QModelIndex)\0"
    "runningScriptChanged(scriptModule*,QModelIndex)\0"
    "customRunningScriptChanged(QModelIndex)\0"
    "standardRunningScriptChanged(QModelIndex)\0"
    "bool\0conf,results\0parseResults(confData*,QString)\0"
    "parseResults()\0bridge\0"
    "bridgeScriptLogConnection(bool)\0show\0"
    "showManual(bool)\0enable\0useNewViewer(bool)\0"
    "reload()\0refresh()\0revert()\0viewHelp()\0"
    "reportBug()\0launchLogBrowser()\0"
    "updateFontInfo()\0toggleHistoryView(bool)\0"
};

const QMetaObject centralWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_centralWindow,
      qt_meta_data_centralWindow, 0 }
};

const QMetaObject *centralWindow::metaObject() const
{
    return &staticMetaObject;
}

void *centralWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_centralWindow))
        return static_cast<void*>(const_cast< centralWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int centralWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: fontInfoUpdated(); break;
        case 1: scriptChanged((*reinterpret_cast< scriptModule*(*)>(_a[1])),(*reinterpret_cast< QModelIndex(*)>(_a[2]))); break;
        case 2: standardScriptChanged((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 3: customScriptChanged((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 4: scriptCompleted((*reinterpret_cast< scriptModule*(*)>(_a[1])),(*reinterpret_cast< QModelIndex(*)>(_a[2]))); break;
        case 5: standardScriptCompleted((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 6: customScriptCompleted((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 7: runningScriptChanged((*reinterpret_cast< scriptModule*(*)>(_a[1])),(*reinterpret_cast< QModelIndex(*)>(_a[2]))); break;
        case 8: customRunningScriptChanged((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 9: standardRunningScriptChanged((*reinterpret_cast< QModelIndex(*)>(_a[1]))); break;
        case 10: { bool _r = parseResults((*reinterpret_cast< confData*(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 11: { bool _r = parseResults();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 12: bridgeScriptLogConnection((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 13: showManual((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 14: useNewViewer((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 15: reload(); break;
        case 16: refresh(); break;
        case 17: revert(); break;
        case 18: viewHelp(); break;
        case 19: reportBug(); break;
        case 20: launchLogBrowser(); break;
        case 21: updateFontInfo(); break;
        case 22: toggleHistoryView((*reinterpret_cast< bool(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 23;
    }
    return _id;
}

// SIGNAL 0
void centralWindow::fontInfoUpdated()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
