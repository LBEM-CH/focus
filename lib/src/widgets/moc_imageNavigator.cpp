/****************************************************************************
** Meta object code from reading C++ file 'imageNavigator.h'
**
** Created: Tue Dec 16 15:18:33 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/imageNavigator.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'imageNavigator.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_imageNavigator[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      46,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      22,   16,   15,   15, 0x05,
      41,   15,   15,   15, 0x05,
      48,   15,   15,   15, 0x05,

 // slots: signature, parameters, type, tag, flags
      57,   15,   15,   15, 0x0a,
      81,   72,   15,   15, 0x0a,
     114,  110,   15,   15, 0x2a,
     153,  137,   15,   15, 0x0a,
     187,  182,   15,   15, 0x2a,
     211,   15,   15,   15, 0x0a,
     232,   15,   15,   15, 0x0a,
     256,  249,   15,   15, 0x0a,
     286,  279,   15,   15, 0x0a,
     321,  307,   15,   15, 0x0a,
     353,  110,   15,   15, 0x0a,
     372,   15,   15,   15, 0x0a,
     395,   15,   15,   15, 0x0a,
     418,   15,   15,   15, 0x0a,
     434,   15,   15,   15, 0x0a,
     460,   15,   15,   15, 0x0a,
     477,   15,   15,   15, 0x0a,
     495,   15,   15,   15, 0x0a,
     514,   15,   15,   15, 0x0a,
     535,   15,   15,   15, 0x0a,
     548,   15,   15,   15, 0x0a,
     565,   15,   15,   15, 0x0a,
     579,   15,   15,   15, 0x0a,
     593,   15,   15,   15, 0x0a,
     607,   15,   15,   15, 0x0a,
     629,  621,   15,   15, 0x0a,
     649,  110,   15,   15, 0x0a,
     674,   15,   15,   15, 0x0a,
     704,  110,   15,   15, 0x0a,
     722,  110,   15,   15, 0x0a,
     746,  110,   15,   15, 0x0a,
     763,   15,   15,   15, 0x0a,
     772,   15,   15,   15, 0x0a,
     782,   15,   15,   15, 0x0a,
     797,   15,   15,   15, 0x0a,
     808,   15,   15,   15, 0x0a,
     817,   15,   15,   15, 0x0a,
     828,   15,   15,   15, 0x0a,
     865,  847,   15,   15, 0x0a,
     909,   15,   15,   15, 0x0a,
     930,  924,   15,   15, 0x0a,
     964,  962,   15,   15, 0x0a,
     988,  981,   15,   15, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_imageNavigator[] = {
    "imageNavigator\0\0scale\0adjustScale(float)\0"
    "test()\0closed()\0closeCurrent()\0pos,size\0"
    "showZoomWindow(QPoint,QSize)\0pos\0"
    "showZoomWindow(QPoint)\0view,reposition\0"
    "showFFTSelection(QRect,bool)\0view\0"
    "showFFTSelection(QRect)\0setReferenceOrigin()\0"
    "setPhaseOrigin()\0enable\0enableFullScreen(bool)\0"
    "widget\0toggleTool(QWidget*)\0widget,action\0"
    "toggleAction(QWidget*,QAction*)\0"
    "toggleSpot(QPoint)\0toggleSpotSelectMode()\0"
    "toggleCreatePathMode()\0toggleCTFView()\0"
    "toggleDisplayParameters()\0toggleInfoTool()\0"
    "toggleColorTool()\0toggleAssignTool()\0"
    "toggleFFTSelection()\0toggleHelp()\0"
    "toggleTiltAxis()\0toggleBoxa1()\0"
    "toggleBoxa2()\0toggleBoxb1()\0toggleBoxb2()\0"
    "visible\0setFitVisible(bool)\0"
    "moveLatticePoint(QPoint)\0"
    "toggleLatticeRefinementMode()\0"
    "zoomClick(QPoint)\0zoomDoubleClick(QPoint)\0"
    "zoomMove(QPoint)\0zoomIn()\0zoomOut()\0"
    "zoomStandard()\0brighter()\0darker()\0"
    "openMenu()\0setMouseDefaults()\0"
    "left,middle,right\0"
    "assignMouseButtons(QString,QString,QString)\0"
    "selectPSList()\0range\0"
    "setMaximumValueSearchRange(int)\0s\0"
    "setSigma(double)\0method\0"
    "setMaxSearchMethod(mrcImage::maxValueMethod)\0"
};

const QMetaObject imageNavigator::staticMetaObject = {
    { &QScrollArea::staticMetaObject, qt_meta_stringdata_imageNavigator,
      qt_meta_data_imageNavigator, 0 }
};

const QMetaObject *imageNavigator::metaObject() const
{
    return &staticMetaObject;
}

void *imageNavigator::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_imageNavigator))
	return static_cast<void*>(const_cast< imageNavigator*>(this));
    return QScrollArea::qt_metacast(_clname);
}

int imageNavigator::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QScrollArea::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: adjustScale((*reinterpret_cast< float(*)>(_a[1]))); break;
        case 1: test(); break;
        case 2: closed(); break;
        case 3: closeCurrent(); break;
        case 4: showZoomWindow((*reinterpret_cast< const QPoint(*)>(_a[1])),(*reinterpret_cast< const QSize(*)>(_a[2]))); break;
        case 5: showZoomWindow((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 6: showFFTSelection((*reinterpret_cast< const QRect(*)>(_a[1])),(*reinterpret_cast< bool(*)>(_a[2]))); break;
        case 7: showFFTSelection((*reinterpret_cast< const QRect(*)>(_a[1]))); break;
        case 8: setReferenceOrigin(); break;
        case 9: setPhaseOrigin(); break;
        case 10: enableFullScreen((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 11: toggleTool((*reinterpret_cast< QWidget*(*)>(_a[1]))); break;
        case 12: toggleAction((*reinterpret_cast< QWidget*(*)>(_a[1])),(*reinterpret_cast< QAction*(*)>(_a[2]))); break;
        case 13: toggleSpot((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 14: toggleSpotSelectMode(); break;
        case 15: toggleCreatePathMode(); break;
        case 16: toggleCTFView(); break;
        case 17: toggleDisplayParameters(); break;
        case 18: toggleInfoTool(); break;
        case 19: toggleColorTool(); break;
        case 20: toggleAssignTool(); break;
        case 21: toggleFFTSelection(); break;
        case 22: toggleHelp(); break;
        case 23: toggleTiltAxis(); break;
        case 24: toggleBoxa1(); break;
        case 25: toggleBoxa2(); break;
        case 26: toggleBoxb1(); break;
        case 27: toggleBoxb2(); break;
        case 28: setFitVisible((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 29: moveLatticePoint((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 30: toggleLatticeRefinementMode(); break;
        case 31: zoomClick((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 32: zoomDoubleClick((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 33: zoomMove((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 34: zoomIn(); break;
        case 35: zoomOut(); break;
        case 36: zoomStandard(); break;
        case 37: brighter(); break;
        case 38: darker(); break;
        case 39: openMenu(); break;
        case 40: setMouseDefaults(); break;
        case 41: assignMouseButtons((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2])),(*reinterpret_cast< const QString(*)>(_a[3]))); break;
        case 42: selectPSList(); break;
        case 43: setMaximumValueSearchRange((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 44: setSigma((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 45: setMaxSearchMethod((*reinterpret_cast< mrcImage::maxValueMethod(*)>(_a[1]))); break;
        }
        _id -= 46;
    }
    return _id;
}

// SIGNAL 0
void imageNavigator::adjustScale(float _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void imageNavigator::test()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void imageNavigator::closed()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
