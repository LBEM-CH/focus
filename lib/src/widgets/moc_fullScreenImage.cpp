/****************************************************************************
** Meta object code from reading C++ file 'fullScreenImage.h'
**
** Created: Tue Dec 16 15:18:33 2008
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../../include/fullScreenImage.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'fullScreenImage.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_fullScreenImage[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
      37,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // signals: signature, parameters, type, tag, flags
      17,   16,   16,   16, 0x05,

 // slots: signature, parameters, type, tag, flags
      27,   16,   16,   16, 0x0a,
      36,   16,   16,   16, 0x0a,
      53,   16,   16,   16, 0x0a,
      73,   16,   16,   16, 0x0a,
      99,   16,   16,   16, 0x0a,
     122,  115,   16,   16, 0x0a,
     144,  115,   16,   16, 0x0a,
     165,  115,   16,   16, 0x0a,
     207,  192,   16,   16, 0x0a,
     240,  232,   16,   16, 0x0a,
     263,  115,   16,   16, 0x0a,
     281,   16,   16,   16, 0x0a,
     327,  297,   16,   16, 0x0a,
     363,  359,   16,   16, 0x0a,
     395,   16,  390,   16, 0x0a,
     413,   16,  409,   16, 0x0a,
     428,   16,  409,   16, 0x0a,
     443,   16,   16,   16, 0x0a,
     459,   16,  409,   16, 0x0a,
     479,   16,   16,   16, 0x0a,
     508,  504,  409,   16, 0x0a,
     531,  526,   16,   16, 0x0a,
     565,  558,   16,   16, 0x0a,
     587,  526,   16,   16, 0x0a,
     611,  526,   16,   16, 0x0a,
     641,  359,   16,   16, 0x0a,
     670,  664,   16,   16, 0x0a,
     696,  690,   16,   16, 0x0a,
     720,  713,   16,   16, 0x0a,
     765,   16,   16,   16, 0x0a,
     781,   16,   16,   16, 0x0a,
     803,  788,   16,   16, 0x0a,
     829,   16,   16,   16, 0x0a,
     845,   16,   16,   16, 0x0a,
     854,   16,   16,   16, 0x0a,
     864,   16,   16,   16, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_fullScreenImage[] = {
    "fullScreenImage\0\0painted()\0update()\0"
    "togglePeakList()\0toggleLatticeView()\0"
    "toggleSecondLatticeView()\0toggleCTFView()\0"
    "enable\0setPeakListView(bool)\0"
    "setLatticeView(bool)\0setRefineLatticeView(bool)\0"
    "overlay,enable\0setVisible(QString,bool)\0"
    "overlay\0toggleVisible(QString)\0"
    "setPathView(bool)\0updateLattice()\0"
    "defocusX,defocusY,astigmatism\0"
    "calculateCTF(float,float,float)\0pos\0"
    "setCurrentMousePos(QPoint)\0bool\0"
    "loadPSPeaks()\0int\0loadPeakList()\0"
    "savePeakList()\0clearPeakList()\0"
    "saveSelectionList()\0clearSelectionVertices()\0"
    "i,j\0gaussian(int,int)\0size\0"
    "setLatticeEllipseSize(int)\0orders\0"
    "setLatticeOrders(int)\0setPeakEllipseSize(int)\0"
    "setRefinementEllipseSize(int)\0"
    "setPhaseOrigin(QPoint)\0range\0"
    "setMaxFitRange(int)\0sigma\0setSigma(double)\0"
    "method\0setMaxSearchMethod(mrcImage::maxValueMethod)\0"
    "createProfile()\0grab()\0min,max,invert\0"
    "rescale(float,float,bool)\0rescaleWidget()\0"
    "zoomIn()\0zoomOut()\0zoomStandard()\0"
};

const QMetaObject fullScreenImage::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_fullScreenImage,
      qt_meta_data_fullScreenImage, 0 }
};

const QMetaObject *fullScreenImage::metaObject() const
{
    return &staticMetaObject;
}

void *fullScreenImage::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_fullScreenImage))
	return static_cast<void*>(const_cast< fullScreenImage*>(this));
    return QWidget::qt_metacast(_clname);
}

int fullScreenImage::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: painted(); break;
        case 1: update(); break;
        case 2: togglePeakList(); break;
        case 3: toggleLatticeView(); break;
        case 4: toggleSecondLatticeView(); break;
        case 5: toggleCTFView(); break;
        case 6: setPeakListView((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 7: setLatticeView((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 8: setRefineLatticeView((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 9: setVisible((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< bool(*)>(_a[2]))); break;
        case 10: toggleVisible((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 11: setPathView((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 12: updateLattice(); break;
        case 13: calculateCTF((*reinterpret_cast< float(*)>(_a[1])),(*reinterpret_cast< float(*)>(_a[2])),(*reinterpret_cast< float(*)>(_a[3]))); break;
        case 14: setCurrentMousePos((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 15: { bool _r = loadPSPeaks();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 16: { int _r = loadPeakList();
            if (_a[0]) *reinterpret_cast< int*>(_a[0]) = _r; }  break;
        case 17: { int _r = savePeakList();
            if (_a[0]) *reinterpret_cast< int*>(_a[0]) = _r; }  break;
        case 18: clearPeakList(); break;
        case 19: { int _r = saveSelectionList();
            if (_a[0]) *reinterpret_cast< int*>(_a[0]) = _r; }  break;
        case 20: clearSelectionVertices(); break;
        case 21: { int _r = gaussian((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])));
            if (_a[0]) *reinterpret_cast< int*>(_a[0]) = _r; }  break;
        case 22: setLatticeEllipseSize((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 23: setLatticeOrders((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 24: setPeakEllipseSize((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 25: setRefinementEllipseSize((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 26: setPhaseOrigin((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        case 27: setMaxFitRange((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 28: setSigma((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 29: setMaxSearchMethod((*reinterpret_cast< mrcImage::maxValueMethod(*)>(_a[1]))); break;
        case 30: createProfile(); break;
        case 31: grab(); break;
        case 32: rescale((*reinterpret_cast< float(*)>(_a[1])),(*reinterpret_cast< float(*)>(_a[2])),(*reinterpret_cast< bool(*)>(_a[3]))); break;
        case 33: rescaleWidget(); break;
        case 34: zoomIn(); break;
        case 35: zoomOut(); break;
        case 36: zoomStandard(); break;
        }
        _id -= 37;
    }
    return _id;
}

// SIGNAL 0
void fullScreenImage::painted()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
