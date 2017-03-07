#ifndef imageNavigator_H
#define imageNavigator_H

#include <QScrollArea>
#include <QMainWindow>
#include <QMenuBar>
#include <QGridLayout>
#include <QPixmap>
#include <QTimer>
#include <QMenu>
#include <QLabel>
#include <QMainWindow>
#include <QDockWidget>
#include <QLayout>
#include <QMenuBar>
#include <QSignalMapper>
#include <QFileDialog>

#include "FullScreenImage.h"
#include "ParameterConfiguration.h"
#include "ZoomWindow.h"
#include "ColorTool.h"
#include "PhaseView.h"
#include "SpotSelectTool.h"
#include "LatticeRefineTool.h"
#include "CtfTool.h"
#include "DisplayParametersTool.h"
#include "MouseAssignTool.h"
#include "SelectionFFT.h"
#include "NavigatorHelpTool.h"
#include "mrcImage.h"
#include "ResolutionRingTool.h"

class ImageNavigator : public QScrollArea {
    Q_OBJECT

public:

    ImageNavigator(QWidget* parent = 0);
    ImageNavigator(const QString& workingDir, mrcImage* image, QWidget* parent = 0);
    ~ImageNavigator();

    void setType(const QString &type);
    void center();

protected:
    void mousePressEvent(QMouseEvent *event);
    void mouseReleaseEvent(QMouseEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
    void mouseDoubleClickEvent(QMouseEvent *event);
    void keyPressEvent(QKeyEvent *event);

    void resizeEvent(QResizeEvent *event);
    void closeEvent(QCloseEvent *event);

public slots:
    void closeCurrent();
    void showZoomWindow(const QPoint &pos, const QSize &size = QSize(256, 256));
    void showFFTSelection(const QRect &view, bool reposition = true);
    void setReferenceOrigin();
    void setPhaseOrigin();
    void enableFullScreen(bool enable);
    void toggleTool(QWidget *widget);
    void toggleAction(QWidget *widget, QAction *action);
    void toggleSpot(const QPoint &pos);
    void toggleSpotSelectMode();
    void toggleCreatePathMode();
    void toggleCTFView();
    void toggleResolutionRingView();
    void toggleDisplayParameters();
    void toggleInfoTool();
    void toggleColorTool();
    void toggleAssignTool();
    void toggleFFTSelection();
    void toggleHelp();
    void toggleTiltAxis();
    void toggleBoxa1();
    void toggleBoxa2();
    void toggleBoxb1();
    void toggleBoxb2();

    void setFitVisible(bool visible);

    void moveLatticePoint(const QPoint &pos);
    void toggleLatticeRefinementMode();

    void zoomClick(const QPoint &pos);
    void zoomDoubleClick(const QPoint &pos);
    void zoomMove(const QPoint &pos);
    void zoomIn();
    void zoomOut();
    void zoomStandard();
    void brighter();
    void darker();
    void openMenu();

    void setMouseDefaults();
    void assignMouseButtons(const QString &left, const QString &middle, const QString &right);
    void selectPSList();

    void setMaximumValueSearchRange(int range);
    void setSigma(double s);
    void setMaxSearchMethod(mrcImage::maxValueMethod method);

signals:
    void adjustScale(float scale);
    void test();
    void closed();

private:
    QString workingDir;

    //Tools
    ZoomWindow *zoomWin;
    ColorTool *colorLookupTool;
    SpotSelectTool *spotSelect;
    LatticeRefineTool *latticeTool;
    CtfTool *ctfEditor;
    ResolutionRingTool* resolutionRingTool;
    DisplayParametersTool *parameterEditor;
    MouseAssignTool *mouseAssign;
    SelectionFFT *selectionFFTTool;
    NavigatorHelpTool *helpTool;

    //Actions
    QAction *closeAction;
    QAction *togglePeakListAction;
    QAction *toggleLatticeViewAction;
    QAction *toggleCTFViewAction;
    QAction *toggleResolutionRingViewAction;
    QAction *savePeakListAction;
    QAction *loadPeakListAction;
    QAction *clearPeakListAction;
    QAction *enterSpotSelectionModeAction;
    QAction *enterLatticeRefinementModeAction;
    QAction *viewDisplayParametersAction;
    QAction *addRefinementPointAction;
    QAction *toggleInfoToolAction;
    QAction *toggleColorToolAction;
    QAction *toggleMouseAssignAction;
    QAction *toggleParticlesViewAction;

    QRubberBand *selectionArea;

    int screenWidth, screenHeight;

    QPoint navOrigin;
    QPoint selectionOrigin;
    QPoint currentMousePos;
    quint32 scrollSpeed;
    int scrollBorder;
    int horScroll;
    int verScroll;
    float imageScale;
    int maximumValueSearchRange;
    double sigma;
    mrcImage::maxValueMethod maxSearchMethod;
    QMenuBar *menuBar;
    QMenu *menu;
    QMenu *selectionMenu;
    //  QMenu *subMen;
    QString imageType;

    //QLabel *image;
    FullScreenImage *image;
    QImage *sourceImage;
    mrcHeader *imageHeader;

    QMenuBar *mainMenuBar;

    bool spotSelectMode, latticeRefinementMode, createPathMode, fftSelectionMode, ctfView, resolutionRingView, viewDisplayParameters;

    void initializeTools();
    void initializeActions();
    void initialize();
};
#endif
