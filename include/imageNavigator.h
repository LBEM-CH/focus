/*
 *  imageNavigator.h
 *  2dx
 *
 *  Created by Bryant Gipson on 11/30/05.
 *  Copyright 2005 __MyCompanyName__. All rights reserved.
 *
 */

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
#include "fullScreenImage.h"
#include "confData.h"
#include "zoomWindow.h"
#include "colorTool.h"
#include <phaseView.h>
#include "spotSelectTool.h"
#include "latticeRefineTool.h"
#include "ctfTool.h"
#include "displayParametersTool.h"
#include "mouseAssignTool.h"
#include "selectionFFT.h"
#include "navigatorHelpTool.h"
#include "mrcImage.h"

class imageNavigator: public QScrollArea
{
  Q_OBJECT

  private:
  QString *workingDir;
  confData *data;

  //Tools
  zoomWindow *zoomWin;
  colorTool *colorLookupTool;
  spotSelectTool *spotSelect;
  latticeRefineTool *latticeTool;
  ctfTool *ctfEditor;
  displayParametersTool *parameterEditor;
  mouseAssignTool *mouseAssign;
  selectionFFT *selectionFFTTool;
  navigatorHelpTool *helpTool;

  //Actions
  QAction *closeAction;
  QAction *togglePeakListAction;
  QAction *toggleLatticeViewAction;
  QAction *toggleCTFViewAction;
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
  fullScreenImage *image;
  QImage *sourceImage;
  mrcHeader *imageHeader;

  QMenuBar *mainMenuBar;
  QWidget *savedParent;

  bool spotSelectMode, latticeRefinementMode, createPathMode, fftSelectionMode, ctfView, viewDisplayParameters;


  void Initialize_Tools();
  void Initialize_Actions();
  void Initialize();

  public:

  imageNavigator(QWidget *parent = NULL);
  imageNavigator(confData *conf, mrcImage *image, QMenuBar *menuBar, QWidget *parent = NULL);
  ~imageNavigator();

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
  void showZoomWindow(const QPoint &pos, const QSize &size = QSize(256,256));
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
};
#endif
