#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsPixmapItem>
#include <QGridLayout>
#include <QAction>
#include <QMenuBar>
#include <QtScript>
#include <QLabel>
#include <QLineEdit>
#include <largeMRC.h>
#include <glWidget.h>
#include <mrcGraphicsItem.h>
#include <confData.h>
#include <latticeTool.h>
#include <QDebug>

class imageViewer;

class imageViewerPrototype : public QObject, public QScriptable
{
  Q_OBJECT

  private:
  imageViewer *viewer;

  public:
  imageViewerPrototype(QObject *parent = NULL);
  
  public Q_SLOTS:
  QScriptValue create();
  void addToWindow();
};

class imageViewer : public QWidget
{
  Q_OBJECT

  public slots:
  void hide();
  void center();
  void resetView();
  void zoomIn();
  void zoomOut();

	void addTool(abstractTool *tool);

  private:
  confData *data;

  QGraphicsView *view;
  QGraphicsScene *scene;

  imageViewerPrototype teProto;
  QScriptEngine toolEngine;
  
  QMenuBar *menuBar;
  QHash<QString,QMenu*> menus;
  QMenu *navigatorMenu;

  QList<abstractTool *> tools;

  largeMRC *image;
  int *threads;

  float scale;

  void setupActions();
  void setupMenus();
  void setupToolEngine();

  public: 
  imageViewer(const QString &fileName, confData *data,  QWidget *parent = NULL);
  ~imageViewer();
	void setupOverlays();

	QScriptValue newObject(QObject *object, QScriptEngine::ValueOwnership ownership = QScriptEngine::QtOwnership, const QScriptEngine::QObjectWrapOptions & options = 0);

};


