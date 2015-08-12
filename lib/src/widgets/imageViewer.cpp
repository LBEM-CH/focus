#include <imageViewer.h>
#include <QGraphicsTextItem>

/*******************************************************************************/
// Common Definitions

Q_DECLARE_METATYPE(imageViewer*);


/*******************************************************************************/
// imageViewer

imageViewer::imageViewer(const QString &fileName, confData *conf, QWidget *parent)
						:QWidget(parent, Qt::Window)
{
  setAttribute(Qt::WA_DeleteOnClose);

  data = conf;
  
  std::cout << "hi 1" << std::endl;

  image = new largeMRC(fileName,this);
  
   std::cout << "hi 2" << std::endl;

  QGridLayout *layout = new QGridLayout(this);
  layout->setMargin(0);
  layout->setSpacing(0);
  setLayout(layout);

  scale = 1.0;

  scene = new QGraphicsScene(this);
  view = new QGraphicsView(scene,this);
  view->setViewportUpdateMode(QGraphicsView::SmartViewportUpdate);
#ifndef Q_OS_MAC 
  view->setViewport(new glWidget(view));
#endif
  view->setRenderHint(QPainter::Antialiasing,false);
  view->setRenderHint(QPainter::SmoothPixmapTransform,false);
  view->setRenderHint(QPainter::HighQualityAntialiasing,false);
  view->setOptimizationFlag(QGraphicsView::DontAdjustForAntialiasing,true);
  
  layout->addWidget(view);

  view->setDragMode(QGraphicsView::ScrollHandDrag);
  
  float nx = image->width(), ny = image->height(); 
  float w = image->slice(); 

  float offsetX = 0.0;
  float offsetY = 0.0;

  scene->setBackgroundBrush(Qt::black);

  threads = new int;
  *threads =  0;
  mrcGraphicsItem *item;

	while(image->pixmapHeight(offsetX,offsetY) != -1)
	{ 
		while(image->pixmapWidth(offsetX,offsetY) != -1)
		{
      
			item = new mrcGraphicsItem(image,offsetX,offsetY,threads);
			item->setZValue(-1.0);
			scene->addItem(item);
			if(image->isFFT() && image->pixmapWidth(-offsetX-1,-offsetY)!=-1)
			{
				item = new mrcGraphicsItem(image,-offsetX-1,-offsetY,threads);
				item->setZValue(-1.0);
				scene->addItem(item);
			}
			offsetX+=image->pixmapWidth(offsetX,offsetY);
		}

		offsetX = 0.0;
		offsetY+=image->pixmapHeight(offsetX,offsetY);
	}

	resize(1024,768);
	resetView();

  if(image->isFFT())
  {
		scene->setSceneRect(-nx/2.0,-ny/2.0,nx,ny);
  }
	else
		scene->setSceneRect(0.0,0.0,image->width(),image->height());



  setupToolEngine();
  setupMenus();
	setupActions();
  setupOverlays();

	show();
}

imageViewer::~imageViewer()
{
	delete view;
	delete scene;
	delete image;
	delete threads;
}

void imageViewer::addTool(abstractTool *tool)
{
  tool->registerActions(this,menus["navigatorMenu"]);
  scene->addItem(tool->overlay());
  tools<<tool;
}

void imageViewer::setupOverlays()
{
  addTool(new latticeTool(this)); 
}

void imageViewer::setupActions()
{
	QAction *closeAction = new QAction(tr("Close"),this);
	closeAction->setShortcut(tr("Esc"));
	addAction(closeAction);
  menus["navigatorMenu"]->addAction(closeAction);
	connect(closeAction,SIGNAL(triggered()),this,SLOT(close()));

	QAction *resetViewAction = new QAction(tr("Reset View"),this);
	resetViewAction->setShortcut(tr("Space"));
	addAction(resetViewAction);
  menus["navigatorMenu"]->addAction(resetViewAction);
	connect(resetViewAction,SIGNAL(triggered()),this,SLOT(resetView())); 

	QMenu *zoomMenu = new QMenu("Zoom",this);
	QAction *zoomInAction = new QAction(tr("Zoom In"),this);
	zoomInAction->setShortcut(tr("."));
	addAction(zoomInAction);
  menus["navigatorMenu"]->addAction(zoomInAction);
	connect(zoomInAction,SIGNAL(triggered()),this,SLOT(zoomIn()));
	zoomMenu->addAction(zoomInAction);

	QAction *zoomOutAction = new QAction(tr("Zoom Out"),this);
	zoomOutAction->setShortcut(tr(","));
	addAction(zoomOutAction);
  menus["navigatorMenu"]->addAction(zoomOutAction);
	connect(zoomInAction,SIGNAL(triggered()),this,SLOT(zoomIn()));
	connect(zoomOutAction,SIGNAL(triggered()),this,SLOT(zoomOut()));
	zoomMenu->addAction(zoomOutAction);

}

void imageViewer::setupMenus()
{
  menuBar = new QMenuBar(this);
  menus["navigatorMenu"] = menuBar->addMenu(tr("&Navigator"));
}

void imageViewer::setupToolEngine()
{
  toolEngine.setDefaultPrototype(qMetaTypeId<imageViewer*>(),toolEngine.newQObject(&teProto));
  QScriptValue viewer = toolEngine.newQObject(this);
  toolEngine.globalObject().setProperty("viewer",viewer);
  QFile file(data->getDir("tools") + "/test.js");
  file.open(QIODevice::ReadOnly);
  toolEngine.evaluate(file.readAll());
  file.close();
}

void imageViewer::zoomIn()
{
	view->scale(2.0,2.0);
}

void imageViewer::zoomOut()
{
	view->scale(0.5,0.5);
}

void imageViewer::center()
{
  if(image->isFFT())
		view->centerOn(0.0,0.0);
  else
		view->centerOn(image->width()/2.0,image->height()/2.0);
}

void imageViewer::resetView()
{
	view->resetMatrix();
	view->scale(1.0,-1.0);
	center(); 
	update();
}

void imageViewer::hide()
{
	close();
}

QScriptValue imageViewer::newObject(QObject *object, QScriptEngine::ValueOwnership ownership, const QScriptEngine::QObjectWrapOptions & options)
{
  return toolEngine.newQObject(object,ownership,options);
}

/*******************************************************************************/
// imageViewerPrototype

imageViewerPrototype::imageViewerPrototype(QObject *parent)
										 :QObject(parent)
{
}

QScriptValue imageViewerPrototype::create()
{
  viewer = qscriptvalue_cast<imageViewer*>(thisObject());
	if(viewer)
	{
		int argc = context()->argumentCount();
		if(argc>0 && context()->argument(0).isString())
		{
			QString type=context()->argument(0).toString().trimmed().toLower();
      qDebug()<<type<<" "<<argc;
			if(type == "window" && argc>4)     
			{
				int x,y,width,height;
				if(context()->argument(1).isNumber() && context()->argument(2).isNumber() && context()->argument(3).isNumber() && context()->argument(4).isNumber())
				{
					x=context()->argument(1).toInteger();
					y=context()->argument(2).toInteger();
					width=context()->argument(3).toInteger();
					height=context()->argument(4).toInteger();

					QWidget *w = new QWidget(viewer,Qt::Tool);
					QGridLayout *layout = new QGridLayout();
					w->setLayout(layout);
					w->move(x,y);
					w->resize(width,height);   
					return viewer->newObject(w);
				}
			}
			if(type == "input" && argc>=1)
			{
				QLineEdit *input = new QLineEdit();
				return  viewer->newObject(input);
			}
			if(type == "label" && argc>=1)
			{
        QLabel *label = new QLabel;
        return viewer->newObject(label);
			}
		}
	}
	return QScriptValue();
}

void imageViewerPrototype::addToWindow()
{
  viewer = qscriptvalue_cast<imageViewer*>(thisObject());
  if(viewer)
  {
		int argc = context()->argumentCount();
    if(argc>=2 && context()->argument(0).isQObject() && context()->argument(1).isQObject())
    {
			QWidget *w = qscriptvalue_cast<QWidget*>(context()->argument(0));
      QWidget *i = qscriptvalue_cast<QWidget*>(context()->argument(1));
			if(w)
			{
				QGridLayout *l = static_cast<QGridLayout*>(w->layout());
        if(argc==2)
					l->addWidget(i);
        else if(argc >= 4 && context()->argument(2).isNumber() && context()->argument(3).isNumber())
        {
          if(argc >= 6 && context()->argument(3).isNumber() && context()->argument(5).isNumber())
						l->addWidget(i,context()->argument(2).toInteger(),context()->argument(3).toInteger(),context()->argument(4).toInteger(),context()->argument(5).toInteger());
          else
						l->addWidget(i,context()->argument(2).toInteger(),context()->argument(3).toInteger());
        }
			}

		}
	}
}

