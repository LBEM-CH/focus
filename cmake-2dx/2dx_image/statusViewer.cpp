#include <statusViewer.h>

void statusViewer::initialize()
{
  int minWidth = int(QApplication::desktop()->width()/5.00);
  if(minWidth > 235) minWidth = 235;
  setMinimumWidth(minWidth);
  setFixedHeight(minWidth);
  setTextSizeMultiplier(0.72);
  timer.setSingleShot(true);
  data = NULL;
  connect(&watcher,SIGNAL(fileChanged(const QString &)),this,SLOT(timedLoad()));
  connect(page()->mainFrame(),SIGNAL(javaScriptWindowObjectCleared()),this,SLOT(loaded()));
  connect(&timer,SIGNAL(timeout()),this,SLOT(load()));
}

statusViewer::statusViewer(QWidget *parent)
						 :QWebView(parent)
{
  initialize();
}

statusViewer::statusViewer(const QString &file, const QString &source, QWidget *parent)
						 :QWebView(parent)
{
  initialize();
  if(!source.isEmpty())
  {
    if(QFileInfo(file).exists()) QFile::remove(file);
    QFile::copy(source,file);
  }
  setFile(file);  
}

void statusViewer::setFile(const QString &file)
{
  filePath = file;
  watcher.setFile(file);
}

void statusViewer::setConf(confData *conf)
{
  if(data != conf && data!=NULL)
		disconnect(data,SIGNAL(dataModified(bool)),this,SLOT(timedLoad()));
  data = conf;
  connect(data,SIGNAL(dataModified(bool)),this,SLOT(timedLoad()));
}

void statusViewer::load()
{
  QWebView::load(QUrl::fromLocalFile(filePath));
}

void statusViewer::loaded()
{
  if(data!=NULL) page()->mainFrame()->addToJavaScriptWindowObject("confData",data);
}

void statusViewer::timedLoad()
{
	timer.start(100);
}

