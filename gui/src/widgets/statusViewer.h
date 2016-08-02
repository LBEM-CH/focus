#ifndef _STATUSVIEWER_H_
#define _STATUSVIEWER_H_

#include <QApplication>
#include <QDesktopWidget>
#include <QLabel>
#include <QWebView>
#include <QWebPage>
#include <QWebFrame>
#include <QFile>
#include <QTimer>
#include <fileWatcher.h>
#include <confData.h>
#include <QDebug>

class statusViewer : public QWebView
{
  Q_OBJECT

  public slots:
  void setFile(const QString &file);
	void setConf(confData *conf);
  void load();
	void loaded();
  void timedLoad();

  private:
  QString filePath;
  fileWatcher watcher;
  confData *data;

  QTimer timer;

	void initialize();

  public:
	statusViewer(QWidget *parent = NULL);
	statusViewer(const QString &file, const QString &source = "", QWidget *parent = NULL);
};

#endif

