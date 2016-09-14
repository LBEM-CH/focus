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
#include <QDebug>

#include "FileWatcher.h"
#include "ParameterConfiguration.h"

class StatusViewer : public QWebView {
    Q_OBJECT

public:
    StatusViewer(QWidget *parent = NULL);
    StatusViewer(const QString &file, const QString &source = "", QWidget *parent = NULL);

public slots:
    void setFile(const QString &file);
    void setConf(ParametersConfiguration *conf);
    void load();
    void loaded();
    void timedLoad();

private:
    QString filePath;
    FileWatcher watcher;
    ParametersConfiguration *data;

    QTimer timer;

    void initialize();
};

#endif

