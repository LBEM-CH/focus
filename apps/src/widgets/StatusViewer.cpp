#include <StatusViewer.h>

void StatusViewer::initialize() {
    setFixedHeight(80);
    setTextSizeMultiplier(0.8);
    timer.setSingleShot(true);
    data = NULL;
    connect(&watcher, SIGNAL(fileChanged(const QString &)), this, SLOT(timedLoad()));
    connect(page()->mainFrame(), SIGNAL(javaScriptWindowObjectCleared()), this, SLOT(loaded()));
    connect(&timer, SIGNAL(timeout()), this, SLOT(load()));
}

StatusViewer::StatusViewer(QWidget *parent)
: QWebView(parent) {
    initialize();
}

StatusViewer::StatusViewer(const QString &file, const QString &source, QWidget *parent)
: QWebView(parent) {
    initialize();
    if (!source.isEmpty()) {
        if (QFileInfo(file).exists()) QFile::remove(file);
        QFile::copy(source, file);
    }
    setFile(file);
}

void StatusViewer::setFile(const QString &file) {
    filePath = file;
    watcher.setFile(file);
}

void StatusViewer::setConf(ParametersConfiguration *conf) {
    if (data != conf && data != NULL)
        disconnect(data, SIGNAL(dataModified(bool)), this, SLOT(timedLoad()));
    data = conf;
    connect(data, SIGNAL(dataModified(bool)), this, SLOT(timedLoad()));
}

void StatusViewer::load() {
    QWebView::load(QUrl::fromLocalFile(filePath));
}

void StatusViewer::loaded() {
    if (data != NULL) page()->mainFrame()->addToJavaScriptWindowObject("config", data);
}

void StatusViewer::timedLoad() {
    timer.start(100);
}

