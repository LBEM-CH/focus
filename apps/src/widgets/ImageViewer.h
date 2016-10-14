#ifndef IMAGEVIEWER_H
#define IMAGEVIEWER_H

#include <QFrame>
#include <QWidget>

#include "MrcHeaderDisplay.h"
#include "FileInfoDisplay.h"

class QLabel;
class QScrollArea;
class QScrollBar;
class QStackedWidget;

class ImageViewer : public QFrame {
    Q_OBJECT

public:
    ImageViewer(const QString& workDir, const QString& notFoundMessage="File not selected", QWidget* parent=0);
    void loadFile(const QString& file, const QString& extenstion, bool loadInfo);
    void setFileProperties(const QString& file, const QString& extenstion);

private slots:
    void progressDialog();
    void clearWidgets();
    
protected:
    void resizeEvent(QResizeEvent* event) override;
    
signals:
    void setProgress(int value);

protected:
    void mouseDoubleClickEvent(QMouseEvent *event);

private:
    void setText(const QString& text);
    void setNotSupportedText();
    void resizeWidgets();

    QStackedWidget* widgets;
    
    QLabel* imageLabel;
    MrcHeaderDisplay* mrcInfo;
    FileInfoDisplay* fileInfo;
            
    QString notFoundMessage_;
    QString workingDir_;
    QString fileName_;
    QString extension_;
};

#endif
