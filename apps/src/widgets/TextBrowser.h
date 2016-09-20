#ifndef TEXTBROWSER_H
#define TEXTBROWSER_H

#include <QTextBrowser>
#include <QProcess>

class TextBrowser : public QTextBrowser {
    
    Q_OBJECT
    
public:
    TextBrowser(QWidget *parent = NULL);

public slots:

    virtual void setSource(const QUrl &source);
    void setLocalSource(const QUrl &source);
    void linkClicked(const QUrl &link);

};

#endif
