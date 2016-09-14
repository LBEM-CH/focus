#ifndef NAVIGATORHELPTOOL_H
#define NAVIGATORHELPTOOL_H

#include <QWidget>
#include <QGridLayout>
#include <QTextBrowser>

class NavigatorHelpTool : public QWidget {
    
public:
    NavigatorHelpTool(const QString &fileName, QWidget *parent = NULL);

private:
    QTextBrowser *helpText;
};

#endif
