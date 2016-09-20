#ifndef SCRIPTHELP_H
#define SCRIPTHELP_H

#include <QDialog>
#include <QLabel>
#include <QString>
#include <QFont>
#include <QPalette>
#include <QVBoxLayout>
#include <QScrollArea>

#include "ApplicationData.h"
#include "TextBrowser.h"

class ScriptHelp : public QDialog {
    Q_OBJECT

public:
    
    ScriptHelp(QWidget* parent);
    void setTitle(const QString& t);
    void setData(const QStringList& manual, const QStringList& scriptPubs);
    
private:
    
    void setupTitleLabel();
    void setupManualLabel();
    
    QLabel* title;
    TextBrowser* browser;
    QStringList defaultPublications;

};

#endif /* SCRIPTHELP_H */

