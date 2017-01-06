#ifndef FILENAMEPARSERDIALOG_H
#define FILENAMEPARSERDIALOG_H

#include <QDialog>
#include <QDebug>
#include <QString>
#include <QLabel>
#include <QListWidget>
#include <QLineEdit>
#include <QMap>

#include "BlockContainer.h"
#include "ProjectData.h"

class FileNameParserDialog : public QDialog {
    
public:
    FileNameParserDialog(QWidget* parent=0);
    
    static QMap<QString, QString> parseFileName(const QString& fileName);
    static QString expectedFileNamePattern();
    
private:

    QWidget* setupParamsContainer();
    
    //Widgets
    QLineEdit* seperatorEdit;
    QListWidget* selectedParamsCont;
    QListWidget* availableScriptCont;
};

#endif /* FILENAMEPARSERDIALOG_H */

