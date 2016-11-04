#ifndef PARALLELPROCESSINGWINDOW_H
#define PARALLELPROCESSINGWINDOW_H

#include <QDialog>
#include <QDebug>
#include <QTimer>
#include <QMap>
#include <QString>
#include <QLabel>
#include <QListWidget>
#include <QProgressBar>
#include <QTabWidget>
#include <QMutex>

#include "BlockContainer.h"
#include "ProjectData.h"

#define scriptSelectorDialog (ScriptSelectorDialog::Instance())

class ScriptSelectorDialog : public QDialog {
    
    Q_OBJECT
    
public:
    static ScriptSelectorDialog& Instance();
    
    QStringList selectedScriptPaths();
    QStringList scriptPaths(const QStringList& titles);
    
private:
    
    ScriptSelectorDialog(QWidget* parent=0);
    
    QWidget* setupScriptsContainer();
    
    void resetSelectedScriptsContainer(const QStringList& availScripts, const QStringList& selectedScripts);
    
    //Widgets
    QListWidget* selectedScriptsCont;
    QTabWidget* availaleScriptsBox;
};

#endif /* PARALLELPROCESSINGWINDOW_H */

