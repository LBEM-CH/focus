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

#define scriptSelectorDialog (ScriptSelectorDialog::ScriptSelectorInstance())
#define importSelectorDialog (ScriptSelectorDialog::ImportSelectorInstance())

class ScriptSelectorDialog : public QDialog {
    
    Q_OBJECT
    
public:
    static ScriptSelectorDialog& ScriptSelectorInstance();
    static ScriptSelectorDialog& ImportSelectorInstance();
    
    QStringList selectedScriptPaths();
    QStringList scriptPaths(const QStringList& titles);
    
    bool hasAvailableScripts();
    
private:
    
    ScriptSelectorDialog(const QString& type, QWidget* parent=0);
    
    QWidget* setupScriptsContainer(const QString& type);
    
    void resetSelectedScriptsContainer(const QStringList& availScripts, const QStringList& selectedScripts);
    
    //Widgets
    QListWidget* selectedScriptsCont;
    QTabWidget* availaleScriptsBox;
};

#endif /* PARALLELPROCESSINGWINDOW_H */

