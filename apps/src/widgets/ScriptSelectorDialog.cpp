#include <QtWidgets>

#include "ScriptSelectorDialog.h"
#include "GraphicalButton.h"
#include "ScriptModuleProperties.h"
#include "ScriptData.h"
#include "UserPreferences.h"
#include "GroupContainer.h"
#include "ProjectData.h"

ScriptSelectorDialog::ScriptSelectorDialog(QWidget* parent)
: QDialog(parent) {
    QPushButton* okButton = new QPushButton("Continue");
    connect(okButton, &QPushButton::clicked, [=]() {
        QStringList selectedScripts;
        for (int row = 0; row < selectedScriptsCont->count(); row++) {
            selectedScripts.append(selectedScriptsCont->item(row)->text());
        }
        ProjectPreferences().setProcessScripts(selectedScripts);
        this->accept();
    });
    QPushButton* cancelButton = new QPushButton("Cancel");
    connect(cancelButton, &QPushButton::clicked, this, &QDialog::reject);
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch(1);
    buttonLayout->addWidget(okButton, 0);
    buttonLayout->addWidget(cancelButton, 0);
    
    QVBoxLayout* mainLayout = new QVBoxLayout;
    mainLayout->setMargin(10);
    mainLayout->setSpacing(0);
    mainLayout->addWidget(setupScriptsContainer(), 1);
    mainLayout->addLayout(buttonLayout, 0);
    setLayout(mainLayout);
    
    setModal(true);
}

ScriptSelectorDialog& ScriptSelectorDialog::Instance() {
    static ScriptSelectorDialog instance_;
    return instance_;
}


QWidget* ScriptSelectorDialog::setupScriptsContainer() {

    GroupContainer* scriptsContainer = new GroupContainer;
    scriptsContainer->setTitle("Scripts to be executed");
    QHBoxLayout* scriptContLayout = new QHBoxLayout();

    QStringList scriptsAvailable;
    
    QStringList scriptFolders = ScriptModuleProperties(ApplicationData::scriptsDir().absolutePath() + "/image/").subfolders();
    availaleScriptsBox = new QTabWidget;
    for (QString scriptFolder : scriptFolders) {

        QListWidget* availableScriptCont = new QListWidget();
        availableScriptCont->setSelectionMode(QAbstractItemView::ExtendedSelection);
        availableScriptCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
        
        QDir scriptDir = QDir(scriptFolder);
        ScriptModuleProperties scriptProps(scriptDir.absolutePath());

        QMap<int, QListWidgetItem*> map;
        quint32 sortOrder, uid;

        for (QString entry : scriptDir.entryList(QStringList() << "*.script", QDir::Files | QDir::NoDotAndDotDot, QDir::Unsorted)) {
            QStringList titleList;
            ScriptData scriptFileData(scriptDir.canonicalPath() + "/" + entry);
            sortOrder = scriptFileData.getProperty("sortOrder").toUInt();

            uid = qHash(scriptDir.canonicalPath() + "/" + entry)^qHash(sortOrder);

            titleList << QString(scriptFileData.getProperty("title")).simplified();

            QListWidgetItem* item = new QListWidgetItem(titleList.first());
            item->setToolTip(entry);
            item->setData(Qt::UserRole + 5, scriptDir.canonicalPath() + "/" + entry);
            item->setData(Qt::UserRole, uid);
            item->setTextAlignment(Qt::AlignVCenter);
            item->setIcon(ApplicationData::icon(scriptProps.scriptIcon()));
            item->setSizeHint(QSize(200, 30));

            map.insert(sortOrder, item);
        }

        QMapIterator<int, QListWidgetItem*> it(map);
        while (it.hasNext()) {
            it.next();
            availableScriptCont->addItem(it.value());
            scriptsAvailable.append(it.value()->text());
        }
        
        availaleScriptsBox->addTab(availableScriptCont, ApplicationData::icon(scriptProps.icon()), "");
        availaleScriptsBox->setTabToolTip(availaleScriptsBox->count()-1, scriptProps.title());
    }

    selectedScriptsCont = new QListWidget;
    selectedScriptsCont->setSelectionMode(QAbstractItemView::ExtendedSelection);
    selectedScriptsCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
    resetSelectedScriptsContainer(scriptsAvailable, ProjectPreferences().processScripts());

    GraphicalButton* moveButton = new GraphicalButton(ApplicationData::icon("move_selected"));
    moveButton->setFixedSize(32, 32);
    connect(moveButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedScripts;
        for (int row = 0; row < selectedScriptsCont->count(); row++) {
            selectedScripts.append(selectedScriptsCont->item(row)->text());
        }

        QListWidget* availableScriptCont = static_cast<QListWidget*> (availaleScriptsBox->currentWidget());
        for (QModelIndex index : availableScriptCont->selectionModel()->selectedIndexes()) {
            if (!selectedScripts.contains(index.data(Qt::DisplayRole).toString())) selectedScripts.append(index.data(Qt::DisplayRole).toString());
        }
        resetSelectedScriptsContainer(scriptsAvailable, selectedScripts);
    });

    GraphicalButton* deleteButton = new GraphicalButton(ApplicationData::icon("delete_selected"));
    deleteButton->setFixedSize(32, 32);
    connect(deleteButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedScripts;
        for (int row = 0; row < selectedScriptsCont->count(); row++) {
            selectedScripts.append(selectedScriptsCont->item(row)->text());
        }
        
        for (QModelIndex index : selectedScriptsCont->selectionModel()->selectedIndexes()) {
            selectedScripts.removeAll(index.data(Qt::DisplayRole).toString());
        }
        resetSelectedScriptsContainer(scriptsAvailable, selectedScripts);
    });

    QVBoxLayout *buttonsLayout = new QVBoxLayout;
    buttonsLayout->setMargin(0);
    buttonsLayout->setSpacing(4);
    buttonsLayout->addStretch(0);
    buttonsLayout->addWidget(moveButton, 0);
    buttonsLayout->addWidget(deleteButton, 0);
    buttonsLayout->addStretch(0);

    scriptContLayout->addWidget(availaleScriptsBox);
    scriptContLayout->addLayout(buttonsLayout, 0);
    scriptContLayout->addWidget(new BlockContainer("Selected Scripts", selectedScriptsCont));
    scriptsContainer->setContainerLayout(scriptContLayout);
    return scriptsContainer;
}

void ScriptSelectorDialog::resetSelectedScriptsContainer(const QStringList& availScripts, const QStringList& selectedScripts) {
    selectedScriptsCont->clear();
    for (QString script : availScripts) {
        if (selectedScripts.contains(script)) {
            QList<QListWidgetItem*> foundItems;
            for(int i=0; i< availaleScriptsBox->count(); ++i)  {
                QListWidget* availCont = static_cast<QListWidget*>(availaleScriptsBox->widget(i));
                foundItems.append(availCont->findItems(script, Qt::MatchExactly));
            }
            if (foundItems.size() > 0) {
                QListWidgetItem* foundItem = foundItems.first();
                QListWidgetItem* item = new QListWidgetItem(foundItem->text());
                item->setToolTip(foundItem->toolTip());
                item->setData(Qt::UserRole + 5, foundItem->data(Qt::UserRole + 5));
                item->setData(Qt::UserRole, foundItem->data(Qt::UserRole));
                item->setTextAlignment(Qt::AlignVCenter);
                item->setIcon(foundItem->icon());
                item->setSizeHint(QSize(150, 30));

                selectedScriptsCont->addItem(item);
            }
        }
    }
}

QStringList ScriptSelectorDialog::selectedScriptPaths() {
    QStringList paths;
    for (int row = 0; row < selectedScriptsCont->count(); row++) {
        paths.append(selectedScriptsCont->item(row)->data(Qt::UserRole + 5).toString());
    }
    return paths;
}

QStringList ScriptSelectorDialog::scriptPaths(const QStringList& titles) {
    QStringList paths;
    for (QString script : titles) {
        QList<QListWidgetItem*> foundItems;
        for (int i = 0; i < availaleScriptsBox->count(); ++i) {
            QListWidget* availCont = static_cast<QListWidget*> (availaleScriptsBox->widget(i));
            foundItems.append(availCont->findItems(script, Qt::MatchExactly));
        }
        if (foundItems.size() > 0) {
            QListWidgetItem* foundItem = foundItems.first();
            paths.append(foundItem->data(Qt::UserRole + 5).toString());
        }
    }
    return paths;
}

