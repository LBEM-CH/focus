#include <QtWidgets>
#include <algorithm>

#include "FileNameParserDialog.h"
#include "GraphicalButton.h"
#include "GroupContainer.h"
#include "ProjectData.h"

FileNameParserDialog::FileNameParserDialog(QWidget* parent)
: QDialog(parent) {
    
    QPushButton* okButton = new QPushButton("Continue");
    connect(okButton, &QPushButton::clicked, [=]() {
        QStringList selectedParams;
        for (int row = 0; row < selectedParamsCont->count(); row++) {
            selectedParams.append(selectedParamsCont->item(row)->text());
        }
        ProjectPreferences().setImportFileParams(selectedParams);
        this->accept();
    });
    
    QPushButton* cancelButton = new QPushButton("Cancel");
    connect(cancelButton, &QPushButton::clicked, this, &QDialog::reject);
    
    QFormLayout* sepLayout = new QFormLayout();
    seperatorEdit = new QLineEdit();
    seperatorEdit->setFrame(false);
    seperatorEdit->setText(ProjectPreferences().importFileSeparator());
    connect(seperatorEdit, &QLineEdit::textEdited, [ = ]{
            ProjectPreferences().setImportFileSeparator(seperatorEdit->text());
        });
    sepLayout->addRow("Separator (delimiter) in file name: ", seperatorEdit);
    
    QHBoxLayout* buttonLayout = new QHBoxLayout();
    buttonLayout->addStretch(1);
    buttonLayout->addWidget(okButton, 0);
    buttonLayout->addWidget(cancelButton, 0);
    
    QVBoxLayout* mainLayout = new QVBoxLayout;
    mainLayout->setMargin(10);
    mainLayout->setSpacing(0);
    mainLayout->addLayout(sepLayout);
    mainLayout->addWidget(setupParamsContainer(), 1);
    mainLayout->addLayout(buttonLayout, 0);
    setLayout(mainLayout);
    
    setModal(true);
}

QWidget* FileNameParserDialog::setupParamsContainer() {

    GroupContainer* paramsContainer = new GroupContainer;
    paramsContainer->setTitle("Select the parameters expected (first to last)");
    QHBoxLayout* paramContLayout = new QHBoxLayout();

    availableScriptCont = new QListWidget();
    availableScriptCont->setSelectionMode(QAbstractItemView::SingleSelection);
    availableScriptCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
    QStringList paramsList = ProjectData::fileNameParamList();
    paramsList.insert(0, "DUMMY");
    availableScriptCont->addItems(paramsList);

    selectedParamsCont = new QListWidget;
    selectedParamsCont->setSelectionMode(QAbstractItemView::ExtendedSelection);
    selectedParamsCont->setAttribute(Qt::WA_MacShowFocusRect, 0);
    selectedParamsCont->addItems(ProjectPreferences().importFileParams());

    GraphicalButton* moveButton = new GraphicalButton(ApplicationData::icon("move_selected"));
    moveButton->setFixedSize(32, 32);
    connect(moveButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedParams;
        for (int row = 0; row < selectedParamsCont->count(); row++) {
            selectedParams.append(selectedParamsCont->item(row)->text());
        }

        for (QModelIndex index : availableScriptCont->selectionModel()->selectedIndexes()) {
            selectedParams.append(index.data(Qt::DisplayRole).toString());
        }
        selectedParamsCont->clear();
        selectedParamsCont->addItems(selectedParams);
    });

    GraphicalButton* deleteButton = new GraphicalButton(ApplicationData::icon("delete_selected"));
    deleteButton->setFixedSize(32, 32);
    connect(deleteButton, &GraphicalButton::clicked, [ = ](){
        QStringList selectedScripts;
        for (int row = 0; row < selectedParamsCont->count(); row++) {
            selectedScripts.append(selectedParamsCont->item(row)->text());
        }
        
        for (QModelIndex index : selectedParamsCont->selectionModel()->selectedIndexes()) {
            selectedScripts.removeAll(index.data(Qt::DisplayRole).toString());
        }
        selectedParamsCont->clear();
        selectedParamsCont->addItems(selectedScripts);
    });

    QVBoxLayout *buttonsLayout = new QVBoxLayout;
    buttonsLayout->setMargin(0);
    buttonsLayout->setSpacing(4);
    buttonsLayout->addStretch(0);
    buttonsLayout->addWidget(moveButton, 0);
    buttonsLayout->addWidget(deleteButton, 0);
    buttonsLayout->addStretch(0);

    paramContLayout->addWidget(availableScriptCont);
    paramContLayout->addLayout(buttonsLayout, 0);
    paramContLayout->addWidget(new BlockContainer("Selected Scripts", selectedParamsCont));
    paramsContainer->setContainerLayout(paramContLayout);
    return paramsContainer;
}

QString FileNameParserDialog::expectedFileNamePattern() {
    QStringList selectedParams = ProjectPreferences().importFileParams();
    QString sep = ProjectPreferences().importFileSeparator();
    
    if(sep.isEmpty() || selectedParams.isEmpty()) {
        return QString("Nothing set.");
    } else {
        QString pattern = '<' + selectedParams[0] + '>';
        for(int i=1; i< selectedParams.size(); ++i){
            pattern += sep + '<' + selectedParams[i] + '>';
        }
        return pattern;
    }
}


QMap<QString, QString> FileNameParserDialog::parseFileName(const QString& fileName) {
    QStringList selectedParams = ProjectPreferences().importFileParams();
    QString sep = ProjectPreferences().importFileSeparator();
    
    QMap<QString, QString> deducedParams;
    
    if(!sep.isEmpty() && !selectedParams.isEmpty()) {
        QStringList cells = fileName.split(sep);
    
        int loopSize = std::min(cells.size(), selectedParams.size());
        
        for(int i=0; i<loopSize; ++i) {
            QString param = selectedParams[i];
            if(!param.startsWith("DUMMY")) {
                deducedParams.insert(param, cells[i]);
            }
        }
    }
    
    return deducedParams;
    
}



