#ifndef IMAGECONFIGCHANGER_H
#define IMAGECONFIGCHANGER_H

#include <QWidget>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QFormLayout>
#include <QLayoutItem>
#include <QScrollArea>
#include <QHBoxLayout>
#include <QRadioButton>
#include <QMap>
#include <QPushButton>
#include <QButtonGroup>
#include <QMessageBox>

#include "ProjectData.h"
#include "ParameterInput.h"
#include "BlockContainer.h"
#include "GroupContainer.h"

class ImageConfigChanger : public QWidget {
public:

    ImageConfigChanger(QWidget* parent = 0)
    : QWidget(parent) {

        GroupContainer* parametersGroup = new GroupContainer();
        parametersGroup->setTitle("Select and change parameters to sync");
        parametersGroup->setContainerLayout(setupParameterWidget());

        GroupContainer* imagesGroup = new GroupContainer();
        imagesGroup->setTitle("Choose the images to sync");
        imagesGroup->setContainerLayout(setupImageSelectionLayout());

        QLabel* mainLabel = new QLabel("Synchronize Image Parameters");
        QFont font = mainLabel->font();
        font.setPointSize(18);
        font.setBold(true);
        mainLabel->setFont(font);
        
        QHBoxLayout* topLayout = new QHBoxLayout;
        topLayout->addStretch(0);
        topLayout->addSpacing(10);
        topLayout->addWidget(mainLabel, 0);
        topLayout->addStretch(1);

        QPushButton* resetButton = new QPushButton("Sync Image Parameters");
        connect(resetButton, &QPushButton::clicked, [ = ](){
            resetParameters();
        });

        QHBoxLayout* bottomLayout = new QHBoxLayout;
        bottomLayout->addStretch(1);
        bottomLayout->addWidget(resetButton, 0);
        bottomLayout->addSpacing(10);
        
        QVBoxLayout* mainLayout = new QVBoxLayout();
        mainLayout->addStretch(0);
        mainLayout->addLayout(topLayout, 0);
        mainLayout->addWidget(parametersGroup, 1);
        mainLayout->addWidget(imagesGroup, 0);
        mainLayout->addLayout(bottomLayout, 0);

        setLayout(mainLayout);
    }

private:

    QHBoxLayout* setupParameterWidget() {
        QScrollArea* selectedWidgetArea = new QScrollArea;
        selectedWidgetArea->setWidgetResizable(true);
        selectedWidgetArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
        selectedWidgetLayout_ = new QFormLayout;
        selectedWidgetLayout_->setRowWrapPolicy(QFormLayout::DontWrapRows);
        selectedWidgetLayout_->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
        selectedWidgetLayout_->setFormAlignment(Qt::AlignLeft | Qt::AlignTop);
        selectedWidgetLayout_->setLabelAlignment(Qt::AlignLeft);
        selectedWidgetLayout_->setVerticalSpacing(0);
        selectedWidgetLayout_->addRow(helpTextLabel());
        
        QWidget* selectedWidget = new QWidget;
        selectedWidget->setLayout(selectedWidgetLayout_);
        selectedWidgetArea->setWidget(selectedWidget);
        
        availableWidget_ = new QTableWidget(0, 2);
        availableWidget_->setSelectionBehavior(QAbstractItemView::SelectRows);
        availableWidget_->setAttribute(Qt::WA_MacShowFocusRect, 0);

        QStringList labels;
        labels << tr("Parameter") << tr("Label");
        availableWidget_->setHorizontalHeaderLabels(labels);
        availableWidget_->verticalHeader()->hide();
        availableWidget_->setShowGrid(false);
        availableWidget_->setAlternatingRowColors(true);

        availableWidget_->setMinimumHeight(300);
        
        QStringList uniqueParams = ProjectData::uniqueParamList();
        ParametersConfiguration* data = projectData.projectParameterData();
        for (QString param : data->getLookupTable().keys()) {
            ParameterElementData* element = data->get(param);
            if (!element->syncWithUpperLevel() &&  element->userLevel() < 2 && !uniqueParams.contains(param.toLower())) {
                QTableWidgetItem* paramItem = new QTableWidgetItem(element->name());
                QTableWidgetItem* labelItem = new QTableWidgetItem(element->label());

                int row = availableWidget_->rowCount();
                availableWidget_->insertRow(row);
                availableWidget_->setItem(row, 0, paramItem);
                availableWidget_->setItem(row, 1, labelItem);
            }
        }

        availableWidget_->resizeColumnsToContents();
        availableWidget_->setSortingEnabled(true);
        availableWidget_->sortByColumn(0, Qt::AscendingOrder);

        QPushButton* addAllButton = new QPushButton("Add all");
        connect(addAllButton, &QPushButton::clicked, [ = ](){
            QStringList paramList;
            for (int i = 0; i < availableWidget_->rowCount(); ++i) {
                QTableWidgetItem* paramItem = availableWidget_->item(i, 0);
                if (paramItem) paramList.append(paramItem->data(Qt::DisplayRole).toString());
            }
            addToSelection(paramList);
        });

        QPushButton* addSelectedButton = new QPushButton("Add selected");
        connect(addSelectedButton, &QPushButton::clicked, [ = ](){
            QStringList paramList;
            for (QModelIndex index : availableWidget_->selectionModel()->selectedRows(0)) {
                paramList.append(index.data(Qt::DisplayRole).toString());
            }
            addToSelection(paramList);
        });

        QWidget* buttonsWidget = new QWidget;
        QHBoxLayout* buttonsLayout = new QHBoxLayout;
        buttonsLayout->setMargin(0);
        buttonsLayout->addStretch(0);
        buttonsLayout->addWidget(addAllButton);
        buttonsLayout->addWidget(addSelectedButton);
        buttonsWidget->setLayout(buttonsLayout);
        
        QPushButton* clearAllButton = new QPushButton("Clear all");
        connect(clearAllButton, &QPushButton::clicked, [ = ](){
            clearSelection();
        });
        
        QWidget* selbuttonsWidget = new QWidget;
        QHBoxLayout* selbuttonsLayout = new QHBoxLayout;
        selbuttonsLayout->setMargin(0);
        selbuttonsLayout->addStretch(0);
        selbuttonsLayout->addWidget(clearAllButton);
        selbuttonsWidget->setLayout(selbuttonsLayout);

        BlockContainer* selectedContainer = new BlockContainer("Parameters to sync", selectedWidgetArea, selbuttonsWidget);
        BlockContainer* availableContainer = new BlockContainer("Non-synced Parameters", availableWidget_, buttonsWidget);

        QHBoxLayout* layout = new QHBoxLayout();
        layout->addWidget(availableContainer);
        layout->addWidget(selectedContainer);

        return layout;
    }

    QHBoxLayout* setupImageSelectionLayout() {
        noneButton_ = new QRadioButton("None");
        selectedButton_ = new QRadioButton("Selected");
        changeSelectionCount(projectData.imagesSelected().count());

        allButton_ = new QRadioButton("All");
        changeImagesCount(projectData.imageList().count());

        connect(&projectData, &ProjectData::selectionChanged, [ = ](const QStringList & paths){
            changeSelectionCount(paths.count());
        });

        connect(&projectData, &ProjectData::imageDirsChanged, [ = ](){
            changeSelectionCount(projectData.imageList().count());
        });

        QButtonGroup* group = new QButtonGroup(this);
        group->setExclusive(true);
        group->addButton(noneButton_);
        group->addButton(selectedButton_);
        group->addButton(allButton_);

        noneButton_->setChecked(true);
        
        QHBoxLayout* layout = new QHBoxLayout();
        layout->addStretch(0);
        layout->addWidget(noneButton_, 0);
        layout->addWidget(selectedButton_, 0);
        layout->addWidget(allButton_, 0);
        layout->addStretch(1);

        return layout;
    }

    void addToSelection(const QStringList& paramList) {
        ParametersConfiguration* data = projectData.projectParameterData();
        QProgressDialog progressDialog;
        progressDialog.setRange(0, paramList.size());
        progressDialog.setWindowTitle("Adding paramters");
        int i = 0;
        for (QString param : paramList) {
            i++;
            progressDialog.setValue(i);
            progressDialog.setLabelText(tr("Adding parameter %1 of %2...").arg(i).arg(paramList.size()));
            qApp->processEvents();

            if (progressDialog.wasCanceled()) break;
            
            if (!paramToElement_.keys().contains(param)) {
                ParameterElementData* element = data->get(param);
                if(element) {
                    ParameterInput* input = new ParameterInput(element);
                    input->load();
                    connect(element, SIGNAL(dataChanged()), input, SLOT(load()));
                    selectedWidgetLayout_->addRow(element->name(), input);
                    paramToElement_.insert(param, element);
                }
            }
        }
        
        progressDialog.reset();
        progressDialog.close();
    }

    void clearSelection() {
        paramToElement_.clear();
        while (selectedWidgetLayout_->count() != 0) {
            QLayoutItem *forDeletion = selectedWidgetLayout_->takeAt(0);
            delete forDeletion->widget();
            delete forDeletion;
        }
        selectedWidgetLayout_->addRow(helpTextLabel());
    }
    
    QLabel* helpTextLabel() {
        QLabel* helpLabel = new QLabel("Add the parameters to be synced from the list. Once added you can change them and sync specific images to global parameters.");
        helpLabel->setWordWrap(true);
        QPalette pal = helpLabel->palette();
        pal.setColor(QPalette::WindowText, Qt::darkGray);
        helpLabel->setPalette(pal);
        return helpLabel;
    }

    void changeSelectionCount(int count) {
        selectedButton_->setText("Sync " + QString::number(count) + " SELECTED images");
    }

    void changeImagesCount(int count) {
        allButton_->setText("Sync ALL " + QString::number(count) + " images");
    }

    void resetParameters() {
        if(noneButton_->isChecked()) {
            QMessageBox::information(this, "No images specified", "Please specify the images to be changed");
            return;
        }
        
        QStringList paramList =  paramToElement_.keys();
        if(paramList.isEmpty()) {
            QMessageBox::information(this, "No parameters specified", "Please specify the parameters to be changed");
            return;
        }
        
        if (QMessageBox::question(this, "Reset image parameters?",
                QString("This will replace the specified parameters in specified images.") +
                QString("\n\nCAREFUL: You might loose the processed results if you have selected such parameter.\n\nARE YOU SURE TO Proceed?"), "Yes", "No", QString(), 0, 1) == 0) {
            QStringList imList;
            if(selectedButton_->isChecked()) imList = projectData.imagesSelected();
            else if(allButton_->isChecked()) imList = projectData.imageList();

            QProgressDialog progressDialog;
            progressDialog.setRange(0, imList.size()); // -1 because a merge dir is present in project folder!!);
            progressDialog.setWindowTitle("Reseting Images");

            for (int i = 0; i < imList.size(); ++i) {

                progressDialog.setValue(i + 1);
                progressDialog.setLabelText(tr("Reseting image %1 of %2...").arg(i + 1).arg(imList.size()));
                qApp->processEvents();

                if (progressDialog.wasCanceled()) break;

                QString selected = imList[i];

                //Copy to backup location
                QFile(selected + "/2dx_image.cfg").copy(selected + "/2dx_image.cfg-backup4");

                ParametersConfiguration* conf = projectData.parameterData(QDir(selected));

                if (conf) {
                    for(QString param :  paramList) {
                        conf->set(param, paramToElement_[param]->value().toString());
                    }
                    conf->setModified(true);
                }
            }

            progressDialog.reset();
            progressDialog.close();
            QMessageBox::information(NULL, "Parameters were reset", "The specified parameters and images were reset.\n\nIf this was a mistake, you can still use the Backup or Restore Databases script to recover the last versions.");
        }

    }

    QTableWidget* availableWidget_;
    QFormLayout* selectedWidgetLayout_;

    QRadioButton* noneButton_;
    QRadioButton* selectedButton_;
    QRadioButton* allButton_;
    
    QMap<QString, ParameterElementData*> paramToElement_;

};

#endif /* IMAGECONFIGCHANGER_H */

