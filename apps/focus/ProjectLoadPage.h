
#ifndef PROJECTLOADPAGE_H
#define PROJECTLOADPAGE_H

#include <QWizardPage>
#include <QVariant>
#include <QString>
#include <QIcon>
#include <QLabel>
#include <QFont>
#include <QEventLoop>

#include "ApplicationData.h"
#include "ProjectData.h"

class ProjectLoadPage : public QWizardPage {
    
public:
    ProjectLoadPage(QWidget* parent):
    QWizardPage(parent) {
        setTitle("Loading project");
        setSubTitle("Please wait while the project is being loaded");
        setPixmap(QWizard::BackgroundPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/background.png"));
        setPixmap(QWizard::LogoPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/logo.png").scaledToHeight(100));
        setFinalPage(true);
        
        QGridLayout* mainLayout = new QGridLayout;
        
        registeredStatus_ = new LoadStatusData(this);
        registeredStatus_->label->setText("Registering parameters");
        addToLayout(mainLayout, registeredStatus_, 0);
        
        findImagesStatus_ = new LoadStatusData(this);
        findImagesStatus_->label->setText("Finding images from disk");
        addToLayout(mainLayout, findImagesStatus_, 1);
        
        initImagesStatus_ = new LoadStatusData(this);
        initImagesStatus_->label->setText("Initializing image parameters");
        addToLayout(mainLayout, initImagesStatus_, 2);
        
        loadLibraryStatus_ = new LoadStatusData(this);
        loadLibraryStatus_->label->setText("Loading Library");
        addToLayout(mainLayout, loadLibraryStatus_, 3);
        
        finalizeStatus_ = new LoadStatusData(this);
        finalizeStatus_->label->setText("Placing the widgets");
        addToLayout(mainLayout, finalizeStatus_, 4);
        
        connect(&projectData, &ProjectData::parametersRegistered, [=]{
            registeredStatus_->status->setText("Done.");
            registeredStatus_->icon->resetIcon(ApplicationData::icon("process_done"));
            findImagesStatus_->icon->resetIcon(ApplicationData::icon("process_executing"));
            QApplication::processEvents();
        });
        
        connect(&projectData, &ProjectData::groupsInitializationStatus, [=](const QString& stat) {
            findImagesStatus_->status->setText(stat);
            qApp->processEvents();
        });
        
        connect(&projectData, &ProjectData::groupsInitialized, [=](int imagesFound){
            findImagesStatus_->icon->resetIcon(ApplicationData::icon("process_done"));
            initImagesStatus_->icon->resetIcon(ApplicationData::icon("process_executing"));
            findImagesStatus_->status->setText(QString::number(imagesFound) + " images found.");
            findImagesStatus_->status->repaint();
            findImagesStatus_->icon->repaint();
            initImagesStatus_->icon->repaint();
            qApp->processEvents();
        });
        
        connect(&projectData, &ProjectData::imageInitializationStatus, [=](const QString& stat) {
            initImagesStatus_->status->setText(stat);
            initImagesStatus_->status->repaint();
        });
        
        connect(&projectData, &ProjectData::imagesInitialized, [=]{
            initImagesStatus_->icon->resetIcon(ApplicationData::icon("process_done"));
            loadLibraryStatus_->icon->resetIcon(ApplicationData::icon("process_executing"));
            initImagesStatus_->status->setText("Done.");
            initImagesStatus_->status->repaint();
            initImagesStatus_->icon->repaint();
            loadLibraryStatus_->icon->repaint();
            qApp->processEvents();
        });
        
        connect(&projectData, &ProjectData::libraryLoaded, [=]() {
            loadLibraryStatus_->icon->resetIcon(ApplicationData::icon("process_done"));
            finalizeStatus_->icon->resetIcon(ApplicationData::icon("process_executing"));
            loadLibraryStatus_->status->setText("Done.");
            loadLibraryStatus_->status->repaint();
            loadLibraryStatus_->icon->repaint();
            finalizeStatus_->icon->repaint();
            qApp->processEvents();
        });
        
        setLayout(mainLayout);
        
    }
    
private:
    
    class LoadStatusData : public QWidget{
    public:
        LoadStatusData(QWidget* parent):
        QWidget(parent){
            icon = new GraphicalButton(ApplicationData::icon("process_wait"));
            icon->setFixedSize(20, 20);
            label = new QLabel;
            QFont font = label->font();
            font.setBold(true);
            label->setFont(font);
            
            status = new QLabel("Waiting...");
        }
        
        GraphicalButton* icon;
        QLabel* label;
        QLabel* status;
    };
    
    void addToLayout(QGridLayout* layout, LoadStatusData* data, int row) {
        layout->addWidget(data->icon, row, 0);
        layout->addWidget(data->label, row, 1);
        layout->addWidget(data->status, row, 2);
    }
    
    LoadStatusData* registeredStatus_;
    LoadStatusData* findImagesStatus_;
    LoadStatusData* initImagesStatus_;
    LoadStatusData* loadLibraryStatus_;
    LoadStatusData* finalizeStatus_;
    
};

#endif

