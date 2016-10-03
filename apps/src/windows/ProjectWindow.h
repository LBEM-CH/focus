#ifndef PROJECTWINDOW_H
#define PROJECTWINDOW_H

#include <QWidget>
#include <QLabel>
#include <QLayout>
#include <QGridLayout>
#include <QVBoxLayout>
#include <QFont>
#include <QFrame>

#include "ExecutionWindow.h"
#include "ApplicationData.h"
#include "ProjectData.h"
#include "AutoImportWindow.h"

class ProjectWindow : public QWidget {

    Q_OBJECT

public:
    ProjectWindow(QWidget* parent)
    : QWidget(parent) {
        
        QVBoxLayout* mainLayout = new QVBoxLayout;
        mainLayout->setAlignment(Qt::AlignLeft);
        mainLayout->setSpacing(0);
        mainLayout->setMargin(0);
        mainLayout->addStretch(0);
        
        mainLayout->addLayout(setupTitleContainer(), 0);
        mainLayout->addLayout(setupProjectActions(), 0);
        
        QFrame* hLine = new QFrame(this);
        hLine->setFrameStyle(QFrame::HLine | QFrame::Sunken);
        mainLayout->addWidget(hLine);
        
        exeWindow_ = new ExecutionWindow(projectData.projectWorkingDir(), QDir(ApplicationData::scriptsDir().canonicalPath() + "/project"), this);
        AutoImportWindow* importWin = new AutoImportWindow(this);
        exeWindow_->addToMainToolBar(importWin, ApplicationData::icon("import"), "Import", true);
        
        mainLayout->addWidget(exeWindow_, 1);
        
        setLayout(mainLayout);
        
        connect(&projectData, &ProjectData::projectNameChanged, [ = ](const QString & name){
            setProjectTitle(name);
        });

    };

public slots:

    void setProjectTitle(const QString& title) {
        projectNameLabel_->setText(title);
    }

    void setProjectPath(const QString& path) {
        projectPathLabel_->setText(path);
    }

private:

    QGridLayout* setupTitleContainer() {
        QGridLayout* layout = new QGridLayout;
        layout->setAlignment(Qt::AlignLeft);
        layout->setMargin(5);
        layout->setSpacing(5);

        projectNameLabel_ = new QLabel;
        projectNameLabel_->setText(projectData.projectName());
        QFont nameFont = projectNameLabel_->font();
        nameFont.setPointSize(20);
        nameFont.setBold(true);
        projectNameLabel_->setFont(nameFont);
        layout->addWidget(projectNameLabel_, 0, 0);

        QFrame* vLine = new QFrame(this);
        vLine->setFrameStyle(QFrame::VLine | QFrame::Plain);
        layout->addWidget(vLine, 0, 1);

        projectPathLabel_ = new QLabel;
        projectPathLabel_->setText(projectData.projectDir().canonicalPath());
        QFont pathFont = projectPathLabel_->font();
        pathFont.setPointSize(16);
        projectPathLabel_->setFont(pathFont);
        layout->addWidget(projectPathLabel_, 0, 2);

        for(int i=0; i< layout->columnCount(); ++i) layout->setColumnStretch(i, 0);
        
        return layout;
    }

    QGridLayout* setupProjectActions() {
        QGridLayout* layout = new QGridLayout;
        layout->setAlignment(Qt::AlignLeft);
        layout->setSpacing(5);
        layout->setMargin(5);

        QPushButton* reindexButton = new QPushButton(ApplicationData::icon("refresh"), "Re-index Images");
        connect(reindexButton, &QPushButton::clicked, &projectData, &ProjectData::indexImages);
        layout->addWidget(reindexButton, 0, 0);

        QPushButton* changeNameButton = new QPushButton(ApplicationData::icon("rename"), "Rename Project");
        connect(changeNameButton, &QPushButton::clicked, &projectData, &ProjectData::changeProjectName);
        layout->addWidget(changeNameButton, 0, 1);
        
        layout->addItem(new QSpacerItem(30, 10), 0, 2);
        
        QPushButton* repiarLinksButton = new QPushButton(ApplicationData::icon("repair"), "Repair Project Links");
        connect(repiarLinksButton, &QPushButton::clicked, &projectData, &ProjectData::repairLinks);
        layout->addWidget(repiarLinksButton, 0, 3);
        
        QPushButton* resetImageConfigsButton = new QPushButton(ApplicationData::icon("reset"), "Reset Image Parameters");
        connect(resetImageConfigsButton, &QPushButton::clicked, &projectData, &ProjectData::resetImageConfigs);
        layout->addWidget(resetImageConfigsButton, 0, 4);
        
        layout->addItem(new QSpacerItem(30, 10), 0, 5);

        QPushButton* renumberButton = new QPushButton(ApplicationData::icon("renumber"), "Renumber Images");
        connect(renumberButton, &QPushButton::clicked, &projectData, &ProjectData::renumberImages);
        layout->addWidget(renumberButton, 0, 6);

        QPushButton* evenOddButton = new QPushButton(ApplicationData::icon("odd"), "Assign Even/Odd");
        connect(evenOddButton, &QPushButton::clicked, &projectData, &ProjectData::assignEvenOdd);
        layout->addWidget(evenOddButton, 0, 7);
        
        for(int i=0; i< layout->columnCount(); ++i) layout->setColumnStretch(i, 0);

        return layout;
    }

    QLabel* projectNameLabel_;
    QLabel* projectPathLabel_;

    ExecutionWindow* exeWindow_;

};


#endif /* PROJECTWINDOW_H */

