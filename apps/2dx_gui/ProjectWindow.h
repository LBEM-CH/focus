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
#include "ImageConfigChanger.h"
#include "GraphicalButton.h"

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
        
        QGridLayout* titleLayout = new QGridLayout;
        titleLayout->setSpacing(0);
        titleLayout->setMargin(0);
        
        GraphicalButton* modeIcon = new GraphicalButton(projectData.projectMode().getIcon());
        modeIcon->setFixedSize(64, 64);
        titleLayout->addWidget(modeIcon, 0, 0, 2, 1);
        titleLayout->addLayout(setupTitleContainer(), 0, 1, 1, 1);
        titleLayout->addLayout(setupProjectActions(), 1, 1, 1, 1);
        
        mainLayout->addLayout(titleLayout);
        
        QFrame* hLine = new QFrame(this);
        hLine->setFrameStyle(QFrame::HLine | QFrame::Sunken);
        mainLayout->addWidget(hLine);
        
        exeWindow_ = new ExecutionWindow(QDir(ApplicationData::scriptsDir().canonicalPath() + "/project"));
        
        ImageConfigChanger* configChanger = new ImageConfigChanger(this);
        exeWindow_->addToMainToolBar(configChanger, ApplicationData::icon("sync"), "Sync", true);
        
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
        QString toBeDisplayed = path;
        if(path.length() > 100) toBeDisplayed.remove(0, path.length()-100);
        projectPathLabel_->setText(toBeDisplayed);
    }

private:

    QHBoxLayout* setupTitleContainer() {
        QHBoxLayout* layout = new QHBoxLayout;
        layout->setMargin(5);
        layout->setSpacing(5);
        layout->addStretch(0);

        projectNameLabel_ = new QLabel;
        projectNameLabel_->setText(projectData.projectName());
        QFont nameFont = projectNameLabel_->font();
        nameFont.setPointSize(16);
        nameFont.setBold(true);
        projectNameLabel_->setFont(nameFont);
        layout->addWidget(projectNameLabel_, 0);

        QFrame* vLine = new QFrame(this);
        vLine->setFrameStyle(QFrame::VLine | QFrame::Plain);
        layout->addWidget(vLine, 0);
        
        QLabel* modeLabel = new QLabel(projectData.projectMode().toString() + " Project");
        QFont modeFont = modeLabel->font();
        modeFont.setPointSize(16);
        modeLabel->setFont(modeFont);
        layout->addWidget(modeLabel, 0);
        
        layout->addStretch(1);

        projectPathLabel_ = new QLabel;
        projectPathLabel_->setTextInteractionFlags(Qt::TextSelectableByMouse);
        setProjectPath(projectData.projectDir().canonicalPath());
        layout->addWidget(projectPathLabel_, 0);

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

