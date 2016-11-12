#ifndef OPEN_PROJECT_WIZARD_H
#define OPEN_PROJECT_WIZARD_H

#include <QWizard>
#include <QDebug>

#include "UserProjects.h"
#include "UserPreferences.h"
#include "SelectProjectPage.h"
#include "AddProjectPage.h"
#include "ProjectData.h"
#include "ProjectLoadPage.h"
#include "MainWindow.h"
#include "GraphicalButton.h"

class StartupWizard : public QWizard {

    Q_OBJECT

public:

    enum PageId{
        SELECT_PROJECT, ADD_PROJECT, PROJECT_LOAD
    };

    StartupWizard(QWidget* parent = 0)
    : QWizard(parent) {
        
        //Set some options
        setOption(QWizard::NoDefaultButton, false);
        setOption(QWizard::NoBackButtonOnStartPage, true);
        setOption(QWizard::NoBackButtonOnLastPage, true);
        setOption(QWizard::NoCancelButtonOnLastPage, true);
        
        //Disable the finish button
        GraphicalButton* button = new GraphicalButton(QIcon());
        button->setVisible(false);
        setButton(QWizard::FinishButton, button);
        
        UserPreferences().loadStarupDialogPreferences(this);

        projectPaths_ = UserProjects().projectPaths();
        
        setWindowTitle(tr("Project initialization"));
        setPage(PageId::SELECT_PROJECT, new SelectProjectPage(projectPaths_, this));
        setPage(PageId::ADD_PROJECT, new AddProjectPage(this));
        setPage(PageId::PROJECT_LOAD, new ProjectLoadPage(this));

        setStartId(PageId::SELECT_PROJECT);
        
        connect(this, &QWizard::currentIdChanged, [=](int id){
            if(id == PageId::PROJECT_LOAD) loadProject();
        });
        
        connect(&projectData, &ProjectData::startupFinished, this, &QDialog::accept);
    }
    
    void accept() override {
        UserPreferences().saveStarupDialogPreferences(this);
        QDialog::accept();
    }


    QString projectPath() {
        if (field("selectProjectpage.listWidget").toInt() == 0) return field("addProjectPage.path").toString();
        else return projectPaths_[field("selectProjectpage.listWidget").toInt()-1];
    }

    int nextId() const override {
        switch (currentId()) {
            case PageId::SELECT_PROJECT:
                if (field("selectProjectpage.listWidget").toInt() == 0) return PageId::ADD_PROJECT;
                else return PageId::PROJECT_LOAD;
            case PageId::ADD_PROJECT:
                return PageId::PROJECT_LOAD;
            case PageId::PROJECT_LOAD:
            default:
                return -1;
        }
    }
    
    void loadProject() {
        projectData.initiailze(QDir(projectPath()));
        UserProjects().addProjectPath(projectData.projectDir().canonicalPath());
        MainWindow *win = new MainWindow(projectData.projectDir().canonicalPath());
        win->show();
        win->raise(); // raises the window on top of the parent widget stack
        win->activateWindow(); // activates the window an thereby putting it on top-level
    }

    
private:   
    QStringList projectPaths_;

};

#endif /* OPEN_PROJECT_WIZARD_H */

