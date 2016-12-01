
#ifndef ADDPROJECTPAGE_H
#define ADDPROJECTPAGE_H

#include <QWizardPage>
#include <QFormLayout>
#include <QDir>
#include <QLineEdit>
#include <QList>
#include <QRadioButton>
#include <QButtonGroup>
#include <QMessageBox>

#include "UserProjects.h"
#include "BrowserWidget.h"
#include "ProjectMode.h"
#include "ProjectPreferences.h"
#include "ParameterConfiguration.h"

class AddProjectPage : public QWizardPage {
public:

    AddProjectPage(QWidget* parent = 0) :
    QWizardPage(parent) {

        setTitle("Add project");
        setSubTitle("Define as Project Path a location on a large harddrive, preferably on the local computer. THIS IS USUALLY NOT ON THE CAMERA COMPUTER.");
        setPixmap(QWizard::BackgroundPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/background.png"));
        setPixmap(QWizard::LogoPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/logo.png").scaledToHeight(40));

        QFormLayout* layout = new QFormLayout;

        newProjectWidget_ = new BrowserWidget(BrowserWidget::BrowseType::DIRECTORY, QDir::homePath());
        registerField("addProjectPage.path*", newProjectWidget_->pathLineEditWidget());
        connect(newProjectWidget_, &BrowserWidget::pathChanged, [ = ]{
            updateFields();
        });
        layout->addRow("Project path", newProjectWidget_);

        projectNameEdit_ = new QLineEdit();
        projectNameEdit_->setEnabled(false);
        projectNameEdit_->setFrame(false);
        connect(projectNameEdit_, &QLineEdit::textEdited, [ = ]{
            ProjectPreferences(QDir(newProjectWidget_->path())).setProjectName(projectNameEdit_->text());
        });
        layout->addRow("Project name", projectNameEdit_);

        //Setup modes
        modebuttonGroup_ = new QButtonGroup(this);
        modebuttonGroup_->setExclusive(true);
        modeWidget_ = new QWidget;
        QVBoxLayout* buttonLayout = new QVBoxLayout;
        buttonLayout->addStretch(0);
        modeWidget_->setLayout(buttonLayout);
        modeWidget_->setEnabled(false);

        QList<ProjectMode> modes = ProjectMode::availableModes();
        for (ProjectMode mode : modes) {
            QRadioButton* button = new QRadioButton;
            button->setText(mode.toString());
            button->setIcon(mode.getIcon());
            button->setCheckable(true);
            button->setChecked(false);
            connect(button, &QRadioButton::toggled, [ = ](bool checked){
                if (checked) {
                    int id = modeButtons_.indexOf(button);
                    ProjectPreferences(QDir(newProjectWidget_->path())).setProjectMode(id);
                }
            });

            modeButtons_.append(button);
            modebuttonGroup_->addButton(button);

            buttonLayout->addWidget(button, 0);
        }

        layout->addRow("Project mode", modeWidget_);

        setLayout(layout);
    }

private:

    void updateFields() {
        QDir projectDir = QDir(newProjectWidget_->path());

        if (projectDir.exists()) {

            //Test for existing project
            if (!QFileInfo(projectDir.canonicalPath() + "/merge/" + "2dx_merge.cfg").exists()) {
                QMessageBox::information(this, "Creating new Project?", "No configuration data found in:\n" + projectDir.canonicalPath() + "\n\nA new project will be created.");
                initializeProject(projectDir.canonicalPath());
            } else {
                QMessageBox::information(this, "Project Exists!", "This folder already has configuration files");
            }

            UserProjects().addProjectPath(projectDir.canonicalPath());

            ProjectPreferences pref(projectDir);
            modeWidget_->setEnabled(true);
            projectNameEdit_->setEnabled(true);

            projectNameEdit_->setText(pref.projectName());
            int modeId = pref.projectMode();
            if (modeId < 0 || modeId > modeButtons_.size()) modeId = 0;
            modeButtons_[modeId]->setChecked(true);
        } else {
            modeWidget_->setEnabled(false);
            projectNameEdit_->setEnabled(false);
        }
    }

    void initializeProject(const QString &projectDir) {
        QDir dir(projectDir);
        dir.mkdir("merge");
        dir.mkpath("merge/proc");
        dir.mkpath("merge/LOGS");
        ParametersConfiguration data(ApplicationData::applicationDir().canonicalPath() + "/resources/config/2dx_master.cfg", projectDir + "/merge/" + "2dx_merge.cfg");
        data.save();
        QFile(projectDir + "/merge/" + "2dx_merge.cfg").link("merge/2dx_merge.cfg", projectDir + "/2dx_master.cfg");
    }

    BrowserWidget* newProjectWidget_;
    QLineEdit* projectNameEdit_;
    QList<QRadioButton*> modeButtons_;
    QButtonGroup* modebuttonGroup_;
    QWidget* modeWidget_;


};

#endif /* ADDPROJECTPAGE_H */

