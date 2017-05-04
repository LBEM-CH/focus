
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
#include <QInputDialog>

#include "UserProjects.h"
#include "BrowserWidget.h"
#include "ProjectMode.h"
#include "ProjectPreferences.h"
#include "ParameterConfiguration.h"

class AddProjectPage : public QWizardPage {
public:

    AddProjectPage(const QStringList& projectPaths, QWidget* parent = 0) :
    QWizardPage(parent) {

        projectPaths_ = projectPaths;
        
        setTitle("Add project");
        setSubTitle("Define as Project Path a location on a large harddrive, preferably on the local computer. THIS IS USUALLY NOT ON THE CAMERA COMPUTER.");
        setPixmap(QWizard::BackgroundPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/background.png"));
        setPixmap(QWizard::LogoPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/logo.png").scaledToHeight(40));

        QFormLayout* layout = new QFormLayout;
        layout->setRowWrapPolicy(QFormLayout::DontWrapRows);
        layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
        layout->setFormAlignment(Qt::AlignLeft | Qt::AlignVCenter);
        layout->setLabelAlignment(Qt::AlignLeft);
        layout->setVerticalSpacing(0);

        newProjectWidget_ = new BrowserWidget(BrowserWidget::BrowseType::DIRECTORY, QDir::currentPath());
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
                QString text = "No configuration data found in:\n" + projectDir.canonicalPath() + "\n\nSelect how do you want to initialize the configuration files:";
                
                QString copyButtonStr;
                if(!projectPaths_.isEmpty()) copyButtonStr = "Copy from existing project";
                
                int response = QMessageBox::question(this, "Creating new Project?", text, "Don't create project", "Initialize from master", copyButtonStr, 1, 0);
                
                if(response == 2) {
                    createFromProject(projectDir.canonicalPath());
                } else if(response == 1) {
                    initializeProject(projectDir.canonicalPath());
                } else {
                    newProjectWidget_->setPath("");
                    modeWidget_->setEnabled(false);
                    projectNameEdit_->setEnabled(false);
                }
                
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
    
    void createFromProject(const QString& newProjectDir) {
        QDir dir(newProjectDir);
        dir.mkdir("merge");
        dir.mkpath("merge/proc");
        dir.mkpath("merge/LOGS");
        dir.mkpath("merge/config");
        
        QStringList projects;
        for(QString project : projectPaths_) {
            QString name = ProjectPreferences(project).projectName();
            QString mode = ProjectMode(ProjectPreferences(project).projectMode()).toString();
            projects.append(name + '(' + mode + ") | " + project);
            
        }
        
        bool ok;
        QString response = QInputDialog::getItem(this, "Select the project", "Which project do you want to copy the files from: ", projects, 0 , false, &ok);
        
        if(ok) {
            QString copyPath = response.split('|')[1].trimmed();
            
            //Copy the parameters file:
            QFile::copy(copyPath + "/merge/2dx_merge.cfg", newProjectDir + "/merge/2dx_merge.cfg");
            
            //Copy other project config files
            //qDebug() << "Copying: " << copyPath + "/merge/config/project.preferences.ini" << newProjectDir + "/merge/config/project.preferences.ini";
            QFile::copy(copyPath + "/merge/config/project.preferences.ini", newProjectDir + "/merge/config/project.preferences.ini");
            //qDebug() << "Copying: " << copyPath + "/merge/config/projectMenu.inf" << newProjectDir + "/merge/config/projectMenu.inf";
            QFile::copy(copyPath + "/merge/config/projectMenu.inf", newProjectDir + "/merge/config/projectMenu.inf");
            
            //Create a link
            QFile(newProjectDir + "/merge/" + "2dx_merge.cfg").link("merge/2dx_merge.cfg", newProjectDir + "/2dx_master.cfg");
        }
        
        if(!ok || !QFileInfo(newProjectDir + "/merge/2dx_merge.cfg").exists()){
            initializeProject(newProjectDir);
            QMessageBox::information(this, "Initialized with master", "Either no project was selected or there were problems in copying, so project preferences have been initialized by the master.");
        }
        
    }

    void initializeProject(const QString &projectDir) {
        QDir dir(projectDir);
        dir.mkdir("merge");
        dir.mkpath("merge/proc");
        dir.mkpath("merge/LOGS");
        
        //Create dummy config file
        QFile data(projectDir + "/merge/2dx_merge.cfg");
        if (!data.open(QIODevice::WriteOnly | QIODevice::Text)) {
            return;
        }
        data.write("#\n#=============================================================================\n#\n", 83);
        data.close();
        
        QFile(projectDir + "/merge/" + "2dx_merge.cfg").link("merge/2dx_merge.cfg", projectDir + "/2dx_master.cfg");
    }

    BrowserWidget* newProjectWidget_;
    QLineEdit* projectNameEdit_;
    QList<QRadioButton*> modeButtons_;
    QButtonGroup* modebuttonGroup_;
    QWidget* modeWidget_;
    QStringList projectPaths_;


};

#endif /* ADDPROJECTPAGE_H */

