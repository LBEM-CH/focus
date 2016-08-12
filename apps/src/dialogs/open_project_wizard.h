/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   open_project_wizard.h
 * Author: biyanin
 *
 * Created on August 3, 2016, 4:28 PM
 */

#ifndef OPEN_PROJECT_WIZARD_H
#define OPEN_PROJECT_WIZARD_H

#include <QWizard>
#include <QWizardPage>
#include <QLineEdit>
#include <QFormLayout>
#include <QDir>
#include <QListWidget>
#include <QGridLayout>
#include <QAbstractItemView>
#include <QPushButton>
#include <QLabel>
#include <QFileDialog>
#include <QMessageBox>
#include <QInputDialog>
#include <QDebug>

#include "user_projects.h"
#include "project_preferences.h"

#include "confData.h"

class OpenProjectWizard : public QWizard {

    Q_OBJECT

public:

    OpenProjectWizard(QWidget* parent = 0)
    : QWizard(parent) {

        optionsPage_ = new OptionsPage(this);
        addPage(optionsPage_);
        setWindowTitle(tr("Open Project"));
        setModal(true);
    }

    void accept() override {
        emit projectSelected(optionsPage_->projectPath());
        QDialog::accept();
    }

    void reject() override {
        qApp->closeAllWindows();
        QDialog::reject();
    }


signals:

    void projectSelected(const QString& projectPath);

private:

    class OptionsPage : public QWizardPage {
    public:

        OptionsPage(QWidget* parent)
        : QWizardPage(parent) {

            setTitle("Open project");
            setSubTitle("Select the project from list or add new project");
            setPixmap(QWizard::BackgroundPixmap, QPixmap(getAppDir()+"resources/images/background.jpg"));
            setPixmap(QWizard::LogoPixmap, QPixmap(getAppDir()+"resources/images/logo.png"));

            QLabel* label = new QLabel("Available Projects");
            QPushButton* createProject = new QPushButton("Add Project");
            connect(createProject, &QPushButton::clicked,
                    [ = ](){
                createNewProject();
            });

            listWidget_ = new QListWidget(this);
            listWidget_->setSelectionMode(QAbstractItemView::SingleSelection);
            QPalette pal = listWidget_->palette();
            pal.setColor(QPalette::Highlight, Qt::gray);
            listWidget_->setPalette(pal);
            
            connect(listWidget_, SIGNAL(itemSelectionChanged()), this, SIGNAL(completeChanged()));

            QGridLayout* layout = new QGridLayout;
            layout->addWidget(label, 0, 0, 1, 1, Qt::AlignLeft | Qt::AlignVCenter);
            layout->addWidget(createProject, 0, 1, 1, 1, Qt::AlignRight | Qt::AlignVCenter);
            layout->addWidget(listWidget_, 1, 0, 1, 2);

            updateListWiget();
            
            setLayout(layout);

        }

        QString projectPath() {
            if (listWidget_->selectedItems().isEmpty()) return QString();
            else {
                QString displayed = listWidget_->selectedItems()[0]->data(Qt::DisplayRole).toString();
                return displayed.split('(')[1].remove(')');
            }
        }
        
        bool isComplete() const override {
            return (!(listWidget_->selectedItems().isEmpty()));
        }

    private:
        QListWidget* listWidget_;

        void updateListWiget() {
            listWidget_->clear();
            QStringList projectPaths = UserProjects(getUserPath()).projectPaths();
            for(int i=0; i< projectPaths.size(); ++i) {
                listWidget_->addItem(ProjectPreferences(projectPaths[i]).projectName() + " (" + projectPaths[i] + ")");
            }
            if(!projectPaths.isEmpty()) listWidget_->setCurrentRow(0);
        }
        
        QString getUserPath() {
            return QDir::homePath() + "/.2dx";
        }

        QString getAppDir() {
            QString appDir = QApplication::applicationDirPath();
            QString sep = "/../";
#ifdef Q_OS_MAC
            appDir += "/../../../";
#endif
            int tries = 0;
            while (!QFileInfo(appDir + sep + "resources/config/2dx_master.cfg").exists() && tries < 3) {
                qDebug() << (appDir + sep + "resources/config/2dx_master.cfg") << " does not exist!";
                sep += "../";
                tries++;
            }
            if (QFileInfo(appDir + sep + "resources/config/2dx_master.cfg").exists()) {
                return QString(appDir + sep);
            } else
                return QString();

        }

        void initializeProject(const QString &workingDir) {
            QDir dir(workingDir);
            dir.mkdir("merge");
            dir.mkpath("merge/proc");
            dir.mkpath("merge/LOGS");
            confData data(workingDir + "/merge/" + "2dx_merge.cfg", getAppDir() + "resources/config/2dx_master.cfg");
            data.setSymLink("merge/2dx_merge.cfg", workingDir + "/2dx_master.cfg");
            data.save();
        }

        void createNewProject() {
            QString projectDir = QFileDialog::getExistingDirectory(this, "Select a Project Directory");
            if (projectDir.isEmpty() || !QDir(projectDir).exists()) return;

            if (!QFileInfo(projectDir + "/merge/" + "2dx_merge.cfg").exists()) {
                quint32 choice = QMessageBox::question(NULL, "Confirm Create new Project?", "No configuration data found in:\n" + projectDir + "\n\nCreate new config files in this directory?", "Create", "Cancel", QString(), 0, 1);
                if (choice) return;
                else {
                    initializeProject(projectDir);
                }
            }
            else {
                QMessageBox::warning(this, "Project Exists!", "This folder already has 2DX configuration files");
            }
            
            UserProjects(getUserPath()).addProjectPath(projectDir);

            bool ok;
            QString projectName = QInputDialog::getText(this, "Project Name", "Enter a name for the project", QLineEdit::Normal,
                    ProjectPreferences(projectDir).projectName(), &ok);

            if (ok && !projectName.isEmpty()) {
                ProjectPreferences(projectDir).setProjectName(projectName);
            }

            updateListWiget();

        }

    };

    OptionsPage* optionsPage_;

};

#endif /* OPEN_PROJECT_WIZARD_H */

