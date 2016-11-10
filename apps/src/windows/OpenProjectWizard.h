
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

#include "UserProjects.h"
#include "ProjectPreferences.h"
#include "ApplicationData.h"
#include "ParameterConfiguration.h"

class OpenProjectWizard : public QWizard {

    Q_OBJECT

public:

    OpenProjectWizard(QWidget* parent = 0)
    : QWizard(parent) {

        optionsPage_ = new OptionsPage(this);
        addPage(optionsPage_);
        setWindowTitle(tr("Open Project"));
        //setModal(true);
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
            setPixmap(QWizard::BackgroundPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/background.png"));
            setPixmap(QWizard::LogoPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/logo.png").scaledToHeight(100));

            QLabel* label = new QLabel("Available Projects");
            QPushButton* createProject = new QPushButton("Add Project");
            createProject->setAutoDefault(false);
            createProject->setDefault(false);
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
            QStringList projectPaths = UserProjects().projectPaths();
            for(int i=0; i< projectPaths.size(); ++i) {
                listWidget_->addItem(ProjectPreferences(projectPaths[i]).projectName() + " (" + projectPaths[i] + ")");
            }
            if(!projectPaths.isEmpty()) listWidget_->setCurrentRow(0);
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
            
            UserProjects().addProjectPath(projectDir);

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

