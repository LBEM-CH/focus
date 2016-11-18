#include "SelectProjectPage.h"
#include "ProjectMode.h"
#include "TitleDescriptionWidget.h"
#include "UserProjects.h"
#include "ProjectPreferences.h"
#include "ApplicationData.h"

SelectProjectPage::SelectProjectPage(QStringList projectPaths, QWidget* parent)
: QWizardPage(parent) {

    setTitle("Open project");
    setSubTitle("Select the project from list or add new project");
    setPixmap(QWizard::BackgroundPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/background.png"));
    setPixmap(QWizard::LogoPixmap, QPixmap(ApplicationData::imagesDir().canonicalPath() + "/logo.png").scaledToHeight(40));

    listWidget_ = new QListWidget(this);
    listWidget_->setSelectionMode(QAbstractItemView::SingleSelection);
    listWidget_->setAlternatingRowColors(true);
    QPalette pal = listWidget_->palette();
    pal.setColor(QPalette::Highlight, Qt::gray);
    listWidget_->setPalette(pal);
    registerField("selectProjectpage.listWidget", listWidget_);
    
    QGridLayout* layout = new QGridLayout;
    layout->addWidget(listWidget_);

    updateListWidget(projectPaths);

    setLayout(layout);

}

void SelectProjectPage::updateListWidget(QStringList projectPaths) {
    listWidget_->clear();

    //Add the add project item
    QListWidgetItem* item = new QListWidgetItem();
    item->setSizeHint(QSize(0,46));
    TitleDescriptionWidget* widget = new TitleDescriptionWidget();

    //set data
    widget->setIcon(ApplicationData::icon("add_project"));
    widget->setTitle("Add Project");
    widget->setDescription("Create new or add an existing project");
    listWidget_->addItem(item);
    listWidget_->setItemWidget(item, widget);

    for (int i = 0; i < projectPaths.size(); ++i) {
        QListWidgetItem* item = new QListWidgetItem();
        item->setSizeHint(QSize(0,46));
        item->setData(Qt::UserRole + 2, projectPaths[i]);
        TitleDescriptionWidget* widget = new TitleDescriptionWidget();

        //set data
        ProjectPreferences pref(projectPaths[i]);
        ProjectMode mode(pref.projectMode());
        widget->setIcon(mode.getIcon());
        widget->setTitle(pref.projectName());
        widget->setDescription(projectPaths[i]);
        listWidget_->addItem(item);
        listWidget_->setItemWidget(item, widget);
    }
    
    if (!projectPaths.isEmpty()) listWidget_->setCurrentRow(1);
    else listWidget_->setCurrentRow(0);
}