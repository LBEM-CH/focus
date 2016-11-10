#ifndef SELECTPROJECTWIZARDPAGE_H
#define SELECTPROJECTWIZARDPAGE_H

#include <QWizardPage>
#include <QLineEdit>
#include <QDir>
#include <QListWidget>
#include <QListWidgetItem>
#include <QGridLayout>
#include <QStringList>

class SelectProjectPage : public QWizardPage {
public:

    SelectProjectPage(QStringList projectPaths, QWidget* parent=0);

private:

    void updateListWidget(QStringList projectPaths);
    
    QListWidget* listWidget_;

};

#endif /* SELECTPROJECTWIZARDPAGE_H */

