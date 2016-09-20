
#ifndef LIBRARYIMAGESTATUS_H
#define LIBRARYIMAGESTATUS_H

#include <QWidget>
#include <QLabel>
#include <QGroupBox>
#include <QString>
#include <QStringList>
#include <QGridLayout>
#include <QFormLayout>

#include "ProjectModel.h"

class LibraryImageStatus : public QWidget {
    Q_OBJECT

public:
    LibraryImageStatus(ProjectModel* model, QWidget* parent = NULL);

public slots:
    void updateData();

private:

    QFormLayout* fillFormLayout(const QStringList& labels);
    void updateFormData(QFormLayout* layout, const QStringList& params);

    ProjectModel* projModel;

    QFormLayout* qvalLayout;
    QFormLayout* tiltLayout;
    QFormLayout* dataLayout;
    QFormLayout* mergeLayout;

    QWidget* dataWidget;
    QLabel* selectImageLabel;


};


#endif /* LIBRARYIMAGESTATUS_H */

