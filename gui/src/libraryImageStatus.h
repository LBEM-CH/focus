
#ifndef LIBRARYIMAGESTATUS_H
#define LIBRARYIMAGESTATUS_H

#include <QWidget>
#include <QLabel>
#include <QGroupBox>
#include <QString>
#include <QStringList>
#include <QGridLayout>
#include <QFormLayout>

#include "projectModel.h"

class libraryImageStatus : public QWidget {
    
    Q_OBJECT
            
    public:
        libraryImageStatus(projectModel* model, QWidget* parent = NULL);
        
    public slots:
        void updateData();
        
    private:
        
        QFormLayout* fillFormLayout(const QStringList& labels);
        void updateFormData(QFormLayout* layout, const QStringList& params);
        
        projectModel* projModel;
        
        QFormLayout* qvalLayout;
        QFormLayout* tiltLayout;
        QFormLayout* dataLayout;
        QFormLayout* mergeLayout;
        
        QWidget* dataWidget;
        QLabel* selectImageLabel;
            
    
};


#endif /* LIBRARYIMAGESTATUS_H */

