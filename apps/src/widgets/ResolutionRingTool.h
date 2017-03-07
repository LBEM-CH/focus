#ifndef RESOLUTIONRINGTOOL_H
#define RESOLUTIONRINGTOOL_H

#include <QDialog>
#include <QDoubleSpinBox>
#include <QVBoxLayout>
#include <QEvent>
#include <QKeyEvent>

#include "FullScreenImage.h"

class ResolutionRingTool : public QDialog {

public:
    ResolutionRingTool(FullScreenImage* sourceImage, QWidget* parent = 0) : 
    QDialog(parent) {
        
        setWindowTitle("Resolution Ring");
        
        image = sourceImage;
        
        resolutionBox = new QDoubleSpinBox(this);
        resolutionBox->setValue(3.7);
        resolutionBox->setMinimum(1.0);
        resolutionBox->setSingleStep(0.1);
        resolutionBox->installEventFilter(this);
        
        connect(resolutionBox, static_cast<void(QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged), [=](double value) {
            image->setResolutionRingValue(value);
        });
        
        QLabel* textLabel = new QLabel("Please provide a value for resolution (in A) for which the ring is to be displayed.");
        
        QVBoxLayout* mainLayout = new QVBoxLayout();
        mainLayout->addWidget(textLabel);
        mainLayout->addWidget(resolutionBox);
        
        setLayout(mainLayout);
        
    };

protected:
    bool eventFilter(QObject *target, QEvent *event) {
        if (target == resolutionBox) {
            if (event->type() == QEvent::KeyPress) {
                QKeyEvent *keyEvent = static_cast<QKeyEvent *> (event);
                if (keyEvent->key() == Qt::Key_R) {
                    image->toggleResolutionRing();
                    return true;
                }
            }
        }
        return QDialog::eventFilter(target, event);
    }

private:
    QDoubleSpinBox* resolutionBox;
    FullScreenImage* image;

};

#endif /* RESOLUTIONRINGTOOL_H */

