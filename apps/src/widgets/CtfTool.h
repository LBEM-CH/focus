#ifndef CTFTOOL_H
#define CTFTOOL_H

#include <QWidget>
#include <QGridLayout>
#include <QLabel>
#include <QDoubleSpinBox>
#include <QPushButton>
#include <cmath>

#include "ParameterConfiguration.h"
#include "FullScreenImage.h"

class CtfTool : public QDialog {
    Q_OBJECT

public:
    CtfTool(const QString& imagePath, FullScreenImage *sourceImage, QWidget *parent = NULL);

public slots:
    void valueChanged();
    void save();
    void load();
    void hideThonRings();

signals:
    void defocusChanged(float defocusX, float defocusY, float astigmatism);

protected:
    bool eventFilter(QObject *target, QEvent *event);

private:
    QString workingDir;
    FullScreenImage *image;
    QLabel *defocusX, *defocusY;
    QDoubleSpinBox *defocus, *defocusDifference, *astigmatism;

    QPushButton *revertButton, *saveButton;
};

#endif
