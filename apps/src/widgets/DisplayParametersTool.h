#ifndef DISPLAYPARAMETERSTOOL_H
#define DISPLAYPARAMETERSTOOL_H

#include <QWidget>
#include <QGridLayout>
#include <QLabel>
#include <QSpinBox>
#include <QDoubleSpinBox>
#include <QComboBox>
#include <QCheckBox>
#include <QSignalMapper>
#include <QAction>

#include "ParameterConfiguration.h"
#include "mrcImage.h"

class DisplayParametersTool : public QWidget {
    Q_OBJECT

public:
    DisplayParametersTool(QWidget *parent = NULL);

public slots:
    /* Overloaded Slots */

    /* Slots */
    void changeLatticeSize(int value);
    void changeLatticeCircleLine(int thickness);
    void changeLatticeOrders(int value);
    void changeSpotSize(int value);
    void changeRefinementSize(int value);
    void changeFontSize(int value);
    void changeViewFitState(int state);
    void changeSearchMethod(int method);
    void flush();
    void complete(QWidget *widget);
    void complete();

    void setDefaults();

signals:
    void latticeSizeChanged(int size);
    void latticeCircleLineChanged(int thickness);
    void latticeOrdersChanged(int orders);
    void spotSizeChanged(int size);
    void refinementSizeChanged(int size);
    void fontSizeChanged(int size);
    void searchRangeChanged(int range);
    void sigmaChanged(double sigma);
    void searchMethodChanged(mrcImage::maxValueMethod method);

    void viewFitChanged(bool view);

protected:
    void showEvent(QShowEvent *event);
    void focusInEvent(QFocusEvent *event);

private:
    QSpinBox latticeSize, latticeOrders, spotSize, latticeRefineSize, latticeCircleLine, fontSize;
    QSpinBox searchRange;
    QDoubleSpinBox sigma;

    QCheckBox viewFit;

    QSignalMapper *signalMapper;

    QComboBox maxValueMethodChooser;

};

#endif
