#ifndef REPROJECTWINDOW_H
#define REPROJECTWINDOW_H

#include <QWidget>
#include <QString>
#include <QLabel>
#include <QComboBox>
#include <QPushButton>
#include <QSlider>
#include <QSpinBox>
#include <QHBoxLayout>
#include <QComboBox>
#include <QLabel>
#include <Qt>
#include <QKeyEvent>
#include <QMessageBox>
#include <QFileDialog>
#include <QSize>
#include <QProcess>


#include "confData.h"

//#include "../kernel/mrc/source/2dx_single_particle_lib/2dxSingleParticle.hpp"

class reprojectWindow : public QWidget
{
    Q_OBJECT

public:
    reprojectWindow(confData* config, QWidget *parent = 0);


signals:


public slots:
    


protected:
	

private:
	
	confData* config_gui;

};

#endif 
