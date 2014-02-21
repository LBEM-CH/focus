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
	void selectPrevParticle();
    void selectNextParticle();
    void updateProjection();
	void lauch2dxImage();
	void particleChanged();
	void changeToggle();
	void doAutoMerge();


protected:
	void setupGUI();
	void keyPressEvent(QKeyEvent *event);
	

private:
	QLabel* particleLabel;
    QComboBox* particleSelection;
    
    QLabel* taxisLabel;
    QLabel* tanglLabel;
    QLabel* taxaLabel;
    QLabel* phaoriXLabel;
    QLabel* phaoriYLabel;
    
    QSpinBox* taxisSpinBox;
    QSpinBox* tanglSpinBox;
    QSpinBox* taxaSpinBox;
    QSpinBox* phaoriXSpinBox;
    QSpinBox* phaoriYSpinBox;
    
    QSlider* taxisSlider;
    QSlider* tanglSlider;
    QSlider* taxaSlider;
    QSlider* phaoriXSlider;
    QSlider* phaoriYSlider;
      
	QVBoxLayout *layout;
	QHBoxLayout* particlePanel;
	QVBoxLayout* anglesLayout;
	QVBoxLayout* phaoriLayout;
	
	QHBoxLayout* taxisValueLayout;
	QVBoxLayout* taxisLayout;
	QHBoxLayout* tanglValueLayout;
	QHBoxLayout* taxaValueLayout;
	QVBoxLayout* taxaLayout;
	QVBoxLayout* phaoriXLayout;
	QVBoxLayout* phaoriYLayout;
	QHBoxLayout* phaoriXValueLayout;
	QHBoxLayout* phaoriYValueLayout;
	
	
	QPixmap pixmap_part;
	QPixmap pixmap_proj;
	
	QLabel* projPixmapLabel;
	QLabel* particlePixmapLabel;
	
	QPushButton* nextParticleButton;
	QPushButton* prevParticleButton;
	QPushButton* automergeButton;
	QPushButton* updateProjectionButton;
	QPushButton* launch2dxImageButton;
	
	
	confData* config_gui;
	
	bool show_proj;

};

#endif 
