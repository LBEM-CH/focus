#ifndef EULERWINDOW_H
#define EULERWINDOW_H

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

class eulerWindow : public QWidget
{
    Q_OBJECT

public:
    eulerWindow(confData* config, QWidget *parent = 0);


signals:


public slots:
    void update();
    void particleChanged();
    void changeGrid();
    void changeToggle();
    void resetParticle();
    void selectNextParticle();
    void selectPrevParticle();
    void saveContainer();
    void showRec();
    void deleteParticle();
    void reloadContainers();
    
 //   void loadAveContainer(SingleParticle2dx::DataStructures::ParticleContainer& cont_ave);
 //   void clearContainer(SingleParticle2dx::DataStructures::ParticleContainer& cont);


protected:
	void keyPressEvent(QKeyEvent *event);
	void setupGUI();
    void setupSP2DX();
  //  void generatePixMap(SingleParticle2dx::DataStructures::Abstract2dData* data, QPixmap& pixmap);
    int determineSize();
    void setContDir();


private:
	QLabel* particleLabel;
    QComboBox* particleSelection;
    QPushButton* saveButton;
    QPushButton* cancelButton;
    QPushButton* gridButton;
    QPushButton* toggleButton;
    QPushButton* reloadButton;

    QLabel* taxisLabel;
    QLabel* tanglLabel;
    QLabel* taxaLabel;
    
    QSpinBox* taxisSpinBox;
    QSpinBox* tanglSpinBox;
    QSpinBox* taxaSpinBox;
    
    QSlider* taxisSlider;
    QSlider* tanglSlider;
    QSlider* taxaSlider;

	QVBoxLayout *layout;
	QHBoxLayout* particlePanel;
	QVBoxLayout* anglesLayout;
	
	QHBoxLayout* taxisValueLayout;
	QVBoxLayout* taxisLayout;
	QHBoxLayout* tanglValueLayout;
	QHBoxLayout* taxaValueLayout;
	QVBoxLayout* taxaLayout;
	
	QPixmap pixmap_part;
	QPixmap pixmap_proj;

	QLabel* projPixmapLabel;
	QLabel* particlePixmapLabel;
	QLabel* togglePixmapLabel;

	QPushButton* resetParticleButton;
	QPushButton* nextParticleButton;
	QPushButton* prevParticleButton;
	QPushButton* showRecButton;
	QPushButton* saveContButton;
	QPushButton* deletePartButton;
	
//	SingleParticle2dx::DataStructures::Reconstruction3d* rec3d;
//	SingleParticle2dx::ConfigContainer* config_sp;
//	SingleParticle2dx::DataStructures::Projection2d* proj;
//	boost::multi_array<float,2>* data_field;
	
//	SingleParticle2dx::DataStructures::ParticleContainer* cont_ave;

	bool show_proj;
	bool show_grid;
	
	int n;
	
	std::string container_dir;
	
	std::vector<std::string> container_full_name_vec;
	std::vector<std::string> container_fp_name_vec;
	
	confData* config_gui;

};

#endif // EULERWINDOW_H
