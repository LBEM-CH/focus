#include "reprojectWindow.h"

#include <iostream>
#include <fstream>

#include <mrcImage.h>


reprojectWindow::reprojectWindow(confData* config,QWidget *parent)
 : QWidget(parent),
   config_gui(config),
   show_proj(true)
{
	setupGUI();
	particleChanged();
}


void reprojectWindow::selectPrevParticle()
{
	int current_index = particleSelection->currentIndex();
	if(current_index == 0)
	{
		return;
	}
	else
	{
		particleSelection->setCurrentIndex(current_index-1);
	}
}


void reprojectWindow::selectNextParticle()
{
	int current_index = particleSelection->currentIndex();
	if(current_index == (particleSelection->count()-1))
	{
		return;
	}
	else
	{
		particleSelection->setCurrentIndex(current_index+1);
	}
}


void reprojectWindow::particleChanged()
{
	std::cout << "particle changed" << std::endl;
	
	std::string result = std::string("asdf");
	
	QString conf_name = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/2dx_image.cfg");
	
	confData* data = new confData(conf_name);
	
	QString imagename = data->get("imagename", "value");
	QString imagenumber = data->get("imagenumber", "value");
	
	int tilt_axis = data->get("TLTAXIS", "value").toFloat();
	int tilt_ang = data->get("TLTANG", "value").toFloat();
	int taxa = data->get("TAXA", "value").toFloat();
		
	taxisSpinBox->setValue(tilt_axis);
    tanglSpinBox->setValue(tilt_ang);
    taxaSpinBox->setValue(taxa);
	
	QString map_file = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/final_map.mrc") ;
	QString ref_file = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/") + imagename + QString("_ref.mrc") ;
	
	std::cout << "ref_file-name: " << ref_file.toStdString() << std::endl;
	
	mrcImage* image = new mrcImage(map_file, true);
	mrcImage* ref = new mrcImage(ref_file, true);
	
	particlePixmapLabel->setPixmap(image->getPixmap());
	projPixmapLabel->setPixmap(ref->getPixmap());
	
	delete image;
	delete ref;
	delete data;
}


void reprojectWindow::updateProjection()
{
	std::string command = "2dx_merge ";
	command += config_gui->getDir("working").toStdString();
	command += ".. 2dx_generateImageMaps ";
	command += particleSelection->currentText().toStdString();
	system(command.c_str());
	particleChanged();
}


void reprojectWindow::lauch2dxImage()
{
	QString image = config_gui->getDir("working") + QString("../") + particleSelection->currentText();
	QProcess::startDetached(config_gui->getApp("2dx_image"), QStringList()<<image);
}


void reprojectWindow::changeToggle()
{
	
	QString conf_name = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/2dx_image.cfg");
	confData* data = new confData(conf_name);
	QString imagename = data->get("imagename", "value");
	QString imagenumber = data->get("imagenumber", "value");
	QString map_file = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/final_map.mrc") ;
	QString ref_file = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/") + imagename + QString("_ref.mrc") ;
	
	if(show_proj)
	{
		mrcImage* image = new mrcImage(map_file, true);
		projPixmapLabel->setPixmap(image->getPixmap());
		delete image;
	}
	else
	{
		mrcImage* ref = new mrcImage(ref_file, true);
		projPixmapLabel->setPixmap(ref->getPixmap());
		delete ref;
	}
	show_proj = !show_proj;
	
	delete data;
}



void reprojectWindow::keyPressEvent(QKeyEvent *event)
{
    if(event->key() == Qt::Key_Left)
    {
		selectPrevParticle();
	}
	if(event->key() == Qt::Key_Right)
    {
		selectNextParticle();
	}
	if( (event->key() == Qt::Key_Shift) || (event->key() == Qt::Key_T) )
    {
       	changeToggle();
    }
}

void reprojectWindow::setupGUI()
{

	QString dir_work = config_gui->getDir("working");
	std::cout << dir_work.toStdString() << std::endl;
	
	layout = new QVBoxLayout();
    particlePanel = new QHBoxLayout();
    
    QVBoxLayout* controlPanel = new QVBoxLayout();
    QVBoxLayout* selectPanel = new QVBoxLayout();
    
    layout->addLayout(selectPanel);
    
    QHBoxLayout* auxPanel = new QHBoxLayout();
    
    layout->addLayout(auxPanel);
    
    auxPanel->addLayout(controlPanel);
    auxPanel->addLayout(particlePanel);

    particleLabel = new QLabel("Particle:");
    particleSelection = new QComboBox();


	std::ifstream infile((dir_work.toStdString() + "/2dx_merge_dirfile.dat").c_str());
	std::string line;
	
	int count = 1;
	while (std::getline(infile, line))
	{
		//QString part_name = "Particle " + QString::number(count++);
		QString part_name = QString(line.c_str());
		particleSelection->addItem(part_name);
	}


//	for(int i=0; i<cont_ave->getNumberOfParticles(); i++)
//	{
//		int particle_number = (*cont_ave)(i).getGlobalParticleInformation().getImageNumber();
//		QString part_name = "Particle " + QString::number(particle_number);
//		particleSelection->addItem(part_name);
//	}
    
    selectPanel->addWidget(particleLabel);
    selectPanel->addWidget(particleSelection);

    QObject::connect(particleSelection, SIGNAL(currentIndexChanged(int)),this, SLOT(particleChanged()));
     
    prevParticleButton = new QPushButton("Previous Particle (<<)");
    controlPanel->addWidget(prevParticleButton);
    QObject::connect(prevParticleButton, SIGNAL(pressed()),this, SLOT(selectPrevParticle()));
    
    nextParticleButton = new QPushButton("Next Particle (>>)");
    controlPanel->addWidget(nextParticleButton);
    QObject::connect(nextParticleButton, SIGNAL(pressed()),this, SLOT(selectNextParticle()));
    
    updateProjectionButton = new QPushButton("Update Projection");
    controlPanel->addWidget(updateProjectionButton);
    QObject::connect(updateProjectionButton, SIGNAL(pressed()),this, SLOT(updateProjection()));
    
    launch2dxImageButton = new QPushButton("Launch 2dx_image");
    controlPanel->addWidget(launch2dxImageButton);
    QObject::connect(launch2dxImageButton, SIGNAL(pressed()),this, SLOT(lauch2dxImage()));
    
    
    //Particle
    pixmap_part =  QPixmap(300,300);
    pixmap_part.fill();
    particlePixmapLabel = new QLabel();
    particlePixmapLabel->setPixmap(pixmap_part);
    particlePanel->addWidget(particlePixmapLabel);

    //Proj
    pixmap_proj =  QPixmap(300,300);
    pixmap_proj.fill();
    projPixmapLabel = new QLabel();
    projPixmapLabel->setPixmap(pixmap_proj);
    particlePanel->addWidget(projPixmapLabel);

 
    anglesLayout = new QVBoxLayout();

    //TAXIS
    taxisLabel = new QLabel("Tilt Axis");
    taxisSpinBox = new QSpinBox();
    taxisSpinBox->setRange(-180,180);
    taxisSpinBox->setValue(0);

    taxisValueLayout = new QHBoxLayout();
    taxisValueLayout->addWidget(taxisLabel);
    taxisValueLayout->addWidget(taxisSpinBox);

    taxisSlider = new QSlider(Qt::Horizontal);
    taxisSlider->setMinimum(taxisSpinBox->minimum());
    taxisSlider->setMaximum(taxisSpinBox->maximum());
    taxisSlider->setValue(taxisSpinBox->value());

    taxisLayout = new QVBoxLayout();
    taxisLayout->addLayout(taxisValueLayout);
    taxisLayout->addWidget(taxisSlider);
    anglesLayout->addLayout(taxisLayout);

    QObject::connect(taxisSpinBox, SIGNAL(valueChanged(int)),taxisSlider, SLOT(setValue(int)));
    QObject::connect(taxisSlider, SIGNAL(valueChanged(int)),taxisSpinBox, SLOT(setValue(int)));
    QObject::connect(taxisSpinBox, SIGNAL(valueChanged(int)),this, SLOT(update()));

    //TANGL
    tanglLabel = new QLabel("Tilt Angle");
    tanglSpinBox = new QSpinBox();
    tanglSpinBox->setRange(-180,180);
    tanglSpinBox->setValue(0);

    tanglValueLayout = new QHBoxLayout();
    tanglValueLayout->addWidget(tanglLabel);
    tanglValueLayout->addWidget(tanglSpinBox);

    tanglSlider = new QSlider(Qt::Horizontal);
    tanglSlider->setMinimum(tanglSpinBox->minimum());
    tanglSlider->setMaximum(tanglSpinBox->maximum());
    tanglSlider->setValue(tanglSpinBox->value());

    QVBoxLayout* tanglLayout = new QVBoxLayout();
    tanglLayout->addLayout(tanglValueLayout);
    tanglLayout->addWidget(tanglSlider);
    anglesLayout->addLayout(tanglLayout);

    QObject::connect(tanglSpinBox, SIGNAL(valueChanged(int)),tanglSlider, SLOT(setValue(int)));
    QObject::connect(tanglSlider, SIGNAL(valueChanged(int)),tanglSpinBox, SLOT(setValue(int)));
    QObject::connect(tanglSpinBox, SIGNAL(valueChanged(int)),this, SLOT(update()));

    //TAXA
    taxaLabel = new QLabel("TAXA");
    taxaSpinBox = new QSpinBox();
    taxaSpinBox->setRange(-180,180);
    taxaSpinBox->setValue(0);

    taxaValueLayout = new QHBoxLayout();
    taxaValueLayout->addWidget(taxaLabel);
    taxaValueLayout->addWidget(taxaSpinBox);

    taxaSlider = new QSlider(Qt::Horizontal);
    taxaSlider->setMinimum(taxaSpinBox->minimum());
    taxaSlider->setMaximum(taxaSpinBox->maximum());
    taxaSlider->setValue(taxaSpinBox->value());

    taxaLayout = new QVBoxLayout();
    taxaLayout->addLayout(taxaValueLayout);
    taxaLayout->addWidget(taxaSlider);
    anglesLayout->addLayout(taxaLayout);

    QObject::connect(taxaSpinBox, SIGNAL(valueChanged(int)),taxaSlider, SLOT(setValue(int)));
    QObject::connect(taxaSlider, SIGNAL(valueChanged(int)),taxaSpinBox, SLOT(setValue(int)));
    QObject::connect(taxaSpinBox, SIGNAL(valueChanged(int)),this, SLOT(update()));

    particlePanel->addLayout(anglesLayout);

    setLayout(layout);
    setWindowTitle("Manual Geometry Manipulation");
   
	
}
