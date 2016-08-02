#include "reprojectWindow.h"

#include <iostream>
#include <fstream>

#include <mrcImage.h>


reprojectWindow::reprojectWindow(confData* config,QWidget *parent)
 : QWidget(parent),
   config_gui(config),
   show_proj(true),
   size(400)
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
	QString conf_name = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/2dx_image.cfg");
	confData* data = new confData(conf_name);
	QString imagename = data->get("imagename", "value");
	QString imagenumber = data->get("imagenumber", "value");
	
	int tilt_axis = data->get("TLTAXIS", "value").toFloat();
	int tilt_ang = data->get("TANGL", "value").toFloat();
	int taxa = data->get("TAXA", "value").toFloat();
	
	taxisSpinBox->setValue(tilt_axis);
    tanglSpinBox->setValue(tilt_ang);
    taxaSpinBox->setValue(taxa);
	
	QString q_phaori = data->get("phaori", "value");
	QStringList phaori_list = q_phaori.split(",");
	
	int phaoriX = phaori_list.at(0).toFloat();
	int phaoriY = phaori_list.at(1).toFloat();
	
	phaoriXSpinBox->setValue(phaoriX);
	phaoriYSpinBox->setValue(phaoriY);
	
	QString map_file = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/final_map.mrc") ;
	QString ref_file = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/") + imagename + QString("_ref.mrc") ;
	
	mrcImage* image = new mrcImage(map_file, true);
	mrcImage* ref = new mrcImage(ref_file, true);
	
	particlePixmapLabel->setPixmap(image->getPixmap().scaled(size,size));
	projPixmapLabel->setPixmap(ref->getPixmap().scaled(size,size));
	
	delete image;
	delete ref;
	delete data;
}


void reprojectWindow::updateProjection()
{
	QString conf_name = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/2dx_image.cfg");
	confData* data = new confData(conf_name);
	QString imagename = data->get("imagename", "value");
	QString imagenumber = data->get("imagenumber", "value");
	
	int tilt_axis = taxisSpinBox->value();
	int tilt_ang = tanglSpinBox->value();
	int taxa =  taxaSpinBox->value();
	
	data->set(QString("TLTAXIS"), QString::number(tilt_axis));
	data->set(QString("TANGL"), QString::number(tilt_ang));
	data->set(QString("TAXA"), QString::number(taxa));
	
	int phaoriX = phaoriXSpinBox->value();
	int phaoriY = phaoriYSpinBox->value();
	data->set(QString("phaori"), QString::number(phaoriX) + QString(",") + QString::number(phaoriY));
	
	data->save();
	
	std::string command = "2dx_merge ";
	command += config_gui->getDir("working").toStdString();
	command += "/.. 2dx_generateImageMaps ";
	command += particleSelection->currentText().toStdString();
	
	std::cout << "command: " << command << std::endl << std::endl;
	system(command.c_str());
	particleChanged();
}


void reprojectWindow::doAutoMerge()
{
	std::string command = "2dx_merge ";
	command += config_gui->getDir("working").toStdString();
	command += "../ 2dx_refine ";
	command += particleSelection->currentText().toStdString();
	
	system(command.c_str());
	
	std::string file_name = config_gui->getDir("working").toStdString() + "LOGS/2dx_refine.results";
	std::cout << file_name << std::endl;
	std::ifstream infile(file_name.c_str());
	std::string line;
	
	QString conf_name = config_gui->getDir("working") + QString("../") + particleSelection->currentText() + QString("/2dx_image.cfg");
	confData* data = new confData(conf_name);
	
	while (std::getline(infile, line))
	{
		QString qline = QString(line.c_str());
		if (qline.startsWith(QString("set phaori = ")))
		{
			QStringList tmp = qline.split(QString(" "));
			data->set(QString("phaori"), tmp.at(3));
			
			QString data_string = tmp.at(3);
			data_string = data_string.remove(0,1);
			data_string = data_string.remove(data_string.size()-1,1);
			QStringList tmp2 = data_string.split(",");
			
			int phaoriX = tmp2.at(0).toFloat();
			int phaoriY = tmp2.at(1).toFloat();
			
			phaoriXSpinBox->setValue(phaoriX);
			phaoriYSpinBox->setValue(phaoriY);
		}
		
		if (qline.startsWith(QString("set phaoriFouFilter = ")))
		{
			QStringList tmp = qline.split(QString(" "));
			data->set(QString("phaoriFouFilter"), tmp.at(3));
		}
	}
	
	data->save();
	updateProjection();
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
		projPixmapLabel->setPixmap(image->getPixmap().scaled(size,size));
		delete image;
	}
	else
	{
		mrcImage* ref = new mrcImage(ref_file, true);
		projPixmapLabel->setPixmap(ref->getPixmap().scaled(size,size));
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
	
	layout = new QVBoxLayout();
    particlePanel = new QHBoxLayout();
    
    QVBoxLayout* controlPanel = new QVBoxLayout();
    QVBoxLayout* selectPanel = new QVBoxLayout();
    
    layout->addLayout(selectPanel);
    
    QHBoxLayout* auxPanel = new QHBoxLayout();
    
    layout->addLayout(auxPanel);
    
    auxPanel->addLayout(controlPanel);
    auxPanel->addLayout(particlePanel);

    particleLabel = new QLabel("Image:");
    particleSelection = new QComboBox();

	std::ifstream infile((dir_work.toStdString() + "/2dx_merge_dirfile.dat").c_str());
	std::string line;
	
	int count = 1;
	while (std::getline(infile, line))
	{
		QString part_name = QString(line.c_str());
		particleSelection->addItem(part_name);
	}
    
    selectPanel->addWidget(particleLabel);
    selectPanel->addWidget(particleSelection);

    QObject::connect(particleSelection, SIGNAL(currentIndexChanged(int)),this, SLOT(particleChanged()));
     
    prevParticleButton = new QPushButton("Previous Particle (<<)");
    controlPanel->addWidget(prevParticleButton);
    QObject::connect(prevParticleButton, SIGNAL(pressed()),this, SLOT(selectPrevParticle()));
    
    nextParticleButton = new QPushButton("Next Particle (>>)");
    controlPanel->addWidget(nextParticleButton);
    QObject::connect(nextParticleButton, SIGNAL(pressed()),this, SLOT(selectNextParticle()));
    
	QSpacerItem *spacer = new QSpacerItem(40, 100, QSizePolicy::Expanding, QSizePolicy::Minimum);
	controlPanel->addItem(spacer);

    automergeButton = new QPushButton("Automatic merge");
    controlPanel->addWidget(automergeButton);
    QObject::connect(automergeButton, SIGNAL(pressed()),this, SLOT(doAutoMerge()));
    
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
    
	controlRLayout = new QGridLayout();

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
	controlRLayout->addLayout(taxisLayout, 0, 0);
	
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
	controlRLayout->addLayout(tanglLayout, 1, 0);

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
	controlRLayout->addLayout(taxaLayout, 2, 0);

    QObject::connect(taxaSpinBox, SIGNAL(valueChanged(int)),taxaSlider, SLOT(setValue(int)));
    QObject::connect(taxaSlider, SIGNAL(valueChanged(int)),taxaSpinBox, SLOT(setValue(int)));
    QObject::connect(taxaSpinBox, SIGNAL(valueChanged(int)),this, SLOT(update()));
    
    phaoriXLabel = new QLabel("Phaori X");
    phaoriXSpinBox = new QSpinBox();
    phaoriXSpinBox->setRange(-180,180);
    phaoriXSpinBox->setValue(0);

    phaoriXValueLayout = new QHBoxLayout();
    phaoriXValueLayout->addWidget(phaoriXLabel);
    phaoriXValueLayout->addWidget(phaoriXSpinBox);

    phaoriXSlider = new QSlider(Qt::Horizontal);
    phaoriXSlider->setMinimum(phaoriXSpinBox->minimum());
    phaoriXSlider->setMaximum(phaoriXSpinBox->maximum());
    phaoriXSlider->setValue(phaoriXSpinBox->value());

    phaoriXLayout = new QVBoxLayout();
    phaoriXLayout->addLayout(phaoriXValueLayout);
    phaoriXLayout->addWidget(phaoriXSlider);
	controlRLayout->addLayout(phaoriXLayout, 0, 1);

    QObject::connect(phaoriXSpinBox, SIGNAL(valueChanged(int)),phaoriXSlider, SLOT(setValue(int)));
    QObject::connect(phaoriXSlider, SIGNAL(valueChanged(int)),phaoriXSpinBox, SLOT(setValue(int)));
    QObject::connect(phaoriXSpinBox, SIGNAL(valueChanged(int)),this, SLOT(update()));
    
    phaoriYLabel = new QLabel("Phaori Y");
    phaoriYSpinBox = new QSpinBox();
    phaoriYSpinBox->setRange(-180,180);
    phaoriYSpinBox->setValue(0);

    phaoriYValueLayout = new QHBoxLayout();
    phaoriYValueLayout->addWidget(phaoriYLabel);
    phaoriYValueLayout->addWidget(phaoriYSpinBox);

    phaoriYSlider = new QSlider(Qt::Horizontal);
    phaoriYSlider->setMinimum(phaoriYSpinBox->minimum());
    phaoriYSlider->setMaximum(phaoriYSpinBox->maximum());
    phaoriYSlider->setValue(phaoriYSpinBox->value());

    phaoriYLayout = new QVBoxLayout();
    phaoriYLayout->addLayout(phaoriYValueLayout);
    phaoriYLayout->addWidget(phaoriYSlider);
	controlRLayout->addLayout(phaoriYLayout, 1, 1);

    QObject::connect(phaoriYSpinBox, SIGNAL(valueChanged(int)),phaoriYSlider, SLOT(setValue(int)));
    QObject::connect(phaoriYSlider, SIGNAL(valueChanged(int)),phaoriYSpinBox, SLOT(setValue(int)));
    QObject::connect(phaoriYSpinBox, SIGNAL(valueChanged(int)),this, SLOT(update()));
    
	particlePanel->addLayout(controlRLayout);
    
	updateProjectionButton = new QPushButton("Update View...");
    QObject::connect(updateProjectionButton, SIGNAL(pressed()),this, SLOT(updateProjection()));
    controlRLayout->addWidget(updateProjectionButton, 3, 0, 1, 2);

    setLayout(layout);
    setWindowTitle("Manual Geometry Manipulation");
}
