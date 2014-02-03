#include "eulerWindow.h"

#include <iostream>

//#include <boost/filesystem.hpp>
//#include <boost/algorithm/minmax_element.hpp>
//#include <boost/algorithm/string.hpp>
//#include <boost/algorithm/string/split.hpp>


//void eulerWindow::generatePixMap(SingleParticle2dx::DataStructures::Abstract2dData* data, QPixmap& pixmap_h)
//{
/*
	QColor color;
	QSize size_image(300,300);
	QImage image(size_image, QImage::Format_RGB888);
	
	data->getRealSpaceData(data_field);
	
	std::pair< float*, float* > minmax_value = boost::minmax_element(data_field->origin(), data_field->origin() + data_field->num_elements());
	float min_value = *(minmax_value.first);
	float max_value = *(minmax_value.second);
				
	float pixel;
	for(int i=0; i<n; i++)
	{
		for(int j=0; j<n; j++)
		{
			pixel = (*data_field)[j][i];
			pixel = (pixel-min_value)/(max_value-min_value)*255;

			image.setPixel(2*i, 2*j, qRgb(pixel, pixel, pixel));
			image.setPixel(2*i+1, 2*j, qRgb(pixel, pixel, pixel));
			image.setPixel(2*i, 2*j+1, qRgb(pixel, pixel, pixel));
			image.setPixel(2*i+1, 2*j+1, qRgb(pixel, pixel, pixel));
		}
	}

	if(show_grid)
	{
		for(int i=1; i<2*n; i++)
		{
			for(int j=1; j<2*n; j++)
			{
				if(i%30==0 || j%30==0)
				{
					image.setPixel(i, j, qRgb(51, 153, 255));
				}
			}
		}
	}
	
	pixmap_h.convertFromImage(image);
	*/
//}


void eulerWindow::setupGUI()
{
/*
	show_proj = true;
	show_grid = true;

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

	for(int i=0; i<cont_ave->getNumberOfParticles(); i++)
	{
		int particle_number = (*cont_ave)(i).getGlobalParticleInformation().getImageNumber();
		QString part_name = "Particle " + QString::number(particle_number);
		particleSelection->addItem(part_name);
	}
    
    selectPanel->addWidget(particleLabel);
    selectPanel->addWidget(particleSelection);

    QObject::connect(particleSelection, SIGNAL(currentIndexChanged(int)),this, SLOT(particleChanged()));

	gridButton = new QPushButton("Show/Hide Grid");
    controlPanel->addWidget(gridButton);
    QObject::connect(gridButton, SIGNAL(pressed()),this, SLOT(changeGrid()));
    
    toggleButton = new QPushButton("Change ViewMode");
    controlPanel->addWidget(toggleButton);
    QObject::connect(toggleButton, SIGNAL(pressed()),this, SLOT(changeToggle()));

	reloadButton = new QPushButton("Reload Containers");
    controlPanel->addWidget(reloadButton);
    QObject::connect(reloadButton, SIGNAL(pressed()),this, SLOT(reloadContainers()));

    resetParticleButton = new QPushButton("Reset Particle");
    controlPanel->addWidget(resetParticleButton);
    QObject::connect(resetParticleButton, SIGNAL(pressed()),this, SLOT(resetParticle()));
    
    deletePartButton = new QPushButton("Delete Particle");
    controlPanel->addWidget(deletePartButton);
    QObject::connect(deletePartButton, SIGNAL(pressed()),this, SLOT(deleteParticle()));
    
    prevParticleButton = new QPushButton("Previous Particle (<<)");
    controlPanel->addWidget(prevParticleButton);
    QObject::connect(prevParticleButton, SIGNAL(pressed()),this, SLOT(selectPrevParticle()));
    
    nextParticleButton = new QPushButton("Next Particle (>>)");
    controlPanel->addWidget(nextParticleButton);
    QObject::connect(nextParticleButton, SIGNAL(pressed()),this, SLOT(selectNextParticle()));

    showRecButton = new QPushButton("Show Reconstruction");
    controlPanel->addWidget(showRecButton);
    QObject::connect(showRecButton, SIGNAL(pressed()),this, SLOT(showRec()));
    
    saveContButton = new QPushButton("Save Container");
    controlPanel->addWidget(saveContButton);
    QObject::connect(saveContButton, SIGNAL(pressed()),this, SLOT(saveContainer()));
    
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

    //Toggle
    togglePixmapLabel = new QLabel();
    togglePixmapLabel->setPixmap(pixmap_proj);
    particlePanel->addWidget(togglePixmapLabel);
    
    anglesLayout = new QVBoxLayout();

    //TAXIS
    taxisLabel = new QLabel("Tilt Axis");
    taxisSpinBox = new QSpinBox();
    taxisSpinBox->setRange(-360,360);
    taxisSpinBox->setValue(180);

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
    tanglSpinBox->setRange(-360,360);
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
    taxaSpinBox->setRange(-360,360);
    taxaSpinBox->setValue(180);

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
    setWindowTitle("Particle Orienter");
}


void eulerWindow::loadAveContainer(SingleParticle2dx::DataStructures::ParticleContainer& cont_ave)
{
	QMessageBox::information(this, tr("Loading Data"), tr("Loading your data will take a while! Just be patient, 2dx did not crash...\n\nLoading starts after you click 'OK'"));
	
	cont_ave.clear();
	
	QString dir_work = config_gui->getDir("working");
	std::string cont_bin_folder;
	std::string cont_bin_folder_fp;
	
	cont_bin_folder = container_dir + "/ParticleContainers/cont_part4";	
	std::vector<std::string> folder_content;
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
	
	container_full_name_vec.clear();
	container_full_name_vec = std::vector<std::string>(folder_content.size(), "invalid");
	
	#pragma omp parallel for schedule(dynamic,1)
	for(int i=0; i< static_cast<int>(folder_content.size()); i++)
	{
		SingleParticle2dx::DataStructures::ParticleContainer cont;
		SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[i], cont, true);	
		
		SingleParticle2dx::DataStructures::ParticleContainer cont_ave_local;
		cont.generateAverageContainer(cont_ave_local, false, false);
		
		SingleParticle2dx::DataStructures::Particle part2add = cont_ave_local(0);
		part2add.setParticleNumber(i);
		part2add.getGlobalParticleInformation().setImageNumber(i);
		
		#pragma omp critical (add_particle_and_contname_to_euler)
		{
			cont_ave.addParticle(part2add);
			container_full_name_vec[part2add.getGlobalParticleInformation().getImageNumber()] = folder_content[i];
		}
	}
	
	cont_ave.sortContainer();
	*/
}


//void eulerWindow::clearContainer(SingleParticle2dx::DataStructures::ParticleContainer& cont)
//{
//	cont.clear();
//}


int eulerWindow::determineSize()
{
	/*
	QString dir_work = config_gui->getDir("working");
	std::string cont_bin_folder = container_dir + "/ParticleContainers/cont_part4";
	std::vector<std::string> folder_content;
	
	SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(cont_bin_folder, folder_content, ".bin");
		
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(folder_content[0], cont, true);
	return cont(0).getSizeX();
	*/
}


void eulerWindow::reloadContainers()
{
	/*
	clearContainer(*cont_ave);
	loadAveContainer(*cont_ave);
    particleChanged();
	update();
	*/
}

  
void eulerWindow::setupSP2DX()
{	
	/*	
	config_sp = SingleParticle2dx::ConfigContainer::Instance();
	n = determineSize();
	
	config_sp->setParticleSize(n);
	config_sp->setTrialAngleGenerator(4);
	config_sp->setCacheProjections(false);
	config_sp->setParallelProjection(false);
	config_sp->setProjectionMethod(4);
	config_sp->setProjectionMaskingMethod(0);
	
	rec3d = new SingleParticle2dx::DataStructures::Reconstruction3d(n,n,n);
	rec3d->setLPFilterParams(1000, 2);
	
	QString dir_work = config_gui->getDir("working");	
	std::string init_file_name = container_dir + "/Rec_3d/Init3DFromMRC.map";
	
	rec3d->readFromFile(init_file_name);

	cont_ave = new SingleParticle2dx::DataStructures::ParticleContainer;

	loadAveContainer(*cont_ave);

	proj = new SingleParticle2dx::DataStructures::Projection2d(n,n);
	proj->setMaskingMethod(0);
	proj->setLPFilterParams(1000, 2);
    
    data_field = new boost::multi_array<float,2>(boost::extents[n][n]);
    
    SingleParticle2dx::DataStructures::ParticleContainer cont_dummy;
	rec3d->forceProjectionPreparation(cont_dummy);
	*/
}


eulerWindow::eulerWindow(confData* config,QWidget *parent)
 : QWidget(parent),
   config_gui(config)
{
	//setContDir();
	//setupSP2DX();
	//setupGUI();
	//particleChanged();
}


void eulerWindow::setContDir()
{
	//QString workingDir = QFileDialog::getExistingDirectory(0, "Select the container to align", config_gui->getDir("working"));
	//container_dir = workingDir.toStdString();
}


void eulerWindow::keyPressEvent(QKeyEvent *event)
{
    if(event->key() == Qt::Key_S)
    {
       	saveContainer();
    }
    if(event->key() == Qt::Key_G)
    {
        changeGrid();
    }
	if(event->key() == Qt::Key_D)
	{
		deleteParticle();
	}
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


void eulerWindow::saveContainer()
{
	/*
	int r = QMessageBox::warning(this, tr("Confirm save"), tr("Do you realy want to save the alignment?\nNote that this action can not be reverted!"), QMessageBox::Yes | QMessageBox::No);
	
	if(r == QMessageBox::Yes )
	{
		#pragma omp parallel for schedule(dynamic,1)
		for(int i=0; i<cont_ave->getNumberOfParticles(); i++)
		{
			SingleParticle2dx::DataStructures::ParticleContainer cont;
			SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(container_full_name_vec[i], cont, true);
		
			SingleParticle2dx::DataStructures::Orientation o2set = (*cont_ave)(i).getNewOrientation();
		
			for(int j=0; j<cont.getNumberOfParticles(); j++)
			{
				cont(j).setOrientation(o2set);
			}
		
			SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, true, container_full_name_vec[i]);
		}
	}
	else
	{
		return;
	}
	*/
}



void eulerWindow::showRec()
{
	/*
	SingleParticle2dx::DataStructures::Reconstruction3d rec_tmp(n,n,n);
	rec_tmp.setBackprojectionMethod(2);
	rec_tmp.setupForBackProjection();
	rec_tmp.setLPFilterParams(n/6.0, 2);
	
	rec_tmp.generateInitialModel(*cont_ave);
	rec_tmp.applyLowPassFilter();
	
	int rand_number = rand() % 10000 + 1;
	std::string number = SingleParticle2dx::Utilities::StringFunctions::TtoString(rand_number);
	
	rec_tmp.writeToFile(container_dir + "/Rec_3d/rec_from_euler_" + number + ".map");
	
    QString program = "chimera";
    QStringList arguments;
    arguments << "--send" << QString(container_dir.c_str()) + "/Rec_3d/rec_from_euler_" + QString(number.c_str()) + ".map";
    
    QProcess *myProcess = new QProcess();
    myProcess->start(program, arguments);
    myProcess->waitForFinished ( 300000000 );
    */
}


void eulerWindow::changeToggle()
{
	if(show_proj)
	{
		togglePixmapLabel->setPixmap(pixmap_part);
	}
	else
	{
		togglePixmapLabel->setPixmap(pixmap_proj);
	}
	show_proj = !show_proj;
}


void eulerWindow::changeGrid()
{
	show_grid = !show_grid;
	particleChanged();
	update();
}


void eulerWindow::selectNextParticle()
{
	/*
	int current_index = particleSelection->currentIndex();
	if(current_index == (cont_ave->getNumberOfParticles()-1))
	{
		return;
	}
	else
	{
		particleSelection->setCurrentIndex(current_index+1);
	}
	*/
}


void eulerWindow::selectPrevParticle()
{
	/*
	int current_index = particleSelection->currentIndex();
	if(current_index == 0)
	{
		return;
	}
	else
	{
		particleSelection->setCurrentIndex(current_index-1);
	}
	*/
}


void eulerWindow::particleChanged()
{
	/*
	int current_index = particleSelection->currentIndex();
		
	generatePixMap(&((*cont_ave)(current_index)), pixmap_part);
	particlePixmapLabel->setPixmap(pixmap_part);

	SingleParticle2dx::DataStructures::Orientation o = (*cont_ave)(current_index).getNewOrientation();

	taxisSlider->setValue(o.getTLTAXIS());
	tanglSlider->setValue(o.getTLTANG());
	taxaSlider->setValue(o.getTAXA());
	
	if(!show_proj)
	{
		togglePixmapLabel->setPixmap(pixmap_part);
	}
	*/
}


void eulerWindow::deleteParticle()
{
	/*
	int current_index = particleSelection->currentIndex();
	
	int r = QMessageBox::warning(this, tr("Confirm delete"), tr("Do you realy want to delete the current particle?\n" "Note that there is no way of getting the particle back expect repicking the container"), QMessageBox::Yes | QMessageBox::No);
	
	if(r == QMessageBox::Yes )
	{
		particleSelection->removeItem(current_index);
		cont_ave->deleteParticle(current_index);
	
		boost::filesystem::remove(container_full_name_vec[current_index]);	
		container_full_name_vec.erase(container_full_name_vec.begin()+current_index);
		
		particleChanged();
	}
	else
	{
		return;
	}
	*/
}


void eulerWindow::resetParticle()
{
	/*
	int current_index = particleSelection->currentIndex();
	SingleParticle2dx::DataStructures::Orientation o = (*cont_ave)(current_index).getInitialOrientation();
	taxisSlider->setValue(o.getTLTAXIS());
	tanglSlider->setValue(o.getTLTANG());
	taxaSlider->setValue(o.getTAXA());
	*/
}


void eulerWindow::update()
{
	/*
	int current_index = particleSelection->currentIndex();
	
	SingleParticle2dx::DataStructures::Orientation o(taxisSpinBox->value(), tanglSpinBox->value(), taxaSpinBox->value());
   
	rec3d->calculateProjection(o, *proj);
	(*cont_ave)(current_index).updateOrientation(o);
    
    generatePixMap(proj, pixmap_proj);
	projPixmapLabel->setPixmap(pixmap_proj);

	if(show_proj)
	{
		togglePixmapLabel->setPixmap(pixmap_proj);
	}
	*/
}
