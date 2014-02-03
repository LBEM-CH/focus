#include <boost/filesystem.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include "UtilityFunctions.hpp"
#include "../Config.hpp"


void SingleParticle2dx::Utilities::UtilityFunctions::generateInitialModelForInitRef(SingleParticle2dx::DataStructures::Reconstruction3d& rec3d_ave)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	int n = config->getParticleSize();
	
	
	EMAN::EMData* eman_data = new EMAN::EMData;
	boost::multi_array<float, 3> ref_volume;
		
	SingleParticle2dx::Utilities::MRCFileIO::readFromMrc("volume.map", &ref_volume);
		
	int nx = ref_volume.shape()[0];
	int ny = ref_volume.shape()[1];
	int nz = ref_volume.shape()[2];
		
	//	int dnx = (nx-n)/2;
	//	int dny = (ny-n)/2;
	//	int dnz = (nz-n)/2;
		
	int dnx = config->getInitOffsetX();
	int dny = config->getInitOffsetY();
	int dnz = config->getInitOffsetZ();
		
	int size = nx*ny*nz;
		
	eman_data->read_image("volume.map");
				
	boost::multi_array<float, 3> ref_volume_sized(boost::extents[2*n][2*n][2*n]);
	std::copy(eman_data->get_data(), eman_data->get_data()+size, ref_volume.origin() );
		
	for(int i=0; i<300; i++)
	{
		for(int j=0; j<300; j++)
		{
			for(int k=0; k<300; k++)
			{
				ref_volume_sized[k][j][i] = (*eman_data)(i+dny,j-dnx,k-dnz);
			}
		}
	}
		
	boost::multi_array<float, 3> ref_volume_sized2(boost::extents[n][n][n]);	
				
	for(int i=0; i<n; i++)
	{
		for(int j=0; j<n; j++)
		{
			for(int k=0; k<n; k++)
			{
				if(config->getDoBinning())
				{
					ref_volume_sized2[i][j][k] = ref_volume_sized[2*i][2*j][2*k];
					
					ref_volume_sized2[i][j][k] = ref_volume_sized[2*i+1][2*j][2*k];
					ref_volume_sized2[i][j][k] = ref_volume_sized[2*i][2*j+1][2*k];
					ref_volume_sized2[i][j][k] = ref_volume_sized[2*i][2*j][2*k+1];
				
					ref_volume_sized2[i][j][k] = ref_volume_sized[2*i+1][2*j+1][2*k];
					ref_volume_sized2[i][j][k] = ref_volume_sized[2*i][2*j+1][2*k+1];
					ref_volume_sized2[i][j][k] = ref_volume_sized[2*i+1][2*j][2*k+1];

					ref_volume_sized2[i][j][k] = ref_volume_sized[2*i+1][2*j+1][2*k+1];
				}
				else
				{
					ref_volume_sized2[i][j][k] = ref_volume_sized[i+n/2][j+n/2][k+n/2];
				}
			}
		}
	}
	
	rec3d_ave.setFourierSpaceData(ref_volume_sized2);
	
	rec3d_ave.applyLowPassFilter();
	rec3d_ave.applyMask();
	
	rec3d_ave.restoreMissingCone();
}


SingleParticle2dx::Utilities::UtilityFunctions::value_type SingleParticle2dx::Utilities::UtilityFunctions::sinc( value_type x )
{
	value_type pi = SingleParticle2dx::ConfigContainer::Instance()->getPI();
	
	if (fabs(x) < 1e-5)
	{
		return 1;
	}
	
	return (sin(x*pi)/(x*pi));
}


Eigen::Matrix3f SingleParticle2dx::Utilities::UtilityFunctions::determineRotation( SingleParticle2dx::DataStructures::Orientation& o )
{
	Eigen::Matrix3f result;
	
	Eigen::Matrix3f D;
	Eigen::Matrix3f C;
	Eigen::Matrix3f B;
	
	value_type pi = SingleParticle2dx::ConfigContainer::Instance()->getPI();
	
	value_type phi = o.getTLTAXIS() * pi / 180;
	value_type theta = o.getTLTANG() * pi / 180;
	value_type psi = o.getTAXA() * pi / 180;
	
	D << cos(phi), sin(phi), 0,
	     -sin(phi), cos(phi), 0,
		 0, 0, 1;

	C << 1, 0, 0,
		 0, cos(theta), sin(theta),
		 0, -sin(theta), cos(theta);
	
	B << cos(psi), sin(psi), 0,
		 -sin(psi), cos(psi), 0,
		 0, 0, 1;
	
	result = B * C * D;
	
	return result;
}

void SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(std::string image_name, std::string image_desc, std::string scriptname, bool important_flag, bool do_reload)
{
	#pragma omp critical (write_file_output)
	{
		boost::shared_ptr<FILE> file( fopen ( ("LOGS/" + scriptname + ".results").c_str(), "a" ), SingleParticle2dx::Utilities::CustomizedDeleter() );

		std::string image_command;
		if (important_flag)
		{
			image_command = " # IMAGE-IMPORTANT: ";
		}
		else
		{
			image_command = " # IMAGE: ";
		}

		std::string string2place = image_command + image_name + " <" + image_desc + ">";
		fprintf(file.get(), "%s\n", string2place.c_str());
		
		if (do_reload)
		{
			forceReload();
		}
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput(std::string variable_name, value_type value, std::string scriptname, bool do_reload)
{
	#pragma omp critical (write_variable_output)
	{
		boost::shared_ptr<FILE> file( fopen ( ("LOGS/" + scriptname + ".results").c_str(), "a" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		std::string string2place = " set " + variable_name + " = " + SingleParticle2dx::Utilities::StringFunctions::TtoString(value);
		fprintf(file.get(), "%s\n", string2place.c_str());
		
		if (do_reload)
		{
			forceReload();
		}
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::forceReload()
{
	std::cout << "<<@evaluate>>" << std::endl;
}


void SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar(value_type value)
{
	#pragma omp critical (set_progres)
	{
		std::string string2place = "<<@progress: " + SingleParticle2dx::Utilities::StringFunctions::TtoString(value) + ">>";
		std::cout << string2place << std::endl;
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(size_type it, value_type value, std::string filename)
{
	boost::shared_ptr<FILE>file(fopen(filename.c_str(), "a"), SingleParticle2dx::Utilities::CustomizedDeleter());
	fprintf(file.get(), "%i\t%f\n", it, value);
}


SingleParticle2dx::Utilities::UtilityFunctions::size_type SingleParticle2dx::Utilities::UtilityFunctions::getGoldStandardFSCRadius( std::pair<std::vector<value_type>, std::vector<value_type> >& fsc)
{
	size_type i;
	for (i=0; i<static_cast<size_type>(fsc.first.size()); i++)
	{
		std::cout << "\t" << (fsc.first)[i] << "\t" << (fsc.second)[i] << std::endl;
		if ( (fsc.second)[i] < 0.5 )
		{
			break;
		}
	}
	
	if (i == static_cast<size_type>(fsc.first.size()) )
	{
		std::cerr << "not able to get the resolution" << std::endl;
		throw std::runtime_error("Bad operation");;
	}
	
	return (fsc.first)[i];
}

void SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(std::string message, int verbosity)
{
	if (verbosity == 1)
	{
		message = "::" + message;
	}
	else if (verbosity == 2)
	{
		message = ":" + message;
	}
	else if (verbosity != 3)
	{
		std::cerr << "unknown verbosity level for output" << std::endl;
	}
	
	std::cout << message << std::endl;
}


void SingleParticle2dx::Utilities::UtilityFunctions::reqularizeMatrix ( Eigen::MatrixXf& matrix )
{
	value_type dx = 0.1;
	if ( matrix.determinant() > dx)
	{
		return;
	}
	
	size_type counter = 0;
	
	while( matrix.determinant() < dx )
	{
		counter++;
		for(size_type i=0; i<matrix.rows(); i++)
		{
			matrix(i,i) += dx;
		}
		if (counter > 10)
		{
			return;
		}
	}
	
	return;
}


SingleParticle2dx::Utilities::UtilityFunctions::value_type SingleParticle2dx::Utilities::UtilityFunctions::calculateCorrespondingSigma(value_type t, value_type rmax)
{
	return sqrt(-0.5 * (rmax*rmax)/log(t));
}


SingleParticle2dx::Utilities::UtilityFunctions::value_type SingleParticle2dx::Utilities::UtilityFunctions::calculateNeighborWeight(value_type r, value_type sigma)
{
	value_type result = 1;
	
	if(r > 0.01)
	{
		result = exp( -0.5 * (r/sigma) * (r/sigma) );
	}
	std::cout << result << std::endl;
	return result;
}


SingleParticle2dx::Utilities::UtilityFunctions::value_type SingleParticle2dx::Utilities::UtilityFunctions::calculateDensity(std::vector<SingleParticle2dx::DataStructures::Particle*> parts, value_type width)
{
	return static_cast<value_type>(parts.size())/(width*width);
}


void SingleParticle2dx::Utilities::UtilityFunctions::disableParticles(std::vector<SingleParticle2dx::DataStructures::Particle*> parts)
{
	for (size_type i=0; i<static_cast<size_type>(parts.size()); i++)
	{
		parts[i]->setUseForReconstruction(false);
		parts[i]->setRejectTreeSplitting(true);
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::calculateAverage(SingleParticle2dx::DataStructures::ParticleContainer& cont, real_array2d_type& ave, bool align)
{
	std::vector<SingleParticle2dx::DataStructures::Particle*> parts;
	
	for(size_type i=0; i<cont.getNumberOfParticles(); i++)
	{
		parts.push_back(&cont(i));
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::calculateAverage(parts, ave, align);
}


void SingleParticle2dx::Utilities::UtilityFunctions::getContentOfFolder(std::string folder, std::vector<std::string>& folder_content, std::string ending)
{
	boost::filesystem::directory_iterator end_iter;
	if ( boost::filesystem::exists(folder) && boost::filesystem::is_directory(folder))
	{
		for( boost::filesystem::directory_iterator dir_iter(folder) ; dir_iter != end_iter ; ++dir_iter)
		{
			if (boost::filesystem::is_regular_file(dir_iter->status()))
			{
				std::string filename = (dir_iter->path()).native();
				if (boost::contains(filename, ending.c_str()))
				{
					folder_content.push_back(filename);
				}
			}
		}
	}
	
	if ( folder_content.size() < 1)
	{
		std::cerr << "no content in your folder: " << folder << std::endl;
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::calculateAverage(std::vector<SingleParticle2dx::DataStructures::Particle*> parts, real_array2d_type& ave, bool align)
{
	size_type n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	real_array2d_type part_tmp( boost::extents[n][n] );
	SingleParticle2dx::Utilities::DataContainerFunctions::resetData(&ave);
	
	SingleParticle2dx::DataStructures::Particle p2add;
	for(size_type i=0; i<static_cast<size_type>(parts.size()); i++)
	{
		if (align)
		{
			p2add = (*parts[i]);
			p2add.updateParticleShift();
			p2add.getRealSpaceData(&part_tmp);
		}
		else
		{
			parts[i]->getRealSpaceData(&part_tmp);
		}
		SingleParticle2dx::Utilities::DataContainerFunctions::addRealData(&part_tmp, 1, &ave);	
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::applyInplaneShift(SingleParticle2dx::DataStructures::ParticleContainer& cont)
{
	for(size_type i=0; i<cont.getNumberOfParticles(); i++)
	{
		cont(i).updateParticleShift();
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::writeAveContainerStatToFile(SingleParticle2dx::DataStructures::ParticleContainer& cont, std::string filename)
{
	boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
	for(size_type i=0; i<cont.getNumberOfParticles(); i++)
	{
		fprintf(file.get(), "Image: %i\t TLTAXIS: %f\t TLTANG: %f\t TAXA: %f\n", cont(i).getParticleNumber(), cont(i).getNewOrientation().getTLTAXIS(), cont(i).getNewOrientation().getTLTANG(), cont(i).getNewOrientation().getTAXA());
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::alignInplaneShift(SingleParticle2dx::DataStructures::ParticleContainer& cont, SingleParticle2dx::DataStructures::Projection2d& ref, bool useneighbors, value_type t, bool do_parallel)
{
	std::vector<SingleParticle2dx::DataStructures::Particle*> parts;
	
	for(size_type i=0; i<cont.getNumberOfParticles(); i++)
	{
		parts.push_back(&cont(i));
	}
	
	SingleParticle2dx::Utilities::UtilityFunctions::alignInplaneShift(parts, ref, useneighbors, t, do_parallel);
}


void SingleParticle2dx::Utilities::UtilityFunctions::alignSingleParticle(SingleParticle2dx::DataStructures::Particle* part, SingleParticle2dx::DataStructures::Projection2d& ref, bool useneighbors, value_type t)
{
	size_type cc_size = SingleParticle2dx::ConfigContainer::Instance()->getCrossCorrelationWindowSize();
	size_type n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	boost::scoped_ptr<SingleParticle2dx::Methods::AbstractPeakSearchMethod> m_peaksearch_strategy;
	boost::scoped_ptr<SingleParticle2dx::Methods::AbstractCalcCCMethod> m_clac_cc_strategy;
	
	m_peaksearch_strategy.reset( new SingleParticle2dx::Methods::MaxSearchMethod );
	m_clac_cc_strategy.reset( new SingleParticle2dx::Methods::BasicCalcCCMethod );
	
	real_array2d_type cc_data( boost::extents[cc_size][cc_size] );
	SingleParticle2dx::DataStructures::ParticleShift optimal_shift;
	value_type current_cc_value;
		
	SingleParticle2dx::Utilities::DataContainerFunctions::resetData( &cc_data );
		
	SingleParticle2dx::DataStructures::Particle p_sum = (*part);
		
	if (useneighbors)
	{
		real_array2d_type part_sum( boost::extents[n][n] );
		real_array2d_type part_tmp( boost::extents[n][n] );
		part->getRealSpaceData(&part_sum);

		std::vector<SingleParticle2dx::DataStructures::Particle*> n_vec = part->getNeighbors();
		std::vector<value_type> dis_vec = part->getDistances();

		value_type sigma = SingleParticle2dx::Utilities::UtilityFunctions::calculateCorrespondingSigma(0.25, part->getMaxDistance());
		value_type weight;
			
		SingleParticle2dx::DataStructures::Particle p2add;
		for (size_type j=0; j<static_cast<size_type>(n_vec.size()); j++)
		{
			p2add = (*n_vec[j]);
			p2add.getRealSpaceData(&part_tmp);
			weight = SingleParticle2dx::Utilities::UtilityFunctions::calculateNeighborWeight(dis_vec[j], sigma);
			//weight = 1;
				
			SingleParticle2dx::Utilities::DataContainerFunctions::addRealData(&part_tmp, weight, &part_sum);
		}        

		p_sum.setFourierSpaceData(&part_sum);
	}
	
	//m_clac_cc_strategy.get()->calculateCrossCorrelation( (*parts[i]), ref, cc_data );
	m_clac_cc_strategy.get()->calculateCrossCorrelation( p_sum, ref, cc_data );
		
	current_cc_value = 0;
		
	try
	{
		current_cc_value = m_peaksearch_strategy.get()->findMaximalElementAndSetShift(cc_data, optimal_shift);
	}
	catch(...)
	{
		optimal_shift.setShiftX(0);
		optimal_shift.setShiftY(0);
	}
		
	if(current_cc_value < t)
	{
		part->setRejectTreeAlignment(true);
		part->setUseForReconstruction(false);
	}
	else
	{
		part->setRejectTreeAlignment(false);
		part->setSimMeasure(current_cc_value);
		part->setParticleShift(optimal_shift);
	}
		
	//std::cout << current_cc_value << " " << optimal_shift.getShiftX() << " " << optimal_shift.getShiftY() << std::endl;
}


void SingleParticle2dx::Utilities::UtilityFunctions::createContainerFolder(std::string foldername)
{
	boost::filesystem::create_directory(foldername);
	boost::filesystem::create_directory(foldername + "/ParticleContainers");
	boost::filesystem::create_directory(foldername + "/FingerPrintContainers");
	boost::filesystem::create_directory(foldername + "/ContainerStatsTXT");
	boost::filesystem::create_directory(foldername + "/ContainerStatsPDF");
	boost::filesystem::create_directory(foldername + "/Rec_3d");
	boost::filesystem::create_directory(foldername + "/Div_output");
	
	boost::filesystem::create_directory(foldername + "/Particles");
	boost::filesystem::create_directory(foldername + "/Particles/Picking_output");
}


SingleParticle2dx::Utilities::UtilityFunctions::value_type SingleParticle2dx::Utilities::UtilityFunctions::calculateCC(SingleParticle2dx::DataStructures::Particle& part, SingleParticle2dx::DataStructures::Projection2d& proj)
{
	real_array2d_type cc_data( boost::extents[1][1] );
	
	boost::scoped_ptr<SingleParticle2dx::Methods::AbstractCalcCCMethod> calc_cc_strategy;
	calc_cc_strategy.reset( new SingleParticle2dx::Methods::BasicCalcCCMethod );
	
	calc_cc_strategy.get()->calculateCrossCorrelation( part, proj, cc_data );
	return cc_data[0][0];
}


void SingleParticle2dx::Utilities::UtilityFunctions::removeFileIfExists(std::string filename)
{
	if ( boost::filesystem::exists(filename) )
	{
		boost::filesystem::remove(filename);
	}
}


void SingleParticle2dx::Utilities::UtilityFunctions::removeFolderIfExists(std::string foldername)
{
	if ( boost::filesystem::exists(foldername) )
	{
		boost::filesystem::remove_all(foldername);
	}
}



void SingleParticle2dx::Utilities::UtilityFunctions::alignInplaneShift(std::vector<SingleParticle2dx::DataStructures::Particle*> parts, SingleParticle2dx::DataStructures::Projection2d& ref, bool useneighbors, value_type t, bool do_parallel)
{
	if(do_parallel)
	{
		#pragma omp parallel for
		for(size_type i=0; i<static_cast<size_type>(parts.size()); i++)
		{
			alignSingleParticle(parts[i], ref, useneighbors, t);
		}
	}
	else
	{
		for(size_type i=0; i<static_cast<size_type>(parts.size()); i++)
		{
			alignSingleParticle(parts[i], ref, useneighbors, t);
		}
	}
	
	
	/*
	size_type cc_size = SingleParticle2dx::ConfigContainer::Instance()->getCrossCorrelationWindowSize();
	size_type n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	boost::scoped_ptr<SingleParticle2dx::Methods::AbstractPeakSearchMethod> m_peaksearch_strategy;
	boost::scoped_ptr<SingleParticle2dx::Methods::AbstractCalcCCMethod> m_clac_cc_strategy;
	
	m_peaksearch_strategy.reset( new SingleParticle2dx::Methods::MaxSearchMethod );
	m_clac_cc_strategy.reset( new SingleParticle2dx::Methods::BasicCalcCCMethod );
	
	#pragma omp parallel for
	for(size_type i=0; i<static_cast<size_type>(parts.size()); i++)
	{
		real_array2d_type cc_data( boost::extents[cc_size][cc_size] );
		SingleParticle2dx::DataStructures::ParticleShift optimal_shift;
		value_type current_cc_value;
		
		SingleParticle2dx::Utilities::DataContainerFunctions::resetData( &cc_data );
		
		SingleParticle2dx::DataStructures::Particle p_sum = (*parts[i]);
		
		if (useneighbors)
		{
			real_array2d_type part_sum( boost::extents[n][n] );
			real_array2d_type part_tmp( boost::extents[n][n] );
			(*parts[i]).getRealSpaceData(&part_sum);

			std::vector<SingleParticle2dx::DataStructures::Particle*> n_vec = parts[i]->getNeighbors();
			std::vector<value_type> dis_vec = parts[i]->getDistances();

			value_type sigma = SingleParticle2dx::Utilities::UtilityFunctions::calculateCorrespondingSigma(0.25, parts[i]->getMaxDistance());
			value_type weight;
			
			//std::cout << "local sum: " << static_cast<size_type>(n_vec.size()) << std::endl;

			SingleParticle2dx::DataStructures::Particle p2add;
			for (size_type j=0; j<static_cast<size_type>(n_vec.size()); j++)
			{
				p2add = (*n_vec[j]);
				p2add.getRealSpaceData(&part_tmp);
				weight = SingleParticle2dx::Utilities::UtilityFunctions::calculateNeighborWeight(dis_vec[i], sigma);
				//weight = 1;
				
				SingleParticle2dx::Utilities::DataContainerFunctions::addRealData(&part_tmp, weight, &part_sum);
			}        

			p_sum.setFourierSpaceData(&part_sum);
		}
	
		//m_clac_cc_strategy.get()->calculateCrossCorrelation( (*parts[i]), ref, cc_data );
		m_clac_cc_strategy.get()->calculateCrossCorrelation( p_sum, ref, cc_data );
		
		current_cc_value = 0;
		
		try
		{
			current_cc_value = m_peaksearch_strategy.get()->findMaximalElementAndSetShift(cc_data, optimal_shift);
		}
		catch(...)
		{
			optimal_shift.setShiftX(0);
			optimal_shift.setShiftY(0);
		}
		
		if(current_cc_value < t)
		{
			parts[i]->setRejectTreeAlignment(true);
			parts[i]->setUseForReconstruction(false);
		}
		else
		{
			parts[i]->setRejectTreeAlignment(false);
			parts[i]->setSimMeasure(current_cc_value);
			parts[i]->setParticleShift(optimal_shift);
		}
		
		std::cout << current_cc_value << " " << optimal_shift.getShiftX() << " " << optimal_shift.getShiftY() << std::endl;
	}
	*/
}
