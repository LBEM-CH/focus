#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>


int main (int argc, char const *argv[])
{
	namespace po = boost::program_options;
	
	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
	std::string binary_config_file;
	std::string binary_particle_file;
	
	po::options_description desc("Allowed options");
	desc.add_options()
	    ("help", "produce help message")
	    ("config", po::value<std::string>(&binary_config_file), "Config container file")
		("particles", po::value<std::string>(&binary_particle_file), "Particle container file")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if (vm.count("help")) {
	    cout << desc << "\n";
	    return 1;
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	SingleParticle2dx::DataStructures::ParticleContainer cont1;
	SingleParticle2dx::DataStructures::ParticleContainer cont2;
	SingleParticle2dx::ConfigContainer* config;
	int n;
	
	if ( vm.count("config") && vm.count("particles") )
	{
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Container and Config from binary files...", 2);

		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Config from binary file...", 1);
		{
		        std::ifstream ifs(binary_config_file.c_str());
		        archive_in_type ia(ifs);
		        ia >> config;
		}

		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Restoring Container from binary file...", 1);
		{
			std::ifstream ifs(binary_particle_file.c_str());
			archive_in_type ia(ifs);
			ia >> cont;
		}
		
		boost::filesystem::remove_all("SCRATCH");
		boost::filesystem::create_directory("SCRATCH");
		
		boost::filesystem::remove_all("LOGS");
		boost::filesystem::create_directory("LOGS");
		
		n = config->getParticleSize();
		
	}
	else
	{
		config = SingleParticle2dx::ConfigContainer::Instance();

		if (config->getConfOnly())
		{
			std::cout << "storing config.bin" << std::endl;
			std::ofstream ofs2("config.bin");
			{
				archive_out_type oa(ofs2);
				oa << config;
			}
			return 0;
		}

		n = config->getParticleSize();

		SingleParticle2dx::DataStructures::PickingDiagnostics dia;
		std::vector<std::string> image_dirs = SingleParticle2dx::ConfigContainer::Instance()->getImageDirectories();

		std::cout << "project dir:" << config->getProjectDirectory() << std::endl;
		
		std::string cont_name = "cont.bin";
		
		if ( boost::filesystem::exists(cont_name) && config->getConsReuse() )
		{
			std::cout << "reloading ref container: " << cont_name << std::endl;
			std::ifstream ifs(cont_name.c_str());
			archive_in_type ia(ifs);
			ia >> cont;
		}
		else
		{

  	  		for (SingleParticle2dx::value_type i=0; i<config->getNumberOfImages(); i++)
  	  		{
  	  			std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
  	  			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("picking from: " + working_dir, 2);
  	  			
				SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, i, false);

			//	SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 2*i, false);
			//	SingleParticle2dx::DataStructures::ParticleContainer::pickParticlesFromCCProfile(working_dir, cont, dia, 2*i+1, true);
  	  		}
  	  	
			cont.setParticleNumbers();
  	  		dia.print();
  	  		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput(boost::lexical_cast<std::string>(cont.getNumberOfParticles()) + " particles picked", 1);
			
			std::ofstream ofs(cont_name.c_str());
			{
				archive_out_type oa(ofs);
				oa << cont;
			}

		}

	//	boost::filesystem::remove("mergevars_written_to_file.txt");

		if ( config->getOnlyStack() )
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Storing Container and Config to binary files...", 2);

			std::ofstream ofs("container.bin");
			{
				archive_out_type oa(ofs);
				oa << cont;
			}

			std::ofstream ofs2("config.bin");
			{
				archive_out_type oa(ofs2);
				oa << config;
			}

			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("...done", 2);

			return 0;
		}
	}
	
	n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_ave(n, n, n);
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave;
	cont.generateAverageContainer(cont_ave);
	rec3d_ave.generateInitialModel(cont_ave);
	rec3d_ave.applyMask();
	rec3d_ave.applyLowPassFilter();
	
	rec3d_ave.writeToFile( "initial_model_ave.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_ave.map", "MAP: Initial Model from AVE", config->getScriptName(), true);
	
	config->setEmanTrialMax(15.0);
	config->setEmanTrialN(20);
	
	SingleParticle2dx::size_type cc_size = config->getCrossCorrelationWindowSize();
	config->setCrossCorrelationWindowSize(1);
	
	for (int i=0; i<10; i++)
	{
		rec3d_ave.updateReconstruction(cont_ave, false);
		rec3d_ave.applyMask();
		rec3d_ave.applyLowPassFilter();
		
		if (rec3d_ave.getLastAngleChange() < 0.25)
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD", 1);
			break;
		}
	}
	config->setCrossCorrelationWindowSize(cc_size);
	
	for (int i=0; i<static_cast<int>(cont_ave.getNumberOfParticles()); i++)
	{
		std::cout << "Image: " << i << std::endl;
		std::cout << "\tTLTAXIS: " << cont_ave(i).getInitialOrientation().getTLTAXIS() << " " << cont_ave(i).getNewOrientation().getTLTAXIS() << std::endl;
		std::cout << "\tTLTANG: " << cont_ave(i).getInitialOrientation().getTLTANG() << " " << cont_ave(i).getNewOrientation().getTLTANG() << std::endl;
		std::cout << "\tTAXA: " << cont_ave(i).getInitialOrientation().getTAXA() << " " << cont_ave(i).getNewOrientation().getTAXA() << std::endl << std::endl;
	}
	
	rec3d_ave.writeToFile( "initial_model_it.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_it.map", "MAP: Initial Model after 10 refines", config->getScriptName(), true);
	//cont.updateInitialTiltGeometry(cont_ave);
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_full(n, n, n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_1(n, n, n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_2(n, n, n);
	
//	cont.shuffleContainer(100000);
	
	if ( config->getSameInitModel() )
	{
		rec3d_1.generateInitialModel(cont);
		rec3d_1.applyLowPassFilter();
		SingleParticle2dx::real_array3d_type temp_data_3d = SingleParticle2dx::real_array3d_type(boost::extents[n][n][n]);
		rec3d_1.getRealSpaceData(temp_data_3d);
		rec3d_2.setFourierSpaceData(temp_data_3d);
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer::splitContainer(cont, cont1, cont2);
	
	std::pair<std::vector<float>, std::vector<float> > fsc_info;
	
	if ( !(config->getSameInitModel()) )
	{
		cont1.shakeContainer(3);
		cont2.shakeContainer(3);
		
		rec3d_1.generateInitialModel(cont1);
		rec3d_2.generateInitialModel(cont2);
		fsc_info = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC(rec3d_1, rec3d_2);
		
		int res_radius = SingleParticle2dx::Utilities::UtilityFunctions::getGoldStandardFSCRadius(fsc_info);
		
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Gold FSC Radius: " + boost::lexical_cast<std::string>(res_radius), 1);
		config->setMaxCCFreq(res_radius);

		rec3d_1.applySqrtFSCFilter(fsc_info.second);
		rec3d_2.applySqrtFSCFilter(fsc_info.second);
		//rec3d_1.applyLowPassFilter();
		//rec3d_2.applyLowPassFilter();
	}
	
	cont1.findNeighbors();
	cont2.findNeighbors();
	
	rec3d_1.writeToFile( "initial_model_1.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_1.map", "MAP: Initial Model 1", config->getScriptName(), true);
	
	rec3d_2.writeToFile( "initial_model_2.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_2.map", "MAP: Initial Model 2", config->getScriptName(), true);

	int max_it = config->getMaxIteration();
	
	config->setEmanTrialMax(2.5);
	config->setEmanTrialN(20);
	
	for(int i=0; i<max_it; i++)
	{
		rec3d_1.updateReconstruction(cont_ave);
		rec3d_2.updateReconstruction(cont_ave);
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + boost::lexical_cast<std::string>(i), rec3d_1.getLastAngleChange(), config->getScriptName());
		SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(i+1, rec3d_1.getLastAngleChange());
				
		rec3d_1.applyMask();
		rec3d_2.applyMask();
		
		rec3d_1.writeToFile("test1.map");
		rec3d_2.writeToFile("test2.map");
		
//		fsc_info = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC(rec3d_1, rec3d_2);
		
//		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Gold FSC Radius: " + boost::lexical_cast<std::string>(SingleParticle2dx::Utilities::UtilityFunctions::getGoldStandardFSCRadius(fsc_info)), 1);
		
//		rec3d_1.applySqrtFSCFilter(fsc_info.second);
//		rec3d_2.applySqrtFSCFilter(fsc_info.second);

		rec3d_1.applyLowPassFilter();
		rec3d_2.applyLowPassFilter();
		
		rec3d_1.writeToFile("reconstruction_1_" + boost::lexical_cast<std::string>(i) + ".map");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction_1_" + boost::lexical_cast<std::string>(i) + ".map", "MAP: Refined Model_1" + boost::lexical_cast<std::string>(i), config->getScriptName());
	
		rec3d_2.writeToFile("reconstruction_2_" + boost::lexical_cast<std::string>(i) + ".map");
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction_2_" + boost::lexical_cast<std::string>(i) + ".map", "MAP: Refined Model_2" + boost::lexical_cast<std::string>(i), config->getScriptName());
		
		int proc = static_cast<int>(static_cast<float>(i+1)/static_cast<float>(max_it)*100);
		SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( proc );
	
		if (rec3d_1.getLastAngleChange() == 0.0)
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD", 1);
			break;
		}	
	}
	
	SingleParticle2dx::DataStructures::ParticleContainer::mergeContainers(cont1, cont2, cont);
	
	rec3d_full.generateInitialModel(cont);
	rec3d_full.applyFinalFSCFilter(fsc_info.second);
//	rec3d_full.applyLowPassFilter();
	rec3d_full.applyFinalMask();
	rec3d_full.writeToFile( "reconstruction.map" );

	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction.map", "MAP: Final Reconstruction", config->getScriptName(), true);
	
	cont.writeStatToFile( "container_stat.txt" );
	
	std::ofstream ofs("fsc.bin");
	{
		archive_out_type oa(ofs);
		oa << fsc_info;
	}
	
	return 0;
}
