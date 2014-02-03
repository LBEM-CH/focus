#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

//#include <boost/mpi/environment.hpp>
//#include <boost/mpi/communicator.hpp>
//#include <boost/mpi/nonblocking.hpp>

#include <boost/mpi.hpp>

//namespace mpi = boost::mpi;

int main(int argc, char* argv[])
{

	boost::mpi::environment env(argc, argv);
	boost::mpi::communicator world;

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
	config->setEmanTrialN(40);
	
	SingleParticle2dx::size_type cc_size = config->getCrossCorrelationWindowSize();
	config->setCrossCorrelationWindowSize(1);
	
	for (int i=0; i<10; i++)
	{
		rec3d_ave.updateReconstruction(cont_ave, false);
		rec3d_ave.applyMask();
		rec3d_ave.applyLowPassFilter();
		
		if (rec3d_ave.getLastAngleChange() < 0.1)
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
	cont.updateInitialTiltGeometryAndShift(cont_ave);

	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_full(n, n, n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_1(n, n, n);
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_2(n, n, n);
	
	rec3d_full.setBackprojectionMethod(3);
	rec3d_1.setBackprojectionMethod(3);
	rec3d_2.setBackprojectionMethod(3);

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
		cont1.shakeContainer(6);
		cont2.shakeContainer(6);
		
		rec3d_1.generateInitialModel(cont1);
		rec3d_2.generateInitialModel(cont2);
		fsc_info = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC(rec3d_1, rec3d_2);
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Gold FSC Radius: " + boost::lexical_cast<std::string>(SingleParticle2dx::Utilities::UtilityFunctions::getGoldStandardFSCRadius(fsc_info)), 1);

		rec3d_1.applySqrtFSCFilter(fsc_info.second);
		rec3d_2.applySqrtFSCFilter(fsc_info.second);
	}
	
	// @TODO FIX ME
	cont1.findNeighbors(1);
	cont2.findNeighbors(1);
	
	rec3d_1.writeToFile( "initial_model_1.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_1.map", "MAP: Initial Model 1", config->getScriptName(), true);
	
	rec3d_1.writeToFile( "initial_model_2.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("initial_model_2.map", "MAP: Initial Model 2", config->getScriptName(), true);

	int max_it = config->getMaxIteration();
	
	config->setEmanTrialMax(4.0);
	config->setEmanTrialN(100);

	for(int i=0; i<max_it; i++)
	{
		if (world.rank() == 0)
		{
			rec3d_1.updateReconstruction(cont1);
			rec3d_1.applyMask();
			
			SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + boost::lexical_cast<std::string>(i), rec3d_1.getLastAngleChange(), config->getScriptName());
		//	SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(i+1, rec3d_1.getLastAngleChange());
		}
		
		if (world.rank() == 1)
		{
			rec3d_2.updateReconstruction(cont2);
			rec3d_2.applyMask();
			
			SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + boost::lexical_cast<std::string>(i), rec3d_2.getLastAngleChange(), config->getScriptName());
		//	SingleParticle2dx::Utilities::UtilityFunctions::writeConvergenceToFile(i+1, rec3d_2.getLastAngleChange());
		}
		
		if (world.rank() == 0)
		{
			boost::mpi::request reqs[2];
			reqs[0] = world.isend(1, 0, rec3d_1);
			reqs[1] = world.irecv(1, 1, rec3d_2);
			boost::mpi::wait_all(reqs, reqs+2);
		}
		
		if (world.rank() == 1)
		{
			boost::mpi::request reqs[2];
			reqs[0] = world.isend(0, 1, rec3d_2);
			reqs[1] = world.irecv(0, 0, rec3d_1);
			boost::mpi::wait_all(reqs, reqs+2);
		}
		
		fsc_info = SingleParticle2dx::DataStructures::ParticleContainer::calculateFSC(rec3d_1, rec3d_2);
		
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("Gold FSC Radius: " + boost::lexical_cast<std::string>(SingleParticle2dx::Utilities::UtilityFunctions::getGoldStandardFSCRadius(fsc_info)), 1);
		
		if (world.rank() == 0)
		{
			rec3d_1.applySqrtFSCFilter(fsc_info.second);
			rec3d_1.writeToFile("reconstruction_1_" + boost::lexical_cast<std::string>(i) + ".map");
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction_1_" + boost::lexical_cast<std::string>(i) + ".map", "MAP: Refined Model_1" + boost::lexical_cast<std::string>(i), config->getScriptName());
		}
		
		if (world.rank() == 1)
		{
			rec3d_2.applySqrtFSCFilter(fsc_info.second);
			rec3d_2.writeToFile("reconstruction_2_" + boost::lexical_cast<std::string>(i) + ".map");
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction_2_" + boost::lexical_cast<std::string>(i) + ".map", "MAP: Refined Model_2" + boost::lexical_cast<std::string>(i), config->getScriptName());
		}
	
		//if (rec3d_1.getLastAngleChange() == 0.0)
		//{
		//	SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD", 1);
		//	break;
		//}	
	}

	
	SingleParticle2dx::DataStructures::Orientation o_old;
	SingleParticle2dx::DataStructures::Orientation o_new;
	SingleParticle2dx::DataStructures::ParticleShift shift;
	bool use_for_rec;
	SingleParticle2dx::value_type sim_measure;
	SingleParticle2dx::value_type qual;
	SingleParticle2dx::value_type weight;
	SingleParticle2dx::value_type consistency;
	
	
	if (world.rank() == 0)
	{
		for(int i=0; i<cont2.getNumberOfParticles(); i++)
		{
			std::cout << i << std::endl;
			
			boost::mpi::request reqs[8];
			reqs[0] = world.irecv(1, 0, o_old);
			reqs[1] = world.irecv(1, 1, o_new);
			reqs[2] = world.irecv(1, 2, shift);
			reqs[3] = world.irecv(1, 3, use_for_rec);
			reqs[4] = world.irecv(1, 4, sim_measure);
			reqs[5] = world.irecv(1, 5, qual);
			reqs[6] = world.irecv(1, 6, weight);
			reqs[7] = world.irecv(1, 7, consistency);
			boost::mpi::wait_all(reqs, reqs+8);
			
			cont2(i).setOldOrientation(o_old);
   		 	cont2(i).setNewOrientation(o_new);
   		 	cont2(i).setParticleShift(shift);
   		 	cont2(i).setUseForReconstruction(use_for_rec);
   		 	cont2(i).setSimMeasure(sim_measure);
   		 	cont2(i).setQuality(qual);
   		 	cont2(i).setWeight(weight);
   		 	cont2(i).setConsistency(consistency);
		}
	}
	
	if (world.rank() == 1)
	{
		for(int i=0; i<cont2.getNumberOfParticles(); i++)
		{
			o_old = cont2(i).getOldOrientation();
			o_new = cont2(i).getNewOrientation();
			shift = cont2(i).getParticleShift();
			use_for_rec = cont2(i).getUseForReconstruction();
			sim_measure = cont2(i).getSimMeasure();
			qual = cont2(i).getQuality();
			weight = cont2(i).getWeight();
			consistency = cont2(i).getConsistency();
			
   		 	boost::mpi::request reqs[8];
   		 	reqs[0] = world.isend(0, 0, o_old);
   		 	reqs[1] = world.isend(0, 1, o_new);
   		 	reqs[2] = world.isend(0, 2, shift);
   		 	reqs[3] = world.isend(0, 3, use_for_rec);
			reqs[4] = world.isend(0, 4, sim_measure);
   		 	reqs[5] = world.isend(0, 5, qual);
   		 	reqs[6] = world.isend(0, 6, weight);
   		 	reqs[7] = world.isend(0, 7, consistency);
   		 	boost::mpi::wait_all(reqs, reqs+8);
		}
	}
	
	if (world.rank() == 0)
	{
		SingleParticle2dx::DataStructures::ParticleContainer::mergeContainers(cont1, cont2, cont);

		rec3d_full.generateInitialModel(cont);
		rec3d_full.applyFinalFSCFilter(fsc_info.second);
		rec3d_full.applyFinalMask();
		rec3d_full.writeToFile( "reconstruction.map" );

		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("reconstruction.map", "MAP: Final Reconstruction", config->getScriptName(), true);

		cont.writeStatToFile( "container_stat.txt" );
	}
	
	
	return 0;
}
